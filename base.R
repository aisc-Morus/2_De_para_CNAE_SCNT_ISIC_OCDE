
# 1. Carrega ambiente ----
if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, stringi, readxl, writexl, purrr, sidrar)

# 2. Processamento ----
## 2.1 CNAE (2.3) ----
df_cnae_v <- read_excel("input/CNAE_2.3_compilado_hierarquia.xlsx") %>%               # estrutura hierarquizada
             rename_with(tolower) %>% 
             mutate(cnae = str_remove_all(cnae, "[^0-9]"),
                    cnae_descr = str_to_sentence(descr_cnae)) %>%  
             select(cnae, cnae_descr)

df_cnae_h <- read_excel("input/CNAE_2.3_compilado.xlsx")                                # estrutura NÃO hierarquizada

## 2.2 CNAE (2.0) x SCNT ----   
df_cnae_scnt <- read_excel("input/CNAE_2.0_SCNT_atividades_divulg.xlsx", skip = 2) %>%
                rename_with(~ stri_trans_general(.x, "Latin-ASCII")) %>%        # remove acentos
                rename_with(tolower) %>%                                        # minúsculas
                rename_with(~ gsub("[^a-z0-9]+", "_", .x)) %>%                  # caracteres especiais → "_"
                rename_with(~ gsub("^_|_$", "", .x)) %>% 
                head(-2) %>% 
                rename(cnae            = resumo_cnae_2_0_ativ_divisao_2dig,
                       scnt_ativ       = ativ_divulg,
                       scnt_ativ_descr = descr_ativ_divulgacao) %>%
                mutate(cnae = str_remove_all(cnae,"\\*")) %>%
                separate_rows(cnae, sep = "\\s*\\+\\s*") %>%                    # separa por "+" e espacos
                mutate(cnae = str_trim(cnae),                                   # remove espaços em brancos da string
                       scnt_ativ_obs  = if_else(cnae %in% c("85", "86", "87"), 
                                                "Cnae está decomposta em mais de uma atividade do SCN segundo a unidade produtiva pública/privada", ""),
                       cnae_divisao   = if_else(nchar(cnae) == 2, substr(cnae, 1, 2), NA_character_),
                       cnae_grupo     = if_else(nchar(cnae) == 3, substr(cnae, 1, 3), NA_character_),
                       cnae_classe    = if_else(nchar(cnae) == 4, substr(cnae, 1, 4), NA_character_)) %>% 
                select(-secao_isic_rev_4_1) %>% 
                relocate(starts_with("cnae"), .before = scnt_ativ)

## 2.3 CNAE (2.0) x ISIC (4.0) ----
df_cnae_isic <- read_excel("input/CNAE_2.0_isic4.xls", skip = 53) %>%
                select(-4) %>% 
                setNames(c("isic", 
                            "isic_descr", 
                            "cnae", 
                            "cnae_descr", 
                            "isic_obs")) %>% 
                mutate(cnae           = str_remove_all(cnae, "[^0-9]"),                                # remove qualquer caractere que não seja número
                       cnae_divisao   = if_else(nchar(cnae) >= 2, substr(cnae, 1, 2), NA_character_),
                       cnae_grupo     = if_else(nchar(cnae) >= 3, substr(cnae, 1, 3), NA_character_),
                       cnae_classe    = if_else(nchar(cnae) >= 4, substr(cnae, 1, 4), NA_character_),
                       cnae_descr     = str_to_sentence(cnae_descr),
                       isic_descr     = str_to_sentence(isic_descr),
                        isic_divisao   = if_else(nchar(isic) >= 2, substr(isic, 1, 2), NA_character_),
                        isic_grupo     = if_else(nchar(isic) >= 3, substr(isic, 1, 3), NA_character_),
                        isic_classe    = if_else(nchar(isic) >= 4, substr(isic, 1, 4), NA_character_)) %>%
                 mutate(across(everything(), ~na_if(., ""))) %>%
                 filter(!str_detect(isic, "^[A-Z]$")) %>% 
                 select(cnae, cnae_divisao, cnae_grupo, cnae_classe, cnae_descr, isic, isic_divisao, isic_grupo, isic_classe, isic_descr, isic_obs)

## 2.4 NCM x ISIC ----
df_isic_ncm <- read_csv2("input/NCM.csv", 
                         locale = locale(encoding = "latin1")) %>%                   # usar latin1 ou UTF-8 para encode de strings em português
               as_tibble() %>% 
               rename_with(tolower) %>%
               select(ncm = co_ncm,
                      ncm_descr = no_ncm_por,
                      isic = co_isic_classe)

## 2.5 CNAE (2.0) x Intensidade Tecnológica OCDE ----
df_cnae_isic_ocde <- read_xlsx("input/CNAE_2.0_OCDE_intensi_PD.xlsx", 
                               col_types = c("text", "text")) %>%  
                     left_join(df_cnae_isic %>% select(cnae, isic), by = "isic") %>% 
                     add_count(isic, name = "n_cnae_por_isic") %>% 
                     add_count(cnae, name = "n_isic_por_cnae") %>% 
                     mutate(cardinalidade = case_when(n_cnae_por_isic == 1 & n_isic_por_cnae == 1 ~ "1:1",
                                                      n_cnae_por_isic > 1  & n_isic_por_cnae == 1 ~ "1:n",
                                                      n_cnae_por_isic == 1 & n_isic_por_cnae > 1 ~ "n:1",
                                                      TRUE ~ "n:n")) %>% 
                     mutate(status = if_else(cardinalidade == "1:1", 
                                             "típica", 
                                             "não típica")) %>% 
                     arrange(cnae) 

df_cnae_ocde <- df_cnae_h %>% 
                filter(cnae_secao == "C") %>%  
                select(cnae_divisao, cnae_grupo, cnae_grupo_descr) %>% 
                distinct() %>% 
                left_join(df_cnae_isic_ocde %>% select(cnae, regra_tipica_grupo = ocde_class),
                          join_by(cnae_grupo == cnae)) %>% 
                left_join(df_cnae_isic_ocde %>% filter(status == "típica") %>%  select(cnae, regra_tipica_divisao = ocde_class),
                          join_by(cnae_divisao == cnae)) %>% 
                mutate(regra_atipica = case_when(cnae_grupo %in% c(151,152,153) ~ "Média Baixa",
                                                 cnae_grupo == 154 ~ "Média",
                                                 cnae_divisao == "16" ~ "Média Baixa",
                                                 cnae_grupo %in% c(191,192) ~ "Média Baixa",
                                                 cnae_grupo == 193 ~ "Média Alta",
                                                 cnae_divisao == 20 ~ "Média Alta",
                                                 cnae_divisao == 22 ~ "Média",
                                                 TRUE ~ NA)) %>%
                mutate(ocde_class = coalesce(regra_tipica_grupo,
                                             regra_tipica_divisao,
                                             regra_atipica)) %>%  
                select(cnae_divisao, 
                       cnae_grupo,
                       cnae_grupo_descr,
                       regra_tipica_grupo, 
                       regra_tipica_divisao, 
                       regra_atipica,
                       ocde_class)

    
## 2.6 CNAE (2.3) x Missões NIB ----
df_cnae_nib <- read_xlsx("input/CNAE_2.3_NIB_HS.xlsx", trim_ws = TRUE)

## 2.7 CNAE (2.3) - Alias IED ----
df_cnae_alias_ied <- df_cnae_h %>% 
                     filter(cnae_secao == "C") %>%  
                     select(cnae_divisao, cnae_grupo, cnae_grupo_descr) %>% 
                     distinct() %>% 
                     mutate(alias_1 = case_when(cnae_divisao %in% c("21")                     ~ "Indústria farmacêutica",
                                                cnae_divisao %in% c("26")                     ~ "Complexo eletrônico",
                                                cnae_grupo %in% c("304")                      ~ "Fabricação de aeronaves",
                                                
                                                cnae_divisao %in% c("29")                     ~ "Fab. veícs. automotores, reboqs. e carrocerias",
                                                cnae_grupo %in% c("325")                      ~ "Fab. I&M uso médico e odontolog., arts. óticos",
                                                cnae_divisao %in% c("28")                     ~ "Fab. M&E",
                                                cnae_divisao %in% c("20")                     ~ "Fab. de químicos (exc. farmacêuticos)",
                                                cnae_grupo %in% c("304")                      ~ "Fab. de químicos (exc. farmacêuticos)",
                                                cnae_divisao %in% c("27")                     ~ "Fab. máqs., apars. e maters. elétricos",
                                                cnae_grupo %in% c("255")                      ~ "Fab. equip. bélicos pesados, armas e munição",
                                                cnae_grupo %in% c("303", "305", "309")        ~ "Fab. de veícs. ferrov., militares e não espec."
                                                
# parei na Média!                                                
                                                
                                                ),
                            alias_2 = case_when(cnae_grupo %in% c("262", "263")               ~ "Material de escritório e informática",
                                                cnae_grupo %in% c("264")                      ~ "Equipamento de rádio, tv e comunicação",
                                                cnae_grupo %in% c("265", "266", "267", "268") ~ "Instrumentos médicos, de ótica e precisão"))


# 3. De_para ----
## 3.1 V ----
depara_v <- df_cnae_v %>% 
            left_join(df_cnae_scnt %>% select(cnae, starts_with("scnt")),
                      by = "cnae") %>% 
            left_join(df_cnae_isic %>% select(cnae, starts_with("isic")),
                      by = "cnae",
                      relationship = "many-to-many") %>%
            left_join(df_cnae_ocde %>% select(cnae_grupo, ocde_class), 
                      join_by(cnae == cnae_grupo)) 
            
            
## 3.2 H ----
depara_h <- df_cnae_h

# 4. xlsx ----
write_xlsx(depara_v, "output/depara_v_cnae_sctn_isic_ocde.xlsx")
write_xlsx(depara_h, "output/depara_h_cnae_ocde.xlsx")
write_xlsx(df_cnae_ocde, "output/regra_h_cnae_ocde.xlsx")
