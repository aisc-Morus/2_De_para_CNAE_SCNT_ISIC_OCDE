
# 1. Carrega ambiente ----
if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, stringi, readxl, writexl, purrr)

# 2. Carrega estrutura CNAE (2.3) ----
df_cnae_h <- read_excel("CNAE_2.3_compilado_hierarquia.xlsx") %>%
             rename_with(tolower) %>% 
             mutate(cnae = str_remove_all(cnae, "[^0-9]"),
                    cnae_descr = str_to_sentence(descr_cnae)) %>%  
             select(cnae, cnae_descr)

df_cnae <- read_excel("CNAE_2.3_compilado_hierarquia.xlsx") 

# 3. Carrega estrutura CNAE (2.0) x SCNT ----   
df_cnae_scnt <- read_excel("Atividade -contas de divulgação x Cnae 2.0 - Resumo.xlsx", skip = 2) %>%
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

df_cnae_scnt %>% group_by(cnae) %>% summarise(scnt_distinct = n_distinct(scnt_ativ)) %>% arrange(desc(scnt_distinct))

# 4. Carrega estrutura CNAE (2.0) x ISIC/CIIU (4.0) ----
df_cnae_isic <- read_excel("CNAE20_Correspondencia_Isic4xCnae20.xls", skip = 53) %>%
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
                        cnae_subclasse = if_else(nchar(cnae) >= 5, substr(cnae, 1, 5), NA_character_),
                        cnae_descr     = str_to_sentence(cnae_descr),
                        isic_descr     = str_to_sentence(isic_descr),
                        isic_versao = "4.0") %>% 
                 select(starts_with("cnae"), everything()) 

df_cnae_isic %>% group_by(isic) %>% summarise(n_cnaes_distinct = n_distinct(cnae)) %>% arrange(desc(n_cnaes_distinct)) %>% head(10)

# 5. Carrega estrutura CNAE (2.0) x Intensidade Tecnológica OCDE ----
df_cnae_ocde_intensi_tec <- read_xlsx("CNAE_2.0_Correspondencia_OCDE_intensidade_tec.xlsx") %>% 
                            rename(cnae = codigo_cnae20,
                                   cnae_descr = divisao)

# 6. Monta de/para com raíz nas CNAES ----
df <- df_cnae_h %>% 
      left_join(df_cnae_scnt %>% select(cnae, starts_with("scnt")),
                by = "cnae") %>% 
      left_join(df_cnae_isic %>% select(cnae, starts_with("isic")),
                by = "cnae",
                relationship = "many-to-many")

# 7. Grava xlsx da tabela de correspondencia ----
write_xlsx(df_de_para, "depara_cnae_sctn_isic.xlsx")

# 8. Extra ----
########################## CNAE NÃO hierarquizada! 
df_depara <- df_cnae %>% 
  left_join(df_cnae_scnt %>% select(cnae_divisao, scnt_ativ, scnt_ativ_descr, scnt_ativ_obs),
            by = "cnae_divisao",
            relationship = "many-to-many") %>% 
  left_join(df_cnae_scnt %>% select(cnae_grupo, scnt_ativ, scnt_ativ_descr, scnt_ativ_obs),
            by = "cnae_grupo",
            relationship = "many-to-many") %>%
  mutate(cnae_classe_4 = substr(cnae_classe, 1, 4)) %>% 
  left_join(df_cnae_scnt %>% select(cnae_classe, scnt_ativ, scnt_ativ_descr, scnt_ativ_obs),
            join_by(cnae_classe_4 == cnae_classe),
            relationship = "many-to-many") %>%
  select(-cnae_classe_4) %>% 
  mutate(scnt_ativ       = coalesce(scnt_ativ,
                                    scnt_ativ.x,
                                    scnt_ativ.y),
         scnt_ativ_descr = coalesce(scnt_ativ_descr,
                                    scnt_ativ_descr.x,
                                    scnt_ativ_descr.y),
         scnt_ativ_obs   = coalesce(scnt_ativ_obs,
                                    scnt_ativ_obs.x,
                                    scnt_ativ_obs.y)) %>%
  left_join(df_cnae_isic %>% select(cnae_divisao, isic, isic_descr, isic_obs),
            by = "cnae_divisao",
            relationship = "many-to-many") %>% 
  left_join(df_cnae_isic %>% select(cnae_grupo, isic, isic_descr, isic_obs),
            by = "cnae_grupo",
            relationship = "many-to-many") %>% 
  left_join(df_cnae_isic %>% select(cnae_classe, isic, isic_descr, isic_obs),
            by = "cnae_classe",
            relationship = "many-to-many") %>% 
  left_join(df_cnae_isic %>% select(cnae_subclasse, isic, isic_descr, isic_obs),
            by = "cnae_subclasse",
            relationship = "many-to-many") %>% 
  mutate(isic       = coalesce(isic.x,
                               isic.y,
                               isic.x.x,
                               isic.y.y),
         isic_descr = coalesce(isic_descr.x,
                               isic_descr.y,
                               isic_descr.x.x,
                               isic_descr.y.y) ,
         isic_obs   = coalesce(isic_obs.x,
                               isic_obs.y,
                               isic_obs.x.x,
                               isic_obs.y.y)) %>% 
  select(-ends_with(c("x","y")))

########################## Intensidade OCDE x CNAE
df_intensidade <- tribble( ~intensidade,  ~codigo_cnae20, ~divisao,
                           "Alta",         21,             "Farmoquímicos e Farmacêuticos",
                           "Alta",         26,             "Equip. de Informát, Prod. Eletrônicos e Ópticos",
                           "Alta",         28,             "Máquinas e Equipamentos",
                           "Média Alta",   27,             "Máquinas, Aparelhos e Materiais Elétricos",
                           "Média Alta",   20,             "Químico",
                           "Média Alta",   29,             "Veículos Automotores, Reboques e Carrocerias",
                           "Média Alta",   30,             "Outros equipamentos de Transporte", 
                           "Média Baixa",  25,             "Produtos de Metal, Exceto Máquinas e Equip.",
                           "Média Baixa",  19,             "Coque, Derivados do petróleo e Biocombustíveis",
                           "Média Baixa",  22,             "Produtos de Borracha e Material Plástico",
                           "Média Baixa",  23,             "Produtos de Minerais Não-Metálicos",
                           "Média Baixa",  24,             "Metalurgia",
                           "Baixa",        18,             "Impressão e Reprod. de Gravações",
                           "Baixa",        33,             "Manut., Rep. e Instal. de Máquinas e Equip.",
                           "Baixa",        11,             "Bebidas",
                           "Baixa",        12,             "Fumo",
                           "Baixa",        13,             "Têxteis",
                           "Baixa",        14,             "Vestuário e Acessórios",
                           "Baixa",        15,             "Couros e Artefatos, Art Viagem e Calçados",
                           "Baixa",        16,             "Produtos de Madeira",
                           "Baixa",        17,             "Celulose, Papel e Prod. de Papel",
                           "Baixa",        10,             "Alimentícios",
                           "Baixa",        31,             "Móveis",
                           "Baixa",        32,             "Produtos diversos")

