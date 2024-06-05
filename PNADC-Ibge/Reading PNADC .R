# Dependências
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
if (!requireNamespace("survey", quietly = TRUE)) install.packages("survey")
if (!requireNamespace("PNADcIBGE", quietly = TRUE)) install.packages("PNADcIBGE")
if (!requireNamespace("srvyr", quietly = TRUE)) install.packages("srvyr")
if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")


# Pacotes
library(PNADcIBGE)
library(survey)
library(tidyverse)
library(srvyr)
library(magrittr)
library(openxlsx)
gc()

# Função
source("./Setores.R")

#Variáveis
variaveis = c("VD4002",  # Pessoal ocupado
              "VD4001",
              "V4046",   # Carteira
              "V4019",   # CNPJ
              "V4013" ,  # CNAE
              "VD4008",  # Posição na ocupação - menos detalhadada
              "VD4009",  # Posição na ocupação - mais detalhadada
              "VD3004",  # Nível de instrução
              "V2007" ,  # Gênero
              "V2010" ,  # Raça ou cor
              "VD4016",  # Rendimento
              "V403311") # grupo de rendimento
 

# Arquivos da PNAD
painel_pnadc_1 <- get_pnadc(year = 2023, quarter = 1, vars = variaveis, design = F, labels = T)
painel_pnadc_2 <- get_pnadc(year = 2023, quarter = 2, vars = variaveis, design = F, labels = T)
painel_pnadc_3 <- get_pnadc(year = 2023, quarter = 3, vars = variaveis, design = F, labels = T)
painel_pnadc_4 <- get_pnadc(year = 2023, quarter = 4, vars = variaveis, design = F, labels = T)

#   
df_pnad_2023 <- bind_rows(painel_pnadc_1,
                          painel_pnadc_2,
                          painel_pnadc_3,
                          painel_pnadc_4)

  
# Classificar CNAE
pre_painel_pnadc <- df_pnad_2023 %>%
  #subset(UF == "Piauí") %>%
  rowwise() %>%
  mutate(V4013 = as.character(V4013), segmento = classificar_cnae(V4013)) %>%
  select(1:30, segmento)


assign(str_glue('pre_painel_pnadc'), # Base original
       
       mutate(get(str_glue('pre_painel_pnadc')), # Base alterada
              one = 1, # Numerador, para projeção de toda a base
              
              'CO' = case_when(
                VD4009 %in% c("Empregado no setor privado com carteira de trabalho assinada", 
                              "Trabalhador doméstico com carteira de trabalho assinada", 
                              "Empregado no setor público com carteira de trabalho assinada") & V4046 == "Sim" ~ "Com carteira",
                VD4009 %in% c("Empregado no setor privado sem carteira de trabalho assinada", 
                              "Trabalhador doméstico sem carteira de trabalho assinada", 
                              "Trabalhador familiar auxiliar") & V4046 == "Não" ~ "Sem carteira",
                VD4009 %in% c("Empregador", "Conta-própria") & V4019 == "Sim" ~ "Com CNPJ",
                VD4009 %in% c("Empregador", "Conta-própria") & V4046 == "Não" ~ "Sem CNPJ",
                TRUE ~ "Não se aplica"), # Lidar com casos não especificados
              
                     
              'IC' = case_when(
                VD3004 %in% c("Sem instrução e menos de 1 ano de estudo") ~ "Sem instrução",
                VD3004 %in% c("Fundamental incompleto ou equivalente",
                              "Fundamental completo ou equivalente") ~ "Fundamental",
                VD3004 %in% c("Médio incompleto ou equivalente", 
                              "Médio completo ou equivalente") ~ "Médio",
                VD3004 %in% c("Superior incompleto ou equivalente", 
                              "Superior completo") ~ "Superior",
                TRUE ~ "Não se aplica") # Lidar com casos não especificados
       )
)

# Painéis
painel_pnadc <- pre_painel_pnadc %>%
  as_survey_design(ids = UPA, 
                   strata = Estrato, 
                   weights = V1028, 
                   nest = TRUE) 


#### Processar tabelas ####
  # Tabela 1
  for(i in 1:1){
#    assign(str_glue('tab_pnadc_{1}'), # ! Base original 
#           mutate(get(str_glue('painel_pnadc')) %>%
#                    filter(VD4002 == "Pessoas ocupadas") %>%
#                    group_by(Ano, Trimestre, VD4009) %>%
#                    summarise(
#                      Agrícola = survey_total(segmento != "Outro", na.rm = TRUE, vartype = NULL),
#                      Nao_Agrícola = survey_total(segmento == "Outro", na.rm = TRUE, vartype = NULL)
#                  ) %>% mutate(regiao = 'Brasil')
#           ))

    
#    assign(str_glue('tab_pnadc_{2}'), # ! Base original 
#           mutate(get(str_glue('painel_pnadc')) %>%
#                    filter(VD4002 == "Pessoas ocupadas" & segmento != "Outro") %>%
#                    group_by(Ano, Trimestre, segmento) %>%
#                    summarise(
#                      Pessoas_ocupadas = survey_total(VD4002 == "Pessoas ocupadas", na.rm = TRUE, vartype = NULL)
#                    ) %>% mutate(regiao = 'Brasil')
#           ))

    
    assign(str_glue('tab_pnadc_{3}'), # ! Base original 
           mutate(get(str_glue('painel_pnadc')) %>%
                    filter(VD4002 == "Pessoas ocupadas") %>%
                    group_by(Ano, Trimestre, VD4009) %>%
                    summarise(
                      Insumos   = survey_total(segmento == "Insumos", na.rm = TRUE, vartype = NULL),
                      Primario  = survey_total(segmento == "Primário", na.rm = TRUE, vartype = NULL),
                      Industria = survey_total(segmento == "Agroindústria", na.rm = TRUE, vartype = NULL), 
                      Servicos  = survey_total(segmento == "Agrosserviços", na.rm = TRUE, vartype = NULL)
                    ) %>% mutate(regiao = 'Brasil')
           ))

    
    assign(str_glue('tab_pnadc_{4}'), # ! Base original 
           mutate(get(str_glue('painel_pnadc')) %>%
                    filter(VD4002 == "Pessoas ocupadas") %>%
                    group_by(Ano, Trimestre, VD3004) %>%
                    summarise(
                      Insumos   = survey_total(segmento == "Insumos", na.rm = TRUE, vartype = NULL),
                      Primario  = survey_total(segmento == "Primário", na.rm = TRUE, vartype = NULL),
                      Industria = survey_total(segmento == "Agroindústria", na.rm = TRUE, vartype = NULL), 
                      Servicos  = survey_total(segmento == "Agrosserviços", na.rm = TRUE, vartype = NULL)
                    ) %>% mutate(regiao = 'Brasil')
           ))
    gc()
    
    
    assign(str_glue('tab_pnadc_{5}'), # ! Base original 
           mutate(get(str_glue('painel_pnadc')) %>%
                    filter(VD4002 == "Pessoas ocupadas") %>%
                    group_by(Ano, Trimestre, V2007) %>%
                    summarise(
                      Insumos   = survey_total(segmento == "Insumos", na.rm = TRUE, vartype = NULL),
                      Primario  = survey_total(segmento == "Primário", na.rm = TRUE, vartype = NULL),
                      Industria = survey_total(segmento == "Agroindústria", na.rm = TRUE, vartype = NULL), 
                      Servicos  = survey_total(segmento == "Agrosserviços", na.rm = TRUE, vartype = NULL)
                    ) %>% mutate(regiao = 'Brasil')
           ))
    gc()
    
    
    assign(str_glue('tab_pnadc_{6}'), # ! Base original 
           mutate(get(str_glue('painel_pnadc')) %>%
                    filter(VD4002 == "Pessoas ocupadas") %>%
                    group_by(Ano, Trimestre, V2010) %>%
                    summarise(
                      Insumos   = survey_total(segmento == "Insumos", na.rm = TRUE, vartype = NULL),
                      Primario  = survey_total(segmento == "Primário", na.rm = TRUE, vartype = NULL),
                      Industria = survey_total(segmento == "Agroindústria", na.rm = TRUE, vartype = NULL), 
                      Servicos  = survey_total(segmento == "Agrosserviços", na.rm = TRUE, vartype = NULL)
                    ) %>% mutate(regiao = 'Brasil')
           ))
    gc()
    
    
#    assign(str_glue('tab_pnadc_{7}'), # ! Base original 
#           mutate(get(str_glue('painel_pnadc')) %>%
    #                    filter(VD4002 == "Pessoas ocupadas") %>%
    #                    group_by(Ano, segmento, VD4008) %>%
    #                   summarise(
    #                     renda_media = survey_mean(VD4016, na.rm = TRUE, vartype = NULL)
    #                  ) %>% 
    #                 #-- ajuste das variáveis
    #                    pivot_wider(names_from = VD4008, values_from = renda_media) %>% 
    #                mutate(regiao = 'Brasil')
    #           ))
    #    gc()
    
    
    #   assign(str_glue('tab_pnadc_{8}'), # ! Base original 
    #          mutate(get(str_glue('painel_pnadc')) %>%
    #                   filter(VD4002 == "Pessoas ocupadas") %>%
 #                   group_by(Ano, segmento, IC) %>%
    #                  summarise(
    #                    renda_media = survey_mean(VD4016, na.rm = TRUE, vartype = NULL)
    #                  ) %>% 
                    #-- ajuste das variáveis
    #                 pivot_wider(names_from = IC, values_from = renda_media) %>% 
    #                  mutate(regiao = 'Brasil') 
    #        ))
    # gc()
  }

  # Tabela 2
  for(i in 1:1){
    assign(str_glue('tab_pnadc_{i}t_pi'), # ! Base original 
           
           mutate(get(str_glue('painel_pnadc_{i}t')) %>%
                    filter(UF == "22", VD4002 == "1") %>%
                    group_by(Ano, Trimestre, VD4009) %>%
                    summarise(
                      Agrícola = survey_total(segmento != "Outro", na.rm = TRUE, vartype = "se"),
                      Nao_Agrícola = survey_total(segmento == "Outro", na.rm = TRUE, vartype = "se")
                    ) %>% mutate(regiao = 'Piauí')
           ))
    system.time(Sys.sleep(3))
    gc()
  }

  # Tabela 3
  for(i in 1:1){
    assign(str_glue('tab_pnadc_{i}t_pi'), # ! Base original 
           
           mutate(get(str_glue('painel_pnadc_{i}t')) %>%
                    filter(UF == "22", VD4002 == "1") %>%
                    group_by(Ano, Trimestre, segmento) %>%
                    summarise(
                      Agrícola = survey_total(segmento != "Outro", na.rm = TRUE, vartype = "se"),
                      Nao_Agrícola = survey_total(segmento == "Outro", na.rm = TRUE, vartype = "se")
                    ) %>% mutate(regiao = 'Piauí')
           ))
    system.time(Sys.sleep(3))
    gc()
  }


###### Salvando em pasta do excel  ########
{
lista_dfs <- list( 
                  df3 = tab_pnadc_3, 
                  df4 = tab_pnadc_4, 
                  df5 = tab_pnadc_5, 
                  df6 = tab_pnadc_6)


wb <- createWorkbook()
lapply(names(lista_dfs), function(nome_df) {
  addWorksheet(wb, nome_df)
  writeData(wb, nome_df, lista_dfs[[nome_df]])
})

saveWorkbook(wb, file = "C:/Users/alexa/OneDrive/Documentos/Pesquisa - Kauane/tabela_multiplos_dfs_br.xlsx", 
             overwrite = TRUE)
}
