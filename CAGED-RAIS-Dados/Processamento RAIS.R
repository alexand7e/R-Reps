#_______________________________________________________________________________

# Processamento e Análise dos dados da RAIS 21

# Autor: Alexandre Barros 
# Email: alexandre.barros@ufpi.edu.br

# --/12/2022
#_______________________________________________________________________________

#> Dependências ----------------------------------------------------------------

install.packages(c('openxlsx', 'sf', 'bigrquery'))
remotes::install_github("georgevbsantiago/qsacnpj")

library(openxlsx)
library(tidyverse)
library(sf)
library(magrittr)
library(bigrquery)


#> discrimianr setores/subsetores ----------------------------------------------
CNAE <- select(qsacnpj::tab_cnae, 2,8,9)

# ID do projeto do Google Cloud
projectid = "composite-cable-369715"


# Query de solicitação da base de dados
sql <- "SELECT ano, sigla_uf, id_municipio, cnae_2, vinculo_ativo_3112, cnae_2_subclasse
        FROM `basedosdados.br_me_rais.microdados_vinculos`
        WHERE ano BETWEEN 2017 AND 2021
        AND sigla_uf = 'PI'" # usar 'BETWEEN ~. AND' para inserir intervalos
# executando o query, no Id do projeto

tb <- bq_project_query(projectid, sql)


# lendo/importando a tabela
dados_rais <- bq_table_download(tb)

#> ANÁLISE DOS DADOS 

dados_rais_2 <- dados_rais %>% 
  group_by(ano, cnae_2_subclasse, id_municipio) %>%
  summarise(Vinculos = n())

# adicionando setores 
dados_rais_2 <- dados_rais_2 %>% left_join(CNAE, by = c('cnae_2_subclasse'='cod_cnae')) %>% 
  mutate(Seção = case_when(
  nm_secao %in% c("INDÚSTRIAS EXTRATIVAS") ~ "Extrativa mineral",
  nm_secao %in% c("INDÚSTRIAS DE TRANSFORMAÇÃO") ~ "Indústria de transformação",
  nm_secao %in% c("ÁGUA ESGOTO ATIVIDADES DE GESTÃO DE RESÍDUOS E DESCONTAMINAÇÃO") ~ "Serviços de utilidade pública",
  nm_secao %in% c("CONSTRUÇÃO") ~ "Construção civil",
  nm_secao %in% c("COMÉRCIO REPARAÇÃO DE VEÍCULOS AUTOMOTORES E MOTOCICLETAS") ~ "Comércio",
  nm_secao %in% c("ADMINISTRAÇÃO PÚBLICA DEFESA E SEGURIDADE SOCIAL") ~ "Administração pública",
  nm_secao %in% c("AGRICULTURA PECUÁRIA PRODUÇÃO FLORESTAL PESCA E AQUICULTURA") ~ "Agropecuária",
  T ~ "Serviços"
))  %>% na.omit() %>% mutate(Setor = ifelse(Seção == 'Administração pública', # Setor de Análise
                                                     'S_Referencia',
                                                     'Demais setores'))

#> CONSOLIDANDO
#> 

df_emprego_formal <- dados_rais_2 %>%
  group_by(ano, Setor, id_municipio) %>%
  summarise(Vínculos = sum(Vinculos)) %>%
  pivot_wider(names_from = 'Setor', 
              values_from = 'Vínculos', 
              values_fill = 0) %>%
  mutate(Total = `S_Referencia` + `Demais setores`)


df_emprego_formal2 <- dados_rais_2 %>%
  mutate(Regiao = ifelse(id_municipio == '2209450',
                        'Parnaíba',
                        'Outras cidades')) %>%
  group_by(ano, Seção, Regiao) %>%
  summarise(Vínculos = sum(Vinculos)) %>%
  pivot_wider(names_from = 'Regiao', 
              values_from = 'Vínculos', 
              values_fill = 0) %>% 
  mutate(Total = `Parnaíba` + `Outras cidades`)


#> SALVANDO BASES --------------------------------------------------------------
#> 

# adicionando os municipios
dados_rais_2 <- dados_rais_2 %>% left_join(piaui, by = c('id_municipio'='CD_MUN'))

#salvando a base

dataset_names <- list('CL' = df_rais.CL, 
                      'CR' = df_rais.CR, 
                      'CE' = df_rais.CE,
                      'QL' = df_rais.QL,
                      'IHHM' = df_rais.IHHm)


# exportanto para excel
openxlsx::write.xlsx(dataset_names, file = 'indicadores2.xlsx') 

# ! INDICADORES !

#> Quociente Locacional
#> chamar os dados para o QL, E<-sum(ei) # ei é o emprego total da regiao i, 
#> E é o emprego total no Estado Ek<-sum(eki) # Ek é o emprego total do setor k

QL <- function(e_ki, e_i, e_k, e) {
  
  s_ki <- e_ki/e_i
  s_i <- e_k/e
  LQ <- s_ki/s_i
  
  return(LQ)
}

#> Índice de Concentração Hirschman-Herfindahl Modificado

IHHm <- function(e_ki, e_i, e_k, e) {
  
  s_ki <- e_ki/e_i
  s_i <- e_k/e
  IHHm <- (s_ki - s_i)
  
  return(round(IHHm,4))
}

#> Coeficiente de Localização
#> 

CL = function(e_ki, e_i, e_k, e) {
  
  s_ki <- e_ki/e_k
  s_i <- e_i/e
  CLh <- sum(abs(s_i - s_ki)) * .5 
  
  return(round(CLh,4))
}

#> O Coeficiente de Redistribuição
#> 

CR <- function(e_ki0, e_ki1, e_k0, e_k1) {
  
  s_ki0 <- e_ki0/e_k0
  s_ki1 <- e_ki1/e_k1
  CR <- (.5 * abs(s_ki1 - s_ki0))
  
  return(round(CR,4))
}


#> Coeficiente de Especialização
#> 

CE <- function(e_ki, e_i, e_k, e) {
  
  s_ki <- e_ki/e_i
  s_i <- e_k/e
  CEH <- sum(.5 * abs(s_ki - s_i))
  
  return(round(CEH,4))
  
}


# QUOCIENTE LOCACIONAL -------------------------------------------------------------------

i=1
repeat{
  
assign(str_glue('rais_QL_{2016+i}'),
  subset(df_emprego_formal, ano == 2016+i) %>%
  mutate(
  df_e_ki = `S_Referencia`,
  df_e_i = Total,
  df_E = sum(Total),
  df_e_k = sum(`S_Referencia`),
  # PRINCIPAL
  QL = QL(df_e_ki, df_e_i, df_e_k, df_E)
) %>% select(ano, id_municipio, QL) %>% pivot_wider(names_from = ano, values_from = QL))

i = i + 1  
  
if(i == 6){
  df_rais.QL = left_join(rais_QL_2017, rais_QL_2018, by = "id_municipio") %>% 
            left_join(rais_QL_2019, by = "id_municipio") %>% 
            left_join(rais_QL_2020, by = "id_municipio") %>% 
            left_join(rais_QL_2021, by = "id_municipio")
  rm(list=ls(pattern = 'rais_QL_'))
  break()
}
}

# COEFICIENTE LOCACIONAL -------------------------------------------------------------------


i=1
repeat{
   
  assign(str_glue('rais_CL_{2016+i}'),
         subset(df_emprego_formal2, ano == 2016+i) %>%
           mutate(
             df_e_ki = `Parnaíba`,
             df_e_i = apply(.[4], 2, sum ),
             df_E = apply(.[3], 2, sum ),
             df_e_k = `Total`,
             # PRINCIPAL
             CL = .5 * abs(df_e_ki/df_e_i - df_e_k/df_E)
           ) %>% select(ano, Seção, CL)  %>% pivot_wider(names_from = ano, values_from = CL))
  
  i = i + 1  
  
  if(i == 6){
    df_rais.CL = left_join(rais_CL_2017, rais_CL_2018, by = 'Seção') %>% 
                 left_join(rais_CL_2019, by = 'Seção') %>% 
                 left_join(rais_CL_2020, by = 'Seção') %>% 
                 left_join(rais_CL_2021, by = 'Seção')
    
    rm(list=ls(pattern = 'rais_CL_'))
    break()
  }
}


# COEFICIENTE DE ESPECIALIZAÇÃO --------------------------------------------------------------

i=1
repeat{
  
  assign(str_glue('rais_CE_{2016+i}'),
         subset(df_emprego_formal, ano == 2016+i) %>%
           mutate(
             df_e_ki = `S_Referencia`,
             df_e_i = Total,
             df_E = apply(.[5], 2, sum ),
             df_e_k = apply(.[5], 2, sum ),
             # PRINCIPAL
             CE = .5 * abs(df_e_ki/df_e_i - df_e_k/df_E)
           ) %>% select(ano, id_municipio, CE)  %>% 
                 pivot_wider(names_from = ano, values_from = CE))
  
  i = i + 1  
  
  if(i == 6){
    df_rais.CE = left_join(rais_CE_2017, rais_CE_2018, by = 'id_municipio') %>% 
                 left_join(rais_CE_2019, by = 'id_municipio') %>% 
                 left_join(rais_CE_2020, by = 'id_municipio') %>% 
                 left_join(rais_CE_2021, by = 'id_municipio')
    
    rm(list=ls(pattern = 'rais_CE_'))
    break()
  }
}

# COEFICIENTE DE REDISTRIBUIÇÃO --------------------------------------------------------------

df_emprego_formal3 <- df_emprego_formal %>% 
                                     pivot_wider(names_from = ano,
                                     values_from = `Demais setores`:Total)
i=1
repeat{
  
  assign(str_glue('rais_CR_{i}'),
         df_emprego_formal3 %>%
           mutate(
             df_e_ki = get(str_glue('S_Referencia_{2016+i}')),
             df_e_k = apply(.[which(names(df_emprego_formal3) == str_glue('S_Referencia_{2016+i}'))], 2, sum ),
             
             df2_e_ki = get(str_glue('S_Referencia_{2017+i}')),
             df2_e_k = apply(.[which(names(df_emprego_formal3) == str_glue('S_Referencia_{2017+i}'))], 2, sum ),
             # PRINCIPAL
             CR = CR(df2_e_ki, df_e_ki, df2_e_k, df_e_k)
           ) %>% select(id_municipio, CR))
  
  i = i + 1  
  
  if(i == 5){
    df_rais.CR = left_join(rais_CR_1, rais_CR_2, by = 'id_municipio') %>% 
                 left_join(rais_CR_3, by = 'id_municipio') %>% 
                 left_join(rais_CR_4, by = 'id_municipio') 
    rm(list=ls(pattern = 'rais_CR_'))
    break()
  }
}

names(df_rais.CR)[2:5] <- c('2017-2018','2018-2019','2019-2020','2020-2021')

# INDICE DE HIRSCHMAN-HERFINDAHL

i=1
repeat{
  
  assign(str_glue('rais_IHHm_{2016+i}'),
         subset(df_emprego_formal2, ano == 2016+i) %>%
           mutate(
             df_e_ki = `Parnaíba`,
             df_e_i = apply(.[4], 2, sum ),
             df_E = apply(.[3], 2, sum ),
             df_e_k = `Total`,
             # PRINCIPAL
             IHHm = IHHm(df_e_ki, df_e_i, df_e_k, df_E)
           ) %>% select(ano, Seção, IHHm)  %>% pivot_wider(names_from = ano, values_from = IHHm))
  
  i = i + 1  
  
  if(i == 6){
    df_rais.IHHm = left_join(rais_IHHm_2017, rais_IHHm_2018, by = 'Seção') %>% 
      left_join(rais_IHHm_2019, by = 'Seção') %>% 
      left_join(rais_IHHm_2020, by = 'Seção') %>% 
      left_join(rais_IHHm_2021, by = 'Seção')
    
    rm(list=ls(pattern = 'rais_IHHm_'))
    break()
  }
}



#> FIGURAS ------------------------------------------------------------------------
 
 


URL = "https://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_2021/UFs/PI/PI_Municipios_2021.zip"
#> DADOS DO EMPREGO RAIS -------------------------------------------------------

# Baixe os dados do estado do Piauí
temp_shapefile <- tempfile()
download.file(URL, temp_shapefile)
unzip(temp_shapefile)

# Lendo o arquivo
piaui <- read_sf('PI_Municipios_2021.shp', 
                 options = 'ENCODING=UTF-8') 


#> BASE DOS MAPAS --------------------------------------------------------------

# i. coeficiente de especialização
piaui_CE <- piaui %>% left_join(df_rais.CE, by = c('CD_MUN'='id_municipio')) %>%
      pivot_longer(names_to = 'Ano', 
                   values_to = 'CE', 
                   cols = `2017`:`2021`) %>%
  
      mutate(CE2 = case_when(
        CE == 0 ~ '0 - Disperso', 
        CE > 0 & CE <= .3 ~ '0 a 0,30 - Pouco disperso',
        CE > .3 & CE <= .5 ~ '0,3 a 0,05 - Pouquíssimo disperso',
        CE > .5 & CE <= .7 ~ '0,5 a 7 Pouquíssimo concentrado',
        CE > .7 ~ '1 - Pouco concentrado'
      ))

# ii. coeficiente de redistribuição
piaui_CR <- piaui %>% left_join(df_rais.CR, by = c('CD_MUN'='id_municipio')) %>%
  pivot_longer(names_to = 'Ano', 
               values_to = 'CR', 
               cols = `2017-2018`:`2020-2021`) %>%
  
  mutate(CR2 = case_when(
    CR == 0 ~ '0 - Não houve reestruturação', 
    CR > 0 & CR < .001 ~ '0 a 0,001 - Baixíssima reestruturação',
    CR >= .001 & CR <= .01 ~ '0,001 a 0,01 - Baixa reestruturação',
    CR > .01 & CR <= .02 ~ '0,001 a 0,01 - Alguma reestruturação',
    CR == 1 ~ '1 - Altamente reestruturado'
  ))

# iii. quociente locacional
piaui_QL <- piaui %>% left_join(df_rais.QL, by = c('CD_MUN'='id_municipio')) %>%
  pivot_longer(names_to = 'Ano', 
               values_to = 'QL', 
               cols = `2017`:`2021`) %>%
  mutate(QL2 = case_when(QL > 1 ~ 'concentração significativa',
                         QL <= 1 & QL > .5 ~ 'concentração média',
                         QL <= .5 ~ 'concentração fraca'))

# LISTA CONSDOLIDADA -----------------------------------------------------------

mapas <- list(
  'QL_pi' = piaui_QL,
  'CE_pi' = piaui_CE,
  'CR_pi' = piaui_CR
)

rm(list = ls(pattern = 'piaui'))

# ! MAPAS DOS INDICADORES ! #

# MAPAS i -----------------------------------------------------------------------

gg <- ggplot()
gg <- gg + geom_sf(data = mapas$CR_pi, size=.05,
                   aes(geometry = geometry, 
                       fill = CR2)) 

gg <- gg + scale_fill_brewer(palette = "Paired", name="CR")
gg <- gg + labs(title = "COEFICIENTE DE REDISTRIBUIÇÃO - SERVIÇOS", 
                subtitle = "Ano-a-Ano",
                caption = "Fonte: Elaboração própria", size = 8, fill = '')

gg <- gg + theme_bw() 

gg <- gg + theme(legend.position="left",
                 axis.text.x = element_text(size = 6)) + facet_wrap(~Ano) 

gg

ggsave('mapa_cr_SERV.png', gg, dpi = 200, width = 9, height = 6)

# MAPAS ii -----------------------------------------------------------------------

gg2 <- ggplot()
gg2 <- gg2 + geom_sf(data = mapas$QL_pi, size=.05,
                   aes(geometry = geometry, 
                       fill = QL2)) 

gg2 <- gg2 + scale_fill_brewer(palette = "Paired", name="QL")
gg2 <- gg2 + labs(title = "QUOCIENTE LOCACIONAL (2017-21 - SERVIÇOS)", 
                caption = "Fonte: Elaboração própria", size = 8, fill = '')

gg2 <- gg2 + theme_bw() 

gg2 <- gg2 + theme(legend.position="left",
                 axis.text.x = element_text(size = 6)) + facet_wrap(~Ano) 

gg2

ggsave('mapa_ql_SERV.png', gg2, dpi = 200, width = 8, height = 6)

# MAPAS iii -----------------------------------------------------------------------

gg3 <- ggplot()
gg3 <- gg3 + geom_sf(data = mapas$CE_pi, size=.05,
                     aes(geometry = geometry, 
                         fill = CE2)) 

gg3 <- gg3 + scale_fill_brewer(palette = "Paired", name="CE")
gg3 <- gg3 + labs(title = "COEFICIENTE LOCACIONAL (2017-21 - SERVIÇOS)", 
                  caption = "Fonte: Elaboração própria", size = 8, fill = '')

gg3 <- gg3 + theme_bw() 

gg3 <- gg3 + theme(legend.position="left",
                   axis.text.x = element_text(size = 6)) + facet_wrap(~Ano) 

gg3

ggsave('mapa_cl_SERV.png', gg3, dpi = 200, width = 8, height = 6)

#> COEFICIENTE DE LOCALIZAÇÃO --------------------------------------------------

df_rais.CL %<>% pivot_longer(names_to = 'Ano', values_to = 'CL', cols = `2017`:`2021`)

gg_CL <- ggplot(data = df_rais.CL,
                aes(y = CL, x = reorder(Seção, -CL)))

gg_CL <- gg_CL + geom_bar(position="stack", 
                          stat="identity", 
                          fill = "steelblue") 

gg_CL <- gg_CL + geom_text(aes(y = CL+(CL*-.5),label = round(CL,4)),
                               color = "black", size = 3) 

gg_CL <- gg_CL + coord_flip() + facet_wrap(~Ano, nrow = 1)

gg_CL <- gg_CL +  labs(title = "Coeficiente de Localização - Parnaíba",
                       subtitle = "2017 a 2021",
                       caption = "Fonte: Elaboração própria",
                       x = "Setor", y = "CL") +  theme_bw() 
gg_CL <- gg_CL + theme(axis.text.y = element_text(face = 'bold'))

gg_CL

ggsave('cl_RICO.png', gg_CL, dpi = 200, width = 9, height = 5)

#> INDICE DE HIRSCHMAN-HERFINDAL -----------------------------------------------
#> 

df_rais.IHHm %<>% pivot_longer(names_to = 'Ano', values_to = 'IHHm', cols = `2017`:`2021`)

gg_IHHm <- ggplot(data = df_rais.IHHm,
                aes(y = IHHm, x = reorder(Seção, -IHHm)))

gg_IHHm <- gg_IHHm + geom_bar(position="stack", 
                          stat="identity", 
                          fill = "steelblue") 

gg_IHHm <- gg_IHHm + geom_text(aes(y = IHHm+(IHHm*-.5), label = IHHm),
                              color = "black", size = 2.5) 

gg_IHHm <- gg_IHHm + coord_flip() + facet_wrap(~Ano, nrow = 1)

gg_IHHm <- gg_IHHm +  labs(title = "Índice de Hirshman-Herfindahl modificado - Parnaíba",
                       subtitle = "2017 a 2021",
                       caption = "Fonte: Elaboração própria",
                       x = "Setor", y = "IHHm") +  theme_bw() 

gg_IHHm <- gg_IHHm + theme(axis.text.y = element_text(face = 'bold'))
  
gg_IHHm

ggsave('ihh_RICO.png', gg_IHHm, dpi = 200, width = 9, height = 5)

library(ggrepel)
