#_______________________________________________________________________________

# Processamento e Análise dos dados da RAIS 21

# Autor: Alexandre Barros 
# Email: alexandre.barros@ufpi.edu.br

# --/12/2022
#_______________________________________________________________________________

#> Dependências ----------------------------------------------------------------

install.packages(c('openxlsx', 'sf'))
remotes::install_github("georgevbsantiago/qsacnpj")

library(openxlsx)
library(tidyverse)
library(sf)
library(magrittr)

getwd()

#> PREPADANDO OS DADOS ---------------------------------------------------------
CNAE <- select(qsacnpj::tab_cnae, 2,8,9)

dados_rais <- readxl::read_excel("RAIS - ERU II.xlsx")
summary(dados_rais)


#> CIDADE E SETOR ESCOLHIDOS


{ # > RODE AQUI <
  
# caixa de diálogo para informar o município da análise
muni <- svDialogs::dlg_list(
  title = "Escolha o município",
  choices = unique(dados_rais$Município)
)$res

# delimitação do município filtrado
dados_rais2 <- subset(dados_rais, dados_rais$Município==muni)

# aqui consideramos o setor mais expressivo para o município
setor <- names(dados_rais2[,4:11])[which.max(apply(dados_rais2[,4:11], 2, max))]

# remove a df
rm(dados_rais2)

}


#> CONSOLIDANDO ----------------------------------------------------------------
 
{ # > RODE AQUI <
  
# Calcule os totais por Ano usando a função aggregate
totais <- aggregate(cbind(Total) ~ Ano, data = dados_rais, FUN = sum)

# Renomeie a coluna de totais para "total"
colnames(totais) <- c("Ano", "Piauí")

# Combine os totais ao data.frame original usando a função merge
df_emprego_formal <- merge(dados_rais, totais, by = "Ano", all.x = TRUE) 

# filtrando os dados apenas do município-referência
df_emprego_formal %<>% filter(Município == muni)

# setores mais expressivos
Totais_Setor <- aggregate(dados_rais[, setor], by = list(dados_rais$Ano), FUN = sum)
colnames(Totais_Setor) <- c("Ano", setor)

}



{

df_emprego_formal <- dados_rais_2 %>%
  group_by(Ano, Setor, id_municipio) %>%
  summarise(Vínculos = sum(Vinculos)) %>%
  pivot_wider(names_from = 'Setor', 
              values_from = 'Vínculos', 
              values_fill = 0) %>%
  mutate(Total = `S_Referencia` + `Demais setores`)


df_emprego_formal2 <- dados_rais_2 %>%
  mutate(Regiao = ifelse(id_municipio == '2209450',
                        'Parnaíba',
                        'Outras cidades')) %>%
  group_by(Ano, Seção, Regiao) %>%
  summarise(Vínculos = sum(Vinculos)) %>%
  pivot_wider(names_from = 'Regiao', 
              values_from = 'Vínculos', 
              values_fill = 0) %>% 
  mutate(Total = `Parnaíba` + `Outras cidades`)

}

#> SALVANDO BASES --------------------------------------------------------------
#> 

#salvando a base

dataset_names <- list('CL' = df_rais.CL, 
                      'CR' = df_rais.CR, 
                      'CE' = df_rais.CE,
                      'QL' = df_rais.QL,
                      'IHHM' = df_rais.IHHm)


# exportanto para excel
openxlsx::write.xlsx(dataset_names, file = 'Indicadores - ERU II.xlsx') 

# ! INDICADORES !

{ # > Rode aqui < 

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
} # -- fim das funções

# QUOCIENTE LOCACIONAL -------------------------------------------------------------------

i<-1
df_rais.QL <- data.frame()

repeat{
  
  assign(str_glue('rais_QL_{2011+i}'),
    subset(df_emprego_formal, Ano == 2011+i) %>%
      mutate(
       df_e_ki = across(setor),
       df_e_i = Total,
       df_e_k = Totais_Setor[Totais_Setor$Ano == 2011+i,2],
       df_E = Piauí,
       # PRINCIPAL
       QL = QL(df_e_ki, df_e_i, df_e_k, df_E)
    ) %>% select(Ano, Município, QL)) 

  df_rais.QL <- rbind(df_rais.QL, get(str_glue('rais_QL_{2011+i}')))
  i = i + 1  
  
if(i == 10){
  rm(list=ls(pattern = 'rais_QL_'))
  break()
}
}

# COEFICIENTE LOCACIONAL -------------------------------------------------------------------


i<-1
df_rais.CL <- data.frame() 
repeat{
   
  assign(str_glue('rais_CL_{2011+i}'),
         subset(df_emprego_formal, Ano == 2011+i) %>%
           mutate(
             df_e_ki = across(setor),
             df_e_i = Total,
             df_E = Piauí,
             df_e_k = Totais_Setor[Totais_Setor$Ano == 2011+i,2],
             # PRINCIPAL
             CL = .5 * abs(df_e_ki/df_e_i - df_e_k/df_E)
           ) %>% select(Ano, Município, CL)) 
  # CONSOLIDANDO
  df_rais.CL <- rbind(df_rais.CL, get(str_glue('rais_CL_{2011+i}')))
  i = i + 1  
  
  if(i == 11){
    rm(list=ls(pattern = 'rais_CL_'))
    break()
  }
}


# COEFICIENTE DE ESPECIALIZAÇÃO --------------------------------------------------------------

i=1
df_rais.CE <- data.frame()

repeat{
  
  assign(str_glue('rais_CE_{2011+i}'),
         subset(df_emprego_formal, Ano == 2011+i) %>%
           mutate(
             df_e_ki = across(setor),
             df_e_i = Total,
             
             df_E = Piauí,
             df_e_k = Totais_Setor[Totais_Setor$Ano == 2011+i,2],
             # PRINCIPAL
             CE = .5 * abs(df_e_ki/df_e_i - df_e_k/df_E)
           ) %>% select(Ano, Município, CE))  
  
  df_rais.CE <- rbind(df_rais.CE, get(str_glue('rais_CE_{2011+i}')))
  i = i + 1  
  
  if(i == 11){
    rm(list=ls(pattern = 'rais_CE_'))
    break()
  }
}

# COEFICIENTE DE REDISTRIBUIÇÃO --------------------------------------------------------------

i=1
df_rais.CR <- data.frame()
repeat{
  
  assign(str_glue('rais_CR_{i}'),
      
         data.frame(
           df_e_ki = df_emprego_formal[
             df_emprego_formal$Ano == str_glue('{2011+i}'),
             which(colnames(df_emprego_formal) == setor)],
           
           df_e_k = Totais_Setor[Totais_Setor$Ano == as.numeric(str_glue('{2011+i}')), 2],
           
           df2_e_ki = df_emprego_formal[
             df_emprego_formal$Ano == str_glue('{2012+i}'),
             which(colnames(df_emprego_formal) == setor)],
           
           df2_e_k = Totais_Setor[Totais_Setor$Ano == str_glue('{2012+i}'), 2]
         ) %>% 
           mutate(CR = CR(df2_e_ki, df_e_ki, df2_e_k, df_e_k),
                  Período = paste(str_glue('{2011+i}'),str_glue('{2012+i}'),sep = '-')) %>%
           select(Período, CR))
  
  
  df_rais.CR <- rbind(df_rais.CR, get(str_glue('rais_CR_{i}')))
  i = i + 1  
  
  if(i == 9){
    rm(list=ls(pattern = 'rais_CR_'))
    break()
  }
}

names(df_rais.CR)[2:5] <- c('2017-2018','2018-2019','2019-2020','2020-2021')

data.frame(  
  df_e_ki = as.numeric(df_emprego_formal[
    df_emprego_formal$Ano==str_glue('{2011+i}'),
    which(colnames(df_emprego_formal) == setor)]))
  
# INDICE DE HIRSCHMAN-HERFINDAHL

i=1
repeat{
  
  assign(str_glue('rais_IHHm_{2016+i}'),
         subset(df_emprego_formal2, Ano == 2016+i) %>%
           mutate(
             df_e_ki = `Parnaíba`,
             df_e_i = apply(.[4], 2, sum ),
             df_E = apply(.[3], 2, sum ),
             df_e_k = `Total`,
             # PRINCIPAL
             IHHm = IHHm(df_e_ki, df_e_i, df_e_k, df_E)
           ) %>% select(Ano, Seção, IHHm)  %>% pivot_wider(names_from = Ano, values_from = IHHm))
  
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
