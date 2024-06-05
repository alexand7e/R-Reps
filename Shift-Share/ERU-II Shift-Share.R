#_______________________________________________________________________________

# CÁLCULO SHIFT-SHARE - ECONOMIA REGIONAL E URBANA II

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
library(svDialogs)
library(leaflet)
library(mapview)

# ! SCRIPT !

setwd('C:/Users/Alexandre/OneDrive/Documentos/R/')

# FUNÇÕES ----------------------------------------------------------------------

#> ! FUNÇÃO DO CÁLCULO - MODELO CLÁSSICO ! 


shift.share <- 
  function(region_t, region_t1, nation_t, nation_t1) {
    
    industries <- length(region_t)
    
    sum_region_t <- sum(region_t)
    sum_region_t1 <- sum(region_t1)
    sum_nation_t <- sum(nation_t)
    sum_nation_t1 <- sum(nation_t1)
    # National Growth Effect on industry i in the rth region between (t-1) and t.
    
    i <- 0
    NS_tir <- vector()
    IM_tir <- vector()
    RS_tir <- vector()
    for (i in 1:industries) {
      NS_tir[i] <- (region_t[i] * ((sum_nation_t1/sum_nation_t) - 1))
      IM_tir[i] <- (region_t[i] * ((nation_t1[i]/nation_t[i]) - (sum_nation_t1/sum_nation_t)))
      RS_tir[i] <- (region_t[i] * ((region_t1[i]/region_t[i]) - (nation_t1[i]/nation_t[i])))
    }
    shifts <- list(Var_Regional = NS_tir, 
                   Var_Estrutural = IM_tir, 
                   Var_Diferencial = RS_tir)
    
    return(shifts)
  }


#> ! SHIFT-SHARE CONFORME O MODELO DE ESTEBAN-MARQUILAS !


shift.esteban <-
  function(region_t, region_t1, nation_t, nation_t1) {
    
    industries <- length(region_t)
    
    sum_region_t <- sum(region_t)
    sum_region_t1 <- sum(region_t1)
    sum_nation_t <- sum(nation_t)
    sum_nation_t1 <- sum(nation_t1)
    # National Growth Effect on industry i in the rth region between (t-1) and t.
    
    i      <- 0
    h      <- vector()
    
    NS_tir <- vector()
    IM_tir <- vector()
    RS_tir <- vector()
    AL_tir <- vector()
    
    for (i in 1:industries) {
      h[i] <- region_t[i] * (nation_t[i]/sum_nation_t)
      
      NS_tir[i] <- (region_t[i] * ((sum_nation_t1/sum_nation_t) - 1))
      IM_tir[i] <- (region_t[i] * ((nation_t1[i]/nation_t[i]) - (sum_nation_t1/sum_nation_t)))
      RS_tir[i] <- (h[i] * ((region_t1[i]/region_t[i]) - (nation_t1[i]/nation_t[i])))
      
      AL_tir[i] <- ((region_t[i] - h[i]) * ((region_t1[i]/region_t[i]) - (nation_t1[i]/nation_t[i])))
    }
    
    shifts <- list(Var_Regional = NS_tir, 
                   Var_Estrutural = IM_tir, 
                   Var_Diferencial = RS_tir,
                   Var_Alocativo = AL_tir)
    
    return(shifts)
  }



#> Importando dados ------------------------------------------------------------
#> 



dados_rais <- read.csv(
  "RAIS_11_21.csv",
  sep = ";", dec = ",",
  check.names = F, fileEncoding = "Latin1"
)



#> Ajustando a base ------------------------------------------------------------



dados_rais %<>% pivot_longer(cols = `2021 - Extrativa mineral`:`2012 - Total`,
                            names_to = "Setor",
                            values_to = "Vinculos") %>% 
  
                mutate(Ano = substr(Setor,0,4),
                       Setor = substr(Setor,8,nchar(Setor))) %>%
  
                filter(Setor != 'Total')



#> Base para o estado e o Brasil -----------------------------------------------



dados_rais_PI_BR <- dados_rais %>% group_by(Setor, Ano) %>%
  summarise(Piaui = sum(Vinculos[UF == 'PI'],na.rm = T),
            Brasil = sum(Vinculos, na.rm = T)) %>%
  
  pivot_wider(names_from = Ano,values_from = Piaui:Brasil)



#> Base para o Piauí -----------------------------------------------------------



# I - base filtrada
RAIS_piaui <- dados_rais %>% filter(UF == "PI") 

# II - summarização de todo o estado
RAIS_piaui_S <- RAIS_piaui %>% group_by(Ano, Setor) %>% 
  summarise(Vinculos_PI = sum(Vinculos))

# III - inserção na base os valores agregados
RAIS_piaui <- RAIS_piaui %>% merge(RAIS_piaui_S, by = c('Setor', 'Ano'), all.x = T)





#> ! APLICAÇÃO DO MODELO !


# I - data.frame inicial 
shift_classico.muni <- data.frame(Cidade = character(), 
                         Setor = character(),
                         Regional = numeric(), 
                         Estrutural = numeric(), 
                         Diferencial = numeric())

shift_ampliado.muni <- data.frame(Cidade = character(), 
                         Setor = character(),
                         Regional = numeric(), 
                         Estrutural = numeric(), 
                         Diferencial = numeric(),
                         Alocativa = numeric())


# II - loop para calcular todos os municipios
for(i in 1:224){
  
  municipio_atual <- unique(RAIS_piaui$MUNICÍPIO)[i]
  
  # I
  Valores = shift.share(
    region_t = subset(RAIS_piaui, MUNICÍPIO == municipio_atual & Ano == 2012, select = Vinculos),
    region_t1= subset(RAIS_piaui, MUNICÍPIO == municipio_atual & Ano == 2021, select = Vinculos),
    nation_t = subset(RAIS_piaui, MUNICÍPIO == municipio_atual & Ano == 2012, select = Vinculos_PI),
    nation_t1= subset(RAIS_piaui, MUNICÍPIO == municipio_atual & Ano == 2021, select = Vinculos_PI))
  # II
  Valores2 = shift.esteban(
    region_t = subset(RAIS_piaui, MUNICÍPIO == municipio_atual & Ano == 2012, select = Vinculos),
    region_t1= subset(RAIS_piaui, MUNICÍPIO == municipio_atual & Ano == 2021, select = Vinculos),
    nation_t = subset(RAIS_piaui, MUNICÍPIO == municipio_atual & Ano == 2012, select = Vinculos_PI),
    nation_t1= subset(RAIS_piaui, MUNICÍPIO == municipio_atual & Ano == 2021, select = Vinculos_PI))
  
  # I
  nova_linha = data.frame(
    Cidade = municipio_atual,
    Setor = unique(RAIS_piaui$Setor),
    Regional = Valores[[1]],
    Estrutural = Valores[[2]],
    Diferencial = Valores[[3]]
   )
  # II
  nova_linha2 = data.frame(
    Cidade = municipio_atual,
    Setor = unique(RAIS_piaui$Setor),
    Regional = Valores2[[1]],
    Estrutural = Valores2[[2]],
    Diferencial = Valores2[[3]],
    Alocativa = Valores2[[4]]
  )
  
  shift_classico.muni = data.table::rbindlist(list(shift_classico.muni, nova_linha))
  shift_ampliado.muni = data.table::rbindlist(list(shift_ampliado.muni, nova_linha2))
  
  if(i==224){
    shift_classico.muni %<>% 
                    purrr::modify_if(~is.numeric(.), ~round(., 1)) %>%
                    mutate_all(~replace(., is.nan(.), 0))
    shift_ampliado.muni %<>% 
                    purrr::modify_if(~is.numeric(.), ~round(., 1)) %>%
                    mutate_all(~replace(., is.nan(.), 0))
    
    }
}


#> Aplicação para o Piauí-Brasil -----------------------------------------------



if(T){ # CLÁSSICO
  
  Valores = shift.share(
    region_t = dados_rais_PI_BR$Piaui_2012,
    region_t1= dados_rais_PI_BR$Piaui_2021,
    nation_t = dados_rais_PI_BR$Brasil_2012,
    nation_t1= dados_rais_PI_BR$Brasil_2021)
  
  
  shift_classico.estado = data.frame(
    Estado = 'Piauí',
    Setor = unique(RAIS_piaui$Setor),
    Regional = Valores[[1]],
    Estrutural = Valores[[2]],
    Diferencial = Valores[[3]]
  ) %>% 
    # ATT
    mutate(Shift_Total = Regional+Estrutural+Diferencial) %>% 
    purrr::modify_if(~is.numeric(.), ~round(., 1)) %>%
    mutate_all(~replace(., is.nan(.), 0))
  
}


if(T){ # AMPLIADO - ESTEBAN-MARQUILLAS
  
  Valores = shift.esteban(
    region_t = dados_rais_PI_BR$Piaui_2012,
    region_t1= dados_rais_PI_BR$Piaui_2021,
    nation_t = dados_rais_PI_BR$Brasil_2012,
    nation_t1= dados_rais_PI_BR$Brasil_2021)
  
  
  shift_ampliado.estado = data.frame(
    Estado = 'Piauí',
    Setor = unique(RAIS_piaui$Setor),
    Regional = Valores[[1]],
    Estrutural = Valores[[2]],
    Diferencial = Valores[[3]],
    Alocativa = Valores[[4]]
  ) %>% 
    # ATT
    mutate(Shift_Total = Regional+Estrutural+Diferencial+Alocativa) %>% 
    purrr::modify_if(~is.numeric(.), ~round(., 1)) %>%
    mutate_all(~replace(., is.nan(.), 0))
  
}


#>  ! MAPAS E FIGURAS !

 
# SHAPE DO ESTADO --------------------------------------------------------------


URL = "https://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_2021/UFs/PI/PI_Municipios_2021.zip"
#> DADOS DO EMPREGO RAIS -------------------------------------------------------

# Baixe os dados do estado do Piauí
temp_shapefile <- tempfile()
download.file(URL, temp_shapefile)
unzip(temp_shapefile)

# Lendo o arquivo
piaui <- read_sf('PI_Municipios_2021.shp', 
                 options = 'ENCODING=UTF-8') %>% 
  
         mutate(MUNICIPIO = toupper(iconv(NM_MUN, 
                                          "UTF-8", "ASCII//TRANSLIT")),
                MUNICIPIO = gsub("'", " ", MUNICIPIO))



# adicionando os municipios
shift.muni <- shift_classico.muni %>% left_join(piaui, by = c('Cidade'='MUNICIPIO'))




# SHAPE DO PAÍS ----------------------------------------------------------------


URL2 = "https://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_2021/Brasil/BR/BR_UF_2021.zip"



#> MAPA INTERATIVO -------------------------------------------------------------


shift.muni <- st_as_sf(shift.muni) %>%
  select(NM_MUN, SIGLA, Setor, Regional, Estrutural, Diferencial) %>%
  mutate(Total = Regional+Estrutural+Diferencial)


set = dlgList(title = "Informe o setor analisado", 
        unique(dados_rais$Setor))$res


filter(shift.muni, Setor == set) %>%
mapview(zcol="Total", label=shift.muni$NM_MUN, 
        labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE)) 





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


#> COEFICIENTE DE LOCALIZAÇÃO --------------------------------------------------



# Graph 1 - SS CLÁSSICO ========================================================


df_shift_s <- shift_classico.estado %>% pivot_longer(names_to = 'Elemento', 
                                            values_to = 'Shift_s', 
                                            cols = Regional:Diferencial) %>%
  filter(Setor!='Extrativa mineral') %>%
  mutate(Elemento=factor(Elemento,
                         levels=c('Regional','Estrutural',
                                  'Diferencial')))


gg_shift_s <- 
ggplot(df_shift_s, aes(y=Shift_s, x=Elemento, fill=Elemento))

gg_shift_s <- gg_shift_s +  geom_bar(position="dodge", stat="identity") 
gg_shift_s <- gg_shift_s +  scale_y_continuous(breaks = seq(-8000,20000,4000),
                                               labels = scales::number_format())
gg_shift_s <- gg_shift_s +  viridis::scale_fill_viridis(discrete = T, option = "E") 
gg_shift_s <- gg_shift_s +  labs(title = "Modelo Shift-share clássico aplicado ao Piauí",
                                 subtitle = "Período 2012-2021",
                                 caption = "Fonte: Elaboração própria, com dados da RAIS",
                                 x = "Setor", y = "") 
gg_shift_s <- gg_shift_s +  theme_light() 
gg_shift_s <- gg_shift_s +  facet_wrap(~Setor, strip.position = "bottom", nrow=1,
                                       labeller = labeller(Setor = label_wrap_gen(width = 20))) 
gg_shift_s <- gg_shift_s +  theme(axis.text.x = element_blank())
gg_shift_s



# Graph 1 - SS AMPLIADO ========================================================



df_shift_s <- shift_ampliado.estado%>% pivot_longer(names_to = 'Elemento', 
                                            values_to = 'Shift_s', 
                                            cols = Regional:Alocativa) %>%
  filter(Setor!='Extrativa mineral') %>%
  mutate(Elemento=factor(Elemento,
                         levels=c('Regional','Estrutural',
                                  'Diferencial','Alocativa')))


gg_shift_s <- 
  ggplot(df_shift_s, aes(y=Shift_s, x=Elemento, fill=Elemento))

gg_shift_s <- gg_shift_s +  geom_bar(position="dodge", stat="identity")  +  
                            scale_y_continuous(breaks = seq(-8000,17000,3000),
                                               labels = scales::number_format()) +  
                            viridis::scale_fill_viridis(discrete = T, option = "E") +  
                            labs(title    = "Modelo Shift-share Ampliado - Piauí",
                                 subtitle = "Período 2012-2021",
                                 caption  = "Fonte: Elaboração própria, com dados da RAIS",
                                 x        = "Setor", 
                                 y        = "") +  
                            theme_light() +  
                            facet_wrap(~Setor, strip.position = "bottom", nrow=1,
                                       labeller = labeller(Setor = label_wrap_gen(width = 20))) +  
  
                            theme(axis.text.x = element_blank())

gg_shift_s


ggsave('shifts_classico.png', gg_shift_s, dpi = 200, width = 9.2, height = 4.5)



# Graph 3 - SS CLÁSSICO ========================================================


df_shift_s <- shift_classico.muni %>% subset(Cidade %in% c('PARNAIBA','ALTOS')) %>% 
                                        pivot_longer(names_to = 'Elemento', 
                                                     values_to = 'Shift_s', 
                                                     cols = Regional:Diferencial) %>%
  filter(Setor!='Extrativa mineral') %>%
  mutate(Elemento=factor(Elemento,
                         levels=c('Regional','Estrutural',
                                  'Diferencial')))  


gg_shift_s <- 
  ggplot(df_shift_s, aes(y=Shift_s, x=Elemento, fill=Elemento))  +  
  geom_bar(position="dodge", stat="identity") +
  scale_y_continuous(breaks = seq(-100,2000,400),
                      labels = scales::number_format()) +  
  viridis::scale_fill_viridis(discrete = T, option = "E")  +  
  labs(title = "Modelo Shift-share clássico aplicado à Altos e Parnaíba",
                                 subtitle = "Período 2012-2021",
                                 caption = "Fonte: Elaboração própria, com dados da RAIS",
                                 x = "Setor", y = "") +  
  theme_light()  +  facet_grid(Cidade~Setor, 
                                       labeller = labeller(Setor = label_wrap_gen(width = 20))) +  
  theme(axis.text.x = element_blank())
gg_shift_s


ggsave('shifts_classico2.png', gg_shift_s, dpi = 200, width = 9.2, height = 4.5)

# Graph 4 - SS AMPLIADO ========================================================



 df_shift_s <- shift_ampliado.muni %>% subset(Cidade %in% c('PARNAIBA','ALTOS')) %>% 
                                       pivot_longer(names_to = 'Elemento', 
                                                    values_to = 'Shift_s', 
                                                    cols = Regional:Alocativa) %>%
  filter(Setor!='Extrativa mineral') %>%
  mutate(Elemento=factor(Elemento,
                         levels=c('Regional','Estrutural',
                                  'Diferencial','Alocativa')))


gg_shift_s <- 
  ggplot(df_shift_s, aes(y=Shift_s, x=Elemento, fill=Elemento))

gg_shift_s <- gg_shift_s +  geom_bar(position="dodge", stat="identity")  +  
  scale_y_continuous(breaks = seq(-200,1200,400),
                     labels = scales::number_format()) +  
  viridis::scale_fill_viridis(discrete = T, option = "E") +  
  labs(title    = "Modelo Shift-share Ampliado - Piauí",
       subtitle = "Período 2012-2021",
       caption  = "Fonte: Elaboração própria, com dados da RAIS",
       x        = "Setor", 
       y        = "") +  
  theme_light() + facet_grid(Cidade~Setor, 
                   labeller = labeller(Setor = label_wrap_gen(width = 20))) +  
  
  theme(axis.text.x = element_blank())

gg_shift_s


ggsave('shifts_ampliado2.png', gg_shift_s, dpi = 200, width = 9.2, height = 4.5)



#salvando a base



dataset_names <- list('SS_estado_classico'    = shift_classico.estado, 
                      'SS_municipio_classico' = shift_classico.muni %>% subset(Cidade %in% c('PARNAIBA','ALTOS')), 
                      'SS_estado_ampliado'    = shift_ampliado.estado, 
                      'SS_municipio_ampliado' = shift_ampliado.muni %>% subset(Cidade %in% c('PARNAIBA','ALTOS')))


# exportanto para excel
openxlsx::write.xlsx(dataset_names, file = 'Dados2 - Shift Share.xlsx') 

shift_classico.muni %>% subset(Cidade %in% c('PIRIPIRI')) %>% kableExtra::kbl()
