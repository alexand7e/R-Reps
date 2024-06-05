library(rvest)
library(stringi)
library(tidyverse)
library(geobr)
library(magrittr)
library(tibble)
## Carregando pacotes exigidos: xml2

html <- read_html("https://www.undp.org/pt/brazil/idhm-munic%C3%ADpios-2010")

html.table <- html %>% html_nodes("table")
dados <- html.table[[1]] %>% html_table()

#####################################

piaui <- dados %>%
  separate(col = Município, into = c("Município", "UF"), sep = "\\(") %>%
  mutate(UF = str_remove(UF, "\\)"), Município = str_trim(str_to_upper(Município))) %>%
  filter(UF == "PI")

#####################################

dados_mapa <- read_municipality(year=2010, showProgress = FALSE) %>% filter(code_state == '22') %>%
  mutate(name_muni = str_to_upper(name_muni))

t <- piaui %>% left_join(dados_mapa, by = c("Município" = "name_muni"))
t$`IDHM 2010` <- as.numeric(str_replace_all(t$`IDHM 2010`, ",", "."))

piaui$Município

######################################
t %<>% mutate(legendas = case_when(
  `IDHM 2010` > .4 & `IDHM 2010` < .5 ~ "Muito baixo",
  `IDHM 2010` >= .5 & `IDHM 2010` < .6 ~ "Baixo",
  `IDHM 2010` >= .6 & `IDHM 2010` < .7 ~ "Médio",
  `IDHM 2010` >= .7 & `IDHM 2010` < .8 ~ "Alto",
  `IDHM 2010` >= .8 ~ "Muito alto"
))

plot = ggplot() +
  geom_sf(data=t, aes(fill=`IDHM 2010`, geometry = geom), size=.15,)+
  labs(title="IDHM dos Municipíos do Piauí",
       subtitle = 'Ano base 2010',
       caption='Fonte: Elaboração própria', size=8)+
    scale_fill_distiller(palette = 'Greens',limits=c(0.4, 0.8),
                       name="Faixa IDH")+
  theme_minimal() 
plot  

#######################################
# Salvando a imagem

ggsave(plot = plot, "idhm_pi.png", dpi = 200, width = 6, height = 6)

typeof(t$`IDHM 2010`)
