library(rvest)
library(stringi)
library(tidyverse)
library(geobr)
library(magrittr)
library(tibble)
## Carregando pacotes exigidos: xml2

install.packages("geobr")

dados <- read.delim("clipboard")

dados %<>% select(4,16) 

names(dados)[] <- ""

########################################

dados_mapa <- read_municipality(year=2010, showProgress = FALSE) %>% filter(code_state == '22') %>%
  mutate(name_muni = str_to_upper(name_muni))

dados$MUNICÍPIO = str_trim(str_to_upper(dados$MUNICÍPIO))

t <- dados %>% left_join(dados_mapa, by = c("MUNICÍPIO" = "name_muni"))

########################################
t <- as.data.frame(t)

plot = ggplot() +
  geom_sf(data=t, aes(fill=`Produção.Média..do.Município.`, geometry = geom), size=.15,)+
  labs(title="PRODUÇÃO MÉDIA DOS MUNICÍPIOS DO PIAUÍ (Mil)",
       subtitle = 'Média de 2010 a 2020',
       caption='Fonte: Elaboração própria', size=8)+
  scale_fill_distiller(palette = "RdGy", limits=c(0,900),
                       name="Faixa da produto")+
  theme_minimal() 
plot  

ggsave(plot = plot, "produção_pi.png", dpi = 200, width = 6, height = 6)
