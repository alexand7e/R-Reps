# Carregue as bibliotecas necessárias
library(sf)
library(tidyverse)

source('Microdados_CAGED_xlsx.R')

dados_mapa <- read_municipality(year=2010, showProgress = FALSE) %>% 
  filter(code_state == '22')

dados_mapa %<>% mutate(code_muni = as.character(code_muni)) %>%
  merge(d_territorios, by.x = 'code_muni', by.y = 'codibge', all.x = T) %>%
  merge(caged_territórios, by = 'territorio', all.x = T)


# a porra do mapa http://127.0.0.1:23449/graphics/17f73e02-f467-4e86-9a35-18c92ac61af3.png
plot = ggplot() +
  geom_sf(data=dados_mapa, aes(fill=Saldo, geometry = geometry), size=.15,)+
  labs(title="Título",
       subtitle = 'Ano base: 2010',
       caption='Fonte: Elaboração própria', size=8)+
  scale_fill_distiller(palette = 'Greens',
                       name="Saldo")+
  theme_minimal() 
plot  

plot <- ggplot() +
  geom_sf(data = dados_mapa, aes(fill = Território, geometry = geometry), size = 0.15, color = NA) +
  labs(title = "CAGED - Saldo acumulado",
       subtitle = 'Ano base: 2023',
       caption = 'Fonte: Elaboração própria',
       size = 8) +
  viridis::scale_fill_viridis(discrete = TRUE,direction=-1) +
  theme_minimal()

plot


zplot <- ggplot() +
  geom_sf(data = dados_mapa, aes(fill = Saldo, geometry = geometry), size = 0.15, color = NA) +
  labs(title = "Título",
       subtitle = 'Ano base: 2010',
       caption = 'Fonte: Elaboração própria',
       size = 8) +
  scale_fill_manual(values = c("CARNAUBAIS" = "white")) + # Escala discreta
  scale_fill_distiller(palette = "Greens", name = "Saldo") + # Escala contínua
  theme_minimal()

plot