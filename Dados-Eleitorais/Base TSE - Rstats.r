# Obtenção dos resultados do 2º Turno das Eleições
# Créditos: Gabriel Zanlorenssi (https://gist.github.com/gabrielzanlorenssi)
# Adptado por: Alexandre Barros
# Data: 01/11/2022

# Pacotes -----------------------------------------------------------------
install.packages("sf")
library(tidyverse)
library(jsonlite)
library(geobr)
library(reshape2)
library(sf)

# Configuracao dos municipios ---------------------------------------------

urlm = "https://resultados.tse.jus.br/oficial/ele2022/544/config/mun-e000544-cm.json"

muni_tse <- fromJSON(urlm,
                     simplifyDataFrame = TRUE) %>%
  .[['abr']]  %>%
  unnest('mu', names_repair="universal") %>%
  select(-c, -z) %>%
  set_names("uf", "estado", "tse", "ibge7", "nm") 

# Por municipio -----------------------------------------------------------

#-- codigo do pleito

#pleito = 544 # 1t !! (usar esse para testar) !!
pleito = 545 # 2t !! (usar esse no dia 30/10) !!
cargo = "0001" # presidente
arquivo = "-v"

#-- url das paginas que vamos usar
url_muni = paste0("https://resultados.tse.jus.br/oficial/ele2022/", # url padrao
                  pleito, # codigo do turno
                  "/dados/", # dados consolidados
                  str_to_lower(muni_tse$uf), # indentificacao do estado
                  "/", str_to_lower(muni_tse$uf), # identificacao do estado
                  muni_tse$tse, # codigo do tse do municipio
                  paste0("-c", cargo), # cargo
                  paste0("-e000", pleito), # pleito
                  arquivo, # tipo de arquivo
                  ".json")

# para evitar quedas do servidor do tse
rate <- rate_backoff(pause_base = 0.1, pause_min = 0.005, max_times = 100)

# funcao para rodar o fromJSON e ir avisando em que passo está
fs <- function(x,y) {
  x<-fromJSON(x, simplifyDataFrame = T)
  print(y)
  return(x)}

# rodar insistentemente e nao parar com erros
insistent <- insistently(fs,
                         rate, 
                         quiet = FALSE)

# map com a funcao criada. argumento y no imap é o iterando (1, 2, 3...)
municipios <- imap(url_muni, insistent)

# juntar todos os dados
dados <- municipios %>% 
  bind_rows() %>% 
  .[["abr"]] %>% 
  unnest(cand, names_repair="universal") %>%  
  filter(tpabr == "MU") |> 
  left_join(muni_tse, by=c("cdabr" = "tse")) %>% 
  mutate(votos = as.numeric(vap)) %>% 
  rename(ncand = n) %>%
  select(cdabr, ncand, votos, ibge7) %>%
  mutate(Candidato = ifelse(ncand == "13", "Lula", "Bolsonaro")) %>%
  pivot_wider(names_from = Candidato, values_from = votos, id_cols = c(cdabr, ibge7))

# Configurar mapa --------------------------------------------

#-- dados poligonais

dados_mapa <- read_municipality(year=2010, showProgress = FALSE) %>% mutate(code_muni = as.character(code_muni))

dados <- dados %>% inner_join(dados_mapa, by = c("ibge7" = "code_muni"))

#-- consoldiando a base
dados.com <- dados %>%
  mutate(`Pc Bolsonaro` = Bolsonaro/(Bolsonaro + Lula),
         `Pc Lula` = Lula/(Bolsonaro + Lula),
         `Conf` = ifelse(`Pc Lula` < .5, "Bozo", "Lula"),
         `Vantagem` = ifelse(`Pc Lula` > .5, `Pc Lula`-`Pc Bolsonaro`,`Pc Bolsonaro`-`Pc Lula`)) 
  
#-- figura
ggplot(data = dados.com) +
  geom_sf(aes(geometry=geom), fill='#d5d5d2', color= "grey", size=.15) +
  geom_point(
    aes(color = Conf, alpha = Vantagem, geometry = geom, size = Vantagem), 
    stat = "sf_coordinates"
  ) +
  scale_size_continuous(name = "") +
  scale_colour_manual(name = 'Candidato', values = setNames(c('red','blue'),c("Lula", "Bozo"))) +
  theme_void() + coord_sf() + 
  guides(
    colour = guide_legend("Candidato"),
    size = guide_legend("Vantagem"),
    shape = guide_legend("Vantagem")) + 
  labs(title='Votação Presidencial - 2º Turno',
       subtitle = "Vantagens dos canditados") +
  theme(
    legend.position = c(0.1, 0.25), legend.text = element_text(size = 12),
    text = element_text(color = '#22211d', size = 10),
    plot.background = element_rect(fill = '#f5f5f2', color = NA), 
    panel.background = element_rect(fill = '#f5f5f2', color = NA), 
    legend.background = element_rect(fill = '#f5f5f2', color = NA),
    plot.title = element_text(size= 16, hjust=0.1, color = '#2e2d27',
                              face = 'bold'),
  )