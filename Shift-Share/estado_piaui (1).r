#> Author: Alexandre Barros
#> Date: 25 nov 2022
#> Abstract: regional economics application on location indicators
#> whith references Figueiredo (2020)


#> Pacotes -------------------------------------------------------------------
 libs <- c("mapview", "sf" , "sidrar" , "magrittr", "basedosdados", "bigrquery", 
           "collapse", "ggspatial", "tmap")
 libs.novas <- libs[ !( libs %in% installed.packages()[ , "Package" ] ) ]
 if( length( libs.novas ) ) install.packages( libs.novas )


# carregando dependências 
library(tidyverse)
library(magrittr)
library(sidrar)
library(mapview)
library(sf)
library(bigrquery)
library(tmap)
library(ggspatial)


#> Arquivo shape -------------------------------------------------------------
#> informações de geração de figuras geográficas

temp_shapefile <- tempfile()
download.file("https://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_2021/UFs/PI/PI_Municipios_2021.zip", temp_shapefile)
unzip(temp_shapefile)


# Lendo o arquivo
sf_piauí <- read_sf('PI_Municipios_2021.shp', 
                     options = 'ENCODING=UTF-8')


#> Testando visualização -----------------------------------------------------
mapview(sf_piauí["AREA_KM2"], col.regions = sf.colors(10))


#> Dados da população --------------------------------------------------------
pop <- get_sidra(6579,
                 variable = '9324',
                 geo = "State",
                 period = as.character(2002:2021)
) %>% select(5:8)


#> Nomes das variáveis
names(pop)[] <- c("população", 'code_state', 'uf', 'ano')


#> DADOS DO EMPREGO RAIS -------------------------------------------------------

# ID do projeto do Google Cloud
projectid = "composite-cable-369715"


# Query de solicitação da base de dados
sql <- "SELECT ano, sigla_uf,id_municipio,  vinculo_ativo_3112, tipo_vinculo, cnae_2
        FROM `basedosdados.br_me_rais.microdados_vinculos`
        WHERE ano BETWEEN 2002 AND 2020
        AND vinculo_ativo_3112 = 1
        AND sigla_uf = 'PI'" # usar 'BETWEEN ~. AND' para inserir intervalos


# executando o query, no Id do projeto
tb <- bq_project_query(projectid, sql)


# lendo/importando a tabela
dados_rais <- bq_table_download(tb)

#> CLASSIFICANDO OS SETORES ----------------------------------------------------
#> conforme classificação da CNAE 2.0

dados_rais %<>% 
  mutate(
    ver = substr(cnae_2, 1, 2),
    Setor = case_when(
      ver =='01' ~ 'AGRICULTURA, PECUÁRIA, PRODUÇÃO FLORESTAL, PESCA E AQÜICULTURA',
      ver =='05' ~ 'INDÚSTRIAS EXTRATIVAS',
      ver =='10' ~ 'INDÚSTRIAS DE TRANSFORMAÇÃO',
      ver =='35' ~ 'ELETRICIDADE E GÁS',
      ver =='36' ~ 'ÁGUA, ESGOTO, ATIVIDADES DE GESTÃO DE RESÍDUOS E DESCONTAMINAÇÃO',
      ver =='41' ~ 'CONSTRUÇÃO',
      ver =='45' ~ 'COMÉRCIO; REPARAÇÃO DE VEÍCULOS AUTOMOTORES E MOTOCICLETAS',
      ver =='49' ~ 'TRANSPORTE, ARMAZENAGEM E CORREIO',
      ver =='55' ~ 'ALOJAMENTO E ALIMENTAÇÃO',
      ver =='58' ~ 'INFORMAÇÃO E COMUNICAÇÃO',
      ver =='64' ~ 'ATIVIDADES FINANCEIRAS, DE SEGUROS E SERVIÇOS RELACIONADOS',
      ver =='68' ~ 'ATIVIDADES IMOBILIÁRIAS',
      ver =='69' ~ 'ATIVIDADES PROFISSIONAIS, CIENTÍFICAS E TÉCNICAS',
      ver =='77' ~ 'ATIVIDADES ADMINISTRATIVAS E SERVIÇOS COMPLEMENTARES',
      ver =='84' ~ 'ADMINISTRAÇÃO PÚBLICA, DEFESA E SEGURIDADE SOCIAL',
      ver =='85' ~ 'EDUCAÇÃO',
      ver =='86' ~ 'SAÚDE HUMANA E SERVIÇOS SOCIAIS',
      ver =='90' ~ 'ARTES, CULTURA, ESPORTE E RECREAÇÃO',
      ver =='94' ~ 'OUTRAS ATIVIDADES DE SERVIÇOS',
      ver =='97' ~ 'SERVIÇOS DOMÉSTICOS',
      ver =='99' ~ 'ORGANISMOS INTERNACIONAIS E OUTRAS INSTITUIÇÕES EXTRATERRITORIAIS',
      T ~ "Outro"
    )
  ) %>% select(-ver)

# consolidando a base
rais.sum <- dados_rais %>% 
  group_by(ano, id_municipio, Setor) %>%
  summarise(n_vinculos = n())

#> OPCIONAL: arquivo do dicionário ---------------------------------------------
sql_dicio <- bq_project_query(projectid, "SELECT * 
                                          FROM `basedosdados.br_me_rais.dicionario`")
dicionario <- bq_table_download(sql_dicio)
#-------------------------------------------------------------------------------


# ! INDICADORES !


#> QUOCIENTE LOCACIONAL (QL) ---------------------------------------------------
#> aplicação e cálculo

#> chamar os dados para o QL, E<-sum(ei) # ei é o emprego total da regiao i, 
#> E é o emprego total no Estado Ek<-sum(eki) # Ek é o emprego total do setor k

QL <- function(e_ki, e_i, e_k, e) {
  
  s_ki <- e_ki/e_i
  s_i <- e_k/e
  LQ <- s_ki/s_i
  
  return(LQ)
}

# ! dados 2019
rais_QL <- filter(rais.sum, ano == 2019)

# ! cálculo QL

rais_QL %<>% mutate(
  df_e_ki = n_vinculos,
  df_e_i = tapply(n_vinculos, FUN=sum, INDEX = id_municipio),
  df_E = sum(rais_QL$n_vinculos)
) 

# ! df_e_k separado
t <- collapse::fsum(rais_QL$n_vinculos, rais_QL$Setor) %>% as.data.frame() %>%
     tibble::rownames_to_column("Setor")
names(t)[2] <- "df_e_k"

# ! consolidando as bases 
rais_QL %<>% 
  left_join(t, by = "Setor") %>% select(id_municipio, Setor, 5:8)

# ! calculando o QL
rais_QL %<>%
  mutate(
    QL = QL(df_e_ki, df_e_i, df_e_k, df_E)
  )

# ! keep !
gdata::keep(rais_QL, rais.sum, sf_piauí, sure = T)


#> COEFICIENTE DE LOCALIZAÇÃO (CL) ---------------------------------------------
#> aplicação e cálculo

CL <- function(e_ki, e_i, e_k, e) {
  
  s_ki <- e_ki/e_k
  s_i <- e_i/e
  CL <- sum(.5 * abs(s_ki - s_i))
  
  return(CL)
}

# base inicial
rais_CL <- filter(rais.sum, ano == 2019)

# ! FIGURAS E MAPAS ! 

#> CONSOLIDANDO A BASE ---------------------------------------------------------

# juntando as bases
rais_QL %<>% merge(sf_piauí, by.x='id_municipio', by.y = 'CD_MUN')

# base filtro para o setor analisado no mapa
dataset_T1 <- filter(rais_QL, Setor == 'ADMINISTRAÇÃO PÚBLICA, DEFESA E SEGURIDADE SOCIAL')


#> MAPPING ---------------------------------------------------------------------
plot = ggplot() +
  geom_sf(data=dataset_T1, aes(fill = QL, geometry = geometry), size=.15)+
  
  labs(title = "QL 2019 dos Municípios de PI para Setor Público", 
       caption = "Fonte: Elaboração própria", size = 8) + 
  scale_fill_distiller(palette = "RdGy", limits = c(0, 3), name = "QL Piauí") + 
  theme_minimal() +
  # componentes do mapa 
  ggspatial::annotation_north_arrow(location = "bl", 
                                    which_north = "true", 
                                    pad_x = unit(0.65, "in"), 
                                    pad_y = unit(0.3, "in"), 
                                    style = north_arrow_fancy_orienteering) + 
  ggspatial::annotation_scale(location = "bl", width_hint = 0.3)

plot  


#> MAPPING II ------------------------------------------------------------------
dataset_T1 = st_as_sf(dataset_T1)

mapview(dataset_T1["QL"], col.regions = sf.colors(10), fgb = FALSE)


#> MAPPING III -----------------------------------------------------------------
tm_shape(dataset_T1) +
  tm_polygons(col = "QL", palette = "-viridis")