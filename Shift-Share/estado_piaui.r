#> Author: Alexandre Barros
#> Date: 25 nov 2022
#> Abstract: regional economics application on location indicators


#> Pacotes -------------------------------------------------------------------

 libs <- c("mapview", "sf" , "sidrar" , "magrittr", "basedosdados", "bigrquery")
 libs.novas <- libs[ !( libs %in% installed.packages()[ , "Package" ] ) ]
 if( length( libs.novas ) ) install.packages( libs.novas )


# carregando dependências 
library(tidyverse)
library(magrittr)
library(sidrar)
library(mapview)
library(sf)
library(bigrquery)


#> Arquivo shape -------------------------------------------------------------
#> informações de geração de figuras geográficas

temp_shapefile <- tempfile()
download.file("https://www.ipea.gov.br/ipeageo/arquivos/malhas/PI_Mun97_region.zip", temp_shapefile)
unzip(temp_shapefile)

# Lendo o arquivo
sf_piauí <- read_sf('PI_Mun97_region.shp', 
                     options = 'ENCODING=WINDOWS-1252')


#> Testando visualização -----------------------------------------------------

mapview(sf_piauí["AREA_97"], col.regions = sf.colors(10))


#> Dados da população --------------------------------------------------------
pop <- get_sidra(6579,
                 variable = '9324',
                 geo = "State",
                 period = as.character(2002:2021)
) %>% select(5:8)

#> Nomes das variáveis
names(pop)[] <- c("população", 'code_state', 'uf', 'ano')

#> Dados do emprego RAIS ------------------------------------------------------

# ID do projeto do Google Cloud
projectid = "composite-cable-369715"

# Query de solicitação da base de dados
sql <- "SELECT ano, sigla_uf,id_municipio,  vinculo_ativo_3112, tipo_vinculo, cnae_2
        FROM `basedosdados.br_me_rais.microdados_vinculos`
        WHERE ano BETWEEN 2002 AND 2020
        AND vinculo_ativo_3112 = 1
        AND sigla_uf = 'PI'" # usar 'BETWEEN ... AND' para inserir intervalos

# Executando o query, no Id do projeto
tb <- bq_project_query(projectid, sql)

# Lendo/importando a tabela
sample <- bq_table_download(tb)


#> OPCIONAL: arquivo do dicionário ---------------------------------------------
sql_dicio <- bq_project_query(projectid, "SELECT * FROM `basedosdados.br_me_rais.dicionario`")
dicionario <- bq_table_download(sql_dicio)
#-------------------------------------------------------------------------------
