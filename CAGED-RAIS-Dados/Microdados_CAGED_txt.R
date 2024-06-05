#__________________________________________________________________________________________________

# Processamento dos microdados do CAGED

# Autor: Alexandre Barros
# Email: alexandrepitstop@gmail.com

# --/09/2023
#__________________________________________________________________________________________________


#> Pacotes -------------------------------------------------------------------
libs <- c("DBI", "RSQLite", "tidyverse", "geobr" , "magrittr" , "magrittr", "readxl", "writexl", "archive")
libs.novas <- libs[ !( libs %in% installed.packages()[ , "Package" ] ) ]
if( length( libs.novas ) ) install.packages( libs.novas )


# carregando dependências 
library(tidyverse)
library(magrittr)

# configurando local
setwd('C:/Users/Alexandre/OneDrive/Documentos/R/Projeto CAGED')
caminho.file = 'C:/Users/Alexandre/OneDrive/Documentos/R/Projeto CAGED/Files/'
nmes = 8

# arquivo-base
filename <- str_glue("CAGEDMOV20230{nmes}.txt")
file_path <- file.path(caminho.file, filename)

caged_agosto <- select(read.table(file_path, sep = ";", header = TRUE, colClasses = "character"),
                      c(1:7, 10, 11, 13, 14))

# bases complementares 
  d_territorios = readxl::read_xlsx('C:/Users/Alexandre/Downloads/d_municipio_territorio.xlsx') %>% select(1:3)
  
  d_setores = readxl::read_xlsx("dicionário_caged.xlsx", sheet = "subclasse",
                                col_types = 'text')
  
  d_unidade_federativa = readxl::read_xlsx("dicionário_caged.xlsx", sheet = "uf")
  names(d_unidade_federativa)[2] = 'UF'
  
  d_municipios = readxl::read_xlsx("dimensao_municipios.xlsx", 
                                   col_types = 'text') %>%
                        select(-UF) %>%
                        mutate(Cód_UF = as.integer(substr(IBGE, 1, 2))) %>%
                        merge(d_unidade_federativa, by.x = 'Cód_UF', by.y = 'Código')


#-- rotina atualização do estoque

if (T) {
  
  i <- 1
  max <- 7
  estoque_atualizado <- NULL  # Inicialize a variável antes do loop
  
  while (i <= max) {
    assign(str_glue("cg_mes_{sprintf('%02d', i)}"),
           base_caged %>%
             filter(substr(competênciamov,6,6) == i) %>%
             mutate(saldomovimentação = as.numeric(saldomovimentação)) %>%
             group_by(município) %>%
             summarise(
              !!str_glue("{i}_Admissões") := sum(saldomovimentação[saldomovimentação ==  1]),
              !!str_glue("{i}_Desligamentos") := sum(saldomovimentação[saldomovimentação == -1]),
             ) %>%
             
             mutate(!!str_glue("{i}_saldo") := get(str_glue("{i}_Admissões")) + get(str_glue("{i}_Desligamentos"))) 
    )
    
    if (i == 1) {
      estoque_atualizado <- merge(estoque_referência, get(str_glue("cg_mes_{sprintf('%02d', i)}")), by = "município", all = T)
    } else {
      estoque_atualizado <- merge(estoque_atualizado, get(str_glue("cg_mes_{sprintf('%02d', i)}")), by = "município", all = T) 
    }
    
    i <- i + 1  # incrementar o contador
    if (i == 8) { 
      rm(list = ls(pattern = 'cg_mes_0'))
      
      # ajustando a coluna 'estoque'
      estoque_atualizado$estoque = as.numeric(estoque_atualizado$estoque)
      
      # todas as colunas que terminam com "saldo"
      colunas_saldo <- estoque_atualizado %>% select(ends_with('saldo'))
      
      # soma das colunas de saldo para cada linha
      estoque_atualizado$soma_saldo <- rowSums(colunas_saldo, na.rm = TRUE)
      
      # inserindo coluna 'estoque' à soma das colunas de saldo
      estoque_atualizado$soma_saldo <- estoque_atualizado$soma_saldo + estoque_atualizado$estoque
      }
  }

}


# dados dos tds
if (T) {
  
municípios_setores = caged_agosto %>%
  filter(uf == 22) %>%
  mutate(saldomovimentação = as.integer(saldomovimentação)) %>%
  group_by(município, subclasse) %>%
  summarise(
    Admissões     = sum(saldomovimentação[saldomovimentação ==  1]),
    Desligamentos = sum(saldomovimentação[saldomovimentação == -1]),
  ) %>%
  
  mutate(Saldo = Admissões + Desligamentos) 


# dados dos tds
municípios = caged_agosto %>%
  filter(uf == 22) %>%
  mutate(saldomovimentação = as.integer(saldomovimentação)) %>%
  group_by(município) %>%
  summarise(
    Admissões     = sum(saldomovimentação[saldomovimentação ==  1]),
    Desligamentos = sum(saldomovimentação[saldomovimentação == -1]),
  ) %>%
  
  mutate(Saldo = Admissões + Desligamentos)



municípios_setores %<>%
  group_by(município) %>%
  slice_max(order_by = Saldo, n = 1) %>%
  left_join(d_setores, by = c('subclasse' = 'Código'))

municípios %<>% arrange(desc(Saldo)) %>%
  head(25) %>%
  left_join(d_municipios, by = c('município' = 'IBGE')) %>%
  left_join(municípios_setores, by = 'município') %>%
  
  select(6, 7, 'Saldo.x', 'Saldo.y', 14)
}

  
writexl::write_xlsx(municípios, "municípios_e_setores.xlsx")
  
 
  
  
  
  
  

