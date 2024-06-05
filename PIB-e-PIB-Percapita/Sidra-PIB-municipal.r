#__________________________________________________________________________________________________

# Análise PIB 2020 - Municípios

# Autor: Alexandre Barros
# Email: alexandrepitstop@gmail.com

# --/12/2022
#__________________________________________________________________________________________________

# Lista de pacotes necessários
pacotes_necessarios <- c("geobr", "tidyverse", "sidrar")

# Verificar e instalar pacotes que não estão instalados
for (pacote in pacotes_necessarios) {
  if (!require(pacote, character.only = TRUE)) {
    install.packages(pacote)
    library(pacote, character.only = TRUE)
  }
}


# Pacotes
library(sidrar)
library(tidyverse)
library(geobr)
library(ggplot2)

# Importação dos dados do PIB dos municípios no estado do Piauí (PI)
df_pib_muni <- sidrar::get_sidra(5938,
                            variable = 37,
                            period = c('2021', '2020', '2019'),
                            geo = "City",
                            geo.filter = list("State" = 22))

# Importação dos dados do PIB do estado do Piauí (PI)
df_pib_state <- sidrar::get_sidra(5938,
                                 variable = 37,
                                 period = c('2021', '2020', '2019'),
                                 geo = "State",
                                 geo.filter = list("State" = 22))

head(df_pib_muni)
head(df_pib_state)


# PIB por Estado
pib_estados <- sidrar::get_sidra(api = "/t/5938/n1/all/v/37/p/2019,2020,2021/c11255/2")

# Emprego por Região
emprego_regioes <- sidrar::get_sidra(api = "/t/1620/n7/all/v/all/p/last%203/c1/all")

# Desocupação por Região
desocupacao_regioes <- sidrar::get_sidra(api = "/t/6399/n7/all/v/all/p/last%205/c1/all")

# IPCA por Região
ipca_regioes <- sidrar::get_sidra(api = "/t/1737/n7/all/v/63/p/last%2012/c315/7169,7170,7171,7172,7173,7174")

# Rendimentos por Região
rendimentos_regioes <- sidrar::get_sidra(api = "/t/6409/n7/all/v/all/p/last%205/c1/all")



