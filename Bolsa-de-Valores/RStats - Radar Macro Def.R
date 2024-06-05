#_______________________________________________________________________________
# Preparação de Dados para Análise Semanal das Bolsas de Valores
# Autor: Alexandre Barros
# Data: ~/01/2023
#_______________________________________________________________________________

# Pacotes ----------------------------------------------------------------------
library(tidyverse)
library(yfR)

# Configuração de diretório
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#-- Variáveis-níveis

semana <- c("segunda-feira", "terça-feira", "quarta-feira", "quinta-feira", "sexta-feira")
bolsas <- c("Ibovespa", "S&P 500", "Dow Jones", "FTSE100", "DAX", "Xangai/SSEC")

#-- Argumentos da função

indices <- c(
   "^BVSP"    # ! Ibovespa
,  "^GSPC"    # ! S&P 500
,  "^DJI"     # ! Dow Jones
,  "^FTSE"    # ! FTSE100
,  "^GDAXI"   # ! DAX
,  "000001.SS"# ! Xangai/SSEC
)

data_inicial <- as.Date("2023-07-07")
data_final <- as.Date("2023-07-14")

#-- Importando a base de dados
df_yf <- yf_get(tickers = indices, 
                first_date = data_inicial,
                last_date = data_final)

unique(df_yf$ticker)

#-- DF para o gráfico

df_sum <- df_yf %>% 
  mutate(
    Bolsa = case_when(
       ticker == "^BVSP" ~ "Ibovespa"
,      ticker == "^GSPC" ~ "S&P 500"
,      ticker == "^DJI"  ~ "Dow Jones"
,      ticker == "^FTSE" ~ "FTSE100"
,      ticker == "^GDAXI"~ "DAX"
,      ticker == "000001.SS" ~ "Xangai/SSEC"
),
    Valor = round(ret_closing_prices, 4),
    Dia = factor(weekdays(ref_date), levels = semana),
    
#-- Condicionais  
    vjust = ifelse(Valor > 0, 1.3, -0.5),
    colour = ifelse(Valor < 0, F, T),
    Bolsa = factor(Bolsa, levels = bolsas)
  )

# Salvando os dados processados ------------------------------------------------
df_sum
write.csv(df_sum, "data/processed_stock_data.csv")
