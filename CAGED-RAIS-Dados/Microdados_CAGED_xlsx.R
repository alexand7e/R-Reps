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
library(lubridate)
library(readxl)

# configurando local
setwd("C:/Users/Alexandre/OneDrive/Documentos/R/Projeto CAGED")
caminho.file = "C:/Users/Alexandre/OneDrive/Documentos/R/Projeto CAGED/Files/"
nmes = 8
data_mes_atual <- as.Date(str_glue("2023-0{nmes}-01"))
data_janeiro <- as.Date(str_glue("2023-01-01"))
data_mes_passado <- as.Date(str_glue("2023-07-01"))


#_______________________________________________________________________________
# Tabelas complementares
{
# Leitura dos dados dos territórios e seleção das colunas relevantes
d_territorios <- read_xlsx("C:/Users/Alexandre/Downloads/d_municipio_territorio.xlsx") %>%
  select(1:3)

# Leitura dos dados dos setores
d_setores <- read_xlsx("dicionário_caged.xlsx", sheet = "subclasse")

# Leitura dos dados das unidades federativas
d_unidade_federativa <- read_xlsx("dicionário_caged.xlsx", sheet = "uf")
names(d_unidade_federativa)[2] <- "UF"

# Leitura dos dados dos municípios
d_municipios <- read_xlsx("dimensao_municipios.xlsx", col_types = "text") %>%
  select(-UF) %>%
  mutate(Cód_UF = as.integer(substr(IBGE, 1, 2)))

# Combinar dados dos municípios com dados das unidades federativas
d_municipios <- merge(d_municipios, d_unidade_federativa, by.x = "Cód_UF", by.y = "Código")
}

#_______________________________________________________________________________
# arquivos-base

# Leitura do arquivo "Tabela_original.xlsx"
caged_geral <- read_xlsx("C:/Users/Alexandre/OneDrive/Documentos/R/Projeto CAGED/Files/Tabela_processada_agosto.xlsx",
                         sheet = "Tabela 8.1",
                         skip = 5,
                         col_types = "numeric")

# cópia do dataframe original
caged_geral %<>% mutate(...2 = as.character(...2))


# Selecione as colunas desejadas e renomeie
caged_geral <- caged_geral %>%
  select(-starts_with("Vari"))
names(caged_geral)[1:3] <- c("uf","município","n_município")

# Remova as linhas finais não desejadas
caged_geral <- caged_geral[1:(nrow(caged_geral) - 8), 1:(ncol(caged_geral) - 6)]

#  variáveis para o loop
i <- 1
inicial <- 4
final <- 8
n_repeticoes <- 36 + nmes  # O número total de repetições

# Vetor com os nomes das colunas
nomes_colunas <- c("Estoque", "Admissões", "Desligamentos", "Saldo")

# Loop para renomear colunas com prefixo
while (i <= n_repeticoes) {
  nomes_com_prefixo <- paste0(i, "_", nomes_colunas)
  
  # Renomeie as colunas do próximo intervalo
  colnames(caged_geral)[inicial:final] <- nomes_com_prefixo
  
  # Atualize valores de inicial, final e i para a próxima iteração
  final <- final + length(nomes_colunas)
  inicial <- inicial + length(nomes_colunas)
  i <- i + 1
}

  

#_______________________________________________________________________________
# base formatada para uso

caged_geral %<>% select(-1,-3)
# Substituir valores NA por 0 se necessário
caged_geral[is.na(caged_geral)] <- 0.0
caged_geral$município[is.na(caged_geral$município)] <- "Desconhecido"
caged_geral %<>% filter(município != "Desconhecido")

# Encontre as colunas que você deseja ajustar (a partir da 4ª coluna)


# Formatar o dataframe caged_geral
caged_geral_formatado <- caged_geral %>% 
  pivot_longer(names_to = "Cat", values_to = "Valor", cols = 2:(length(caged_geral))) %>%
  mutate(
    ndata = as.numeric(str_split(Cat, "_", simplify = TRUE)[, 1]),
    Ano = floor((ndata - 1) / 12) + 2020, 
    Mes = (ndata - 1) %% 12 + 1, 
    Data = as.Date(paste0(Ano, "-", sprintf("%02d", Mes), "-01")),
    Categoria = str_split(Cat, "_", simplify = TRUE)[, 2],
    Valor = round(Valor, 2) # 
  ) %>%
  select(-"Cat", -"ndata", -"Ano", -"Mes")


caged_formatado = caged_geral_formatado %>%
    pivot_wider(names_from = "Categoria", values_from = "Valor") %>%
    arrange(município, Data) %>% 
    group_by(município) %>% 
    mutate(Estoque_mes_anterior = lag(Estoque)) %>% 
    ungroup() %>% # 
    mutate(Variação = round((Saldo / Estoque_mes_anterior) * 100, 2))


# consolidação da base geral
  caged_formatado_completo = caged_formatado %>% 
    merge(d_municipios, all = T, by.x = "município", by.y = "IBGE") 

  caged_formatado_completo[is.na(caged_formatado_completo)] <- 0
  
#_______________________________________________________________________________
# arquivos-base
  
{
caged_territórios_julho = caged_formatado_completo %>%
    filter(Cód_UF == 22,
           Data == data_mes_passado) %>%
    merge(d_territorios, by.x = "IBGE7", by.y = "codibge", all.x = T) %>%
    group_by (territorio) %>%
    summarise(
      Estoque_anterior = sum(Estoque, na.rm = T)
    )   

caged_territórios_inicial = caged_formatado_completo %>%
    filter(Cód_UF == 22,
           Data == data_janeiro) %>%
    merge(d_territorios, by.x = "IBGE7", by.y = "codibge", all.x = T) %>%
    group_by (territorio) %>%
    summarise(
      Estoque_anterior = sum(Estoque, na.rm = T)
    ) 

caged_municípios_inicial = caged_formatado_completo %>%
  filter(Data == data_janeiro) %>%
  group_by (Município) %>%
  summarise(
      Estoque_anterior = sum(Estoque, na.rm = T)
  ) 

caged_municípios_julho = caged_formatado_completo %>%
  filter(Data == data_mes_passado) %>%
  group_by (Município) %>%
  summarise(
    Estoque_anterior = sum(Estoque, na.rm = T)
  ) 


caged_estado_inicial = caged_formatado_completo %>%
  filter(Data == data_janeiro) %>%
  group_by (UF) %>%
  summarise(
    Estoque_anterior = sum(Estoque, na.rm = T)
  ) 

caged_estado_julho = caged_formatado_completo %>%
  filter(Data == data_mes_passado) %>%
  group_by (UF) %>%
  summarise(
    Estoque_anterior = sum(Estoque, na.rm = T)
  ) 

caged_regiao_inicial = caged_formatado_completo %>%
  filter(Data == data_janeiro) %>%
  group_by (Região) %>%
  summarise(
    Estoque_anterior = sum(Estoque, na.rm = T)
  ) 

caged_regiao_julho = caged_formatado_completo %>%
  filter(Data == data_mes_passado) %>%
  group_by (Região) %>%
  summarise(
    Estoque_anterior = sum(Estoque, na.rm = T)
  ) 
}

#_______________________________________________________________________________
# arquivos-base
  
  
{ # executar todas as bases

#-- base dos territórios do mês
{
caged_territórios_agosto = caged_formatado_completo %>%
  filter(Cód_UF == 22,
         Data == data_mes_atual) %>%
  merge(d_territorios, by.x = "IBGE7", by.y = "codibge", all.x = T) %>%
  group_by (territorio) %>%
  summarise(
    
    Estoque = sum(Estoque, na.rm = T),
    Admissões = sum(Admissões, na.rm = T),
    Desligamentos = sum(Desligamentos, na.rm = T),
    Saldo = sum(Saldo, na.rm = T)
    
  ) %>% 
  merge(caged_territórios_julho, by = "territorio") %>%
  mutate(Variação = round(Estoque/Estoque_anterior - 1, 6)*100) %>%
 # select(-Estoque_anterior) %>%
  arrange(desc(Variação)) 
  
# Calcular a soma de todas as colunas
total <- colSums(caged_territórios_agosto[, 2:ncol(caged_territórios_agosto)], na.rm = TRUE)

# Converter o resultado em uma linha
total_df <- data.frame(territorio = "Total", 
                       t(total))

# Adicionar a linha de total ao seu_dataframe
caged_territórios_agosto %<>% rbind(total_df)

} 

#-- territórios no acumulado do ano
{
caged_territórios_2023 = caged_formatado_completo %>%
    filter(Cód_UF == 22,
           Data >= data_janeiro) %>%
    merge(d_territorios, by.x = "IBGE7", by.y = "codibge") %>%
    group_by (territorio) %>%
    summarise(
      Admissões = sum(Admissões, na.rm = T),
      Desligamentos = sum(Desligamentos, na.rm = T),
      Saldo = sum(Saldo, na.rm = T)
    ) %>% 
    
    merge(caged_territórios_inicial, by = "territorio") %>%
    mutate(Variação = round(Saldo/Estoque_anterior, 6)*100) %>%
    #select(-Estoque_anterior) %>%
    arrange(desc(Variação)) 
  
# Calcular a soma de todas as colunas
total <- colSums(caged_territórios_2023[, 2:ncol(caged_territórios_2023)], na.rm = TRUE)
  
total_df <- data.frame(territorio = "Total", 
                         t(total))

caged_territórios_2023 %<>% rbind(total_df)
}
  

# municípios do Estado do mês de referência
{
caged_municípios_pi_agosto = caged_formatado_completo %>%
  filter(#Cód_UF == 22,
         Data == data_mes_atual) %>%
  group_by (Município) %>%
  summarise(
    
    Estoque = sum(Estoque, na.rm = T),
    Admissões = sum(Admissões, na.rm = T),
    Desligamentos = sum(Desligamentos, na.rm = T),
    Saldo = sum(Saldo, na.rm = T),
    Variação = mean(Variação, na.rm = T)
    
  ) %>% arrange(desc(Saldo)) %>% head(25)

}

  
  
# municípios do Estado do acumulado do ano]
{
  
caged_municípios_pi_2023_I = caged_formatado_completo %>%
  filter(Cód_UF == 22,
         Data >= data_janeiro) %>%
  rename("Municípios com mais admissões" = Município) %>%
  group_by (.$"Municípios com mais admissões") %>%
  summarise(
    #Admissões = sum(Admissões, na.rm = T),
    #Desligamentos = sum(Desligamentos, na.rm = T),
    Saldo_I = sum(Saldo, na.rm = T), 
  ) %>% arrange(desc(Saldo_I)) %>% head(10)
  
caged_municípios_pi_2023_II = caged_formatado_completo %>%
    filter(Cód_UF == 22,
           Data >= data_janeiro) %>%
    rename("Municípios com mais desligamentos" = Município) %>%
    group_by (.$"Municípios com mais desligamentos") %>%
    summarise(
      #Admissões = sum(Admissões, na.rm = T),
      #Desligamentos = sum(Desligamentos, na.rm = T),
      Saldo_II = sum(Saldo, na.rm = T),
  ) %>% arrange(Saldo_II) %>% head(10)

caged_maiores_saldos = bind_cols(caged_municípios_pi_2023_I, caged_municípios_pi_2023_II)
}

{
caged_estados_agosto = caged_formatado_completo %>%
  filter(Data == data_mes_atual) %>%
  group_by (Região,UF) %>%
  summarise(
    
    Estoque = sum(Estoque, na.rm = T),
    Admissões = sum(Admissões, na.rm = T),
    Desligamentos = sum(Desligamentos, na.rm = T),
    Saldo = sum(Saldo, na.rm = T)
    
  ) %>% 
    merge(caged_estado_julho, by = "UF") %>%
    mutate(Variação = round((Estoque)/Estoque_anterior-1,4)*100) %>%
    arrange(desc(Variação)) %>%
    mutate(Posição = 1:n())
  
# Calcular a soma de todas as colunas
total <- colSums(caged_estados_agosto[, 3:ncol(caged_estados_agosto)], na.rm = TRUE)
  
total_df <- data.frame(Região = "Total", 
                       UF =     "Total", 
                       t(total))
  
caged_estados_agosto %<>% rbind(total_df)
}

{
caged_estados_2023 = caged_formatado_completo %>%
  filter(Data >= data_janeiro) %>%
  group_by (UF) %>%
  summarise(
    Admissões = sum(Admissões, na.rm = T),
    Desligamentos = sum(Desligamentos, na.rm = T),
    Saldo = sum(Saldo, na.rm = T),
  ) %>% 
    merge(caged_estado_inicial, by = "UF") %>%
    mutate(Variação = round(Saldo/Estoque_anterior,6)*100) %>%
    arrange(desc(Variação)) %>%
    mutate(Posição = 1:n())
  
# Calcular a soma de todas as colunas
total <- colSums(caged_estados_2023[, 2:ncol(caged_estados_2023)], na.rm = TRUE)
  
total_df <- data.frame(UF = "Total", 
                       t(total))
  
caged_estados_2023 %<>% rbind(total_df)
}


  {
    caged_regiao_2023 = caged_formatado_completo %>%
      filter(Data >= data_janeiro) %>%
             #UF != "Não identificado") %>%
      group_by (Região) %>%
      summarise(
        Admissões = sum(Admissões, na.rm = T),
        Desligamentos = sum(Desligamentos, na.rm = T),
        Saldo = sum(Saldo, na.rm = T),
      ) %>% 
      merge(caged_regiao_inicial, by = "Região") %>%
      mutate(Variação = round(Saldo/Estoque_anterior,6)*100) %>%
      arrange(desc(Variação)) %>%
      mutate(Posição = 1:n())
    
    # Calcular a soma de todas as colunas
    total <- colSums(caged_regiao_2023[, 2:ncol(caged_regiao_2023)], na.rm = TRUE)
    
    total_df <- data.frame(Região = "Total", 
                           t(total))
    
    caged_regiao_2023 %<>% rbind(total_df)
  }  
  
  {
    caged_regiao_agosto = caged_formatado_completo %>%
      filter(Data == data_mes_atual) %>%
      group_by (Região) %>%
      summarise(
        
        Estoque = sum(Estoque, na.rm = T),
        Admissões = sum(Admissões, na.rm = T),
        Desligamentos = sum(Desligamentos, na.rm = T),
        Saldo = sum(Saldo, na.rm = T)
        
      ) %>% 
      merge(caged_regiao_julho, by = "Região") %>%
      mutate(Variação = round((Estoque)/Estoque_anterior-1,6)*100) %>%
      arrange(desc(Variação)) %>%
      mutate(Posição = 1:n())
    
    # Calcular a soma de todas as colunas
    total <- colSums(caged_regiao_agosto[, 2:ncol(caged_regiao_agosto)], na.rm = TRUE)
    
    total_df <- data.frame(Região = "Total", 
                           t(total))
    
    caged_regiao_agosto %<>% rbind(total_df)
  }

caged_histórico = caged_formatado_completo %>%
  filter(Cód_UF == 22) %>%
  #mutate(Data) %>%
  group_by (Data) %>%
  summarise(
    Estoque = sum(Estoque, na.rm = T),
    Saldo = sum(Saldo, na.rm = T),
  ) 

}
# mutate(Território = paste0(territorio, " (", Saldo, ")"))

# tabelas finais consolidadas 



# salvando bases consolidadas

dataset_names <- list("caged_piaui_julho"          = caged_municípios_pi_agosto, 
                      "caged_territórios_agosto"   = caged_territórios_agosto, 
                      "caged_territórios_2023"     = caged_territórios_2023, 
                      "caged_municípios_pi"        = caged_maiores_saldos,
                      "caged_estados_AGOSTO"       = caged_estados_agosto,
                      "caged_estados_2023"         = caged_estados_2023,
                      "caged_histórico"            = caged_histórico)
dataset_names_regiao <- list(
  "Região_agosto" = caged_regiao_agosto,
  "Regiao_2023" = caged_regiao_2023
)

dataset_names_acumulados <- list(
  "estados"    = caged_estados_2023,
  "regiao"     = caged_regiao_2023,
  "territorios"= caged_territórios_2023
) %>%

openxlsx::write.xlsx(file = "Tabelas - acumulados - 2023.2.xlsx") 

# exportanto para excel
openxlsx::write.xlsx(dataset_names, file = "Tabelas - CAGED - agosto.xlsx") 
"caged_estados_2023"         = caged_estados_2023
openxlsx::write.xlsx(caged_estados_2023, "caged_estados_2023.xlsx") 
openxlsx::write.xlsx(caged_estados_agosto, "caged_estados_agosto.xlsx") 

openxlsx::write.xlsx(caged_histórico, "histórico - caged - piauí.xlsx") 

sum(caged_estado_inicial$Estoque_anterior)
sum(caged_estado_julho$Estoque_anterior)
