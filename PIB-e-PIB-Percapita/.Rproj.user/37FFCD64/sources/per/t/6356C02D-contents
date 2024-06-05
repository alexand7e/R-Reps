library(dplyr)
library(ggplot2)

# Configuração de diretório
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source('./Sidra-PIB-municipal.r')

################################################################################

# Gráfico de linha para a evolução do PIB de todos os municípios
ggplot(df_pib_muni, aes(x = as.factor(Ano), y = Valor, group = Município, color = Município)) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  labs(title = "Evolução do PIB ao Longo dos Anos para Todos os Municípios",
       x = "Ano",
       y = "PIB (em Mil Reais)") +
  theme_minimal() +
  theme(legend.position = "none")

################################################################################

# Preparando dados para os top 5 municípios em cada ano
top_municipios_por_ano <- df_pib_muni %>%
  group_by(Ano) %>%
  slice_max(order_by = Valor, n = 5) %>%
  ungroup()

# Gráfico de barras para os top 5 municípios em cada ano
ggplot(top_municipios_por_ano, aes(x = reorder(Município, Valor), y = Valor, fill = as.factor(Ano))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~Ano) +
  labs(title = "Top 5 Municípios por PIB em Cada Ano",
       x = "Município",
       y = "PIB (em Mil Reais)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")


################################################################################

df_pib_state <- df_pib_state %>%
  mutate(Município = "Total Piauí", Type = "Estado")

pib_comparative <- bind_rows(subset(df_pib_muni, Município == random_municipio$Município), df_pib_state)

# Criando o gráfico
ggplot(pib_comparative, aes(x = as.factor(Ano), y = Valor, group = Município, color = Município)) +
  geom_line() +
  geom_point() +
  facet_wrap(~Type, scales = "free_y") +
  labs(title = "Comparação do PIB entre Municípios e o Total do Estado do Piauí",
       x = "Ano",
       y = "PIB (em Mil Reais)") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())

################################################################################


# Selecionando aleatoriamente um município
random_municipio <- df_pib_muni %>% 
  sample_n(1)  # Seleciona 1 município aleatoriamente

# Verificando qual município foi escolhido
print(random_municipio$Município)

# Filtrando dados para o município selecionado
selected_municipio_pib <- df_pib_muni %>% 
  filter(Município == random_municipio$Município)

# Gráfico de linha para evolução do PIB
ggplot(selected_municipio_pib, aes(x = as.factor(Ano), y = Valor, group = 1)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "red", size = 3) +
  labs(title = paste("Evolução do PIB de", random_municipio$Município, "(2019-2021)"),
       x = "Ano",
       y = "PIB (em Mil Reais)") +
  theme_minimal()
