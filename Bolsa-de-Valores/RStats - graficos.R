#_______________________________________________________________________________
# Análise e Visualização do Desempenho Semanal das Bolsas de Valores
# Autor: Alexandre Barros
# Data: /01/2023
#_______________________________________________________________________________

# Carregamento de pacotes ------------------------------------------------------
# install.packages("transformr")  # Dependência para interpolações suaves
# install.packages("gifski")  # Para renderizar GIFs
# install.packages("gifski")

library(tidyverse)
library(ggthemes)
library(gganimate)

# Configuração de diretório ----------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("./RStats - Radar Macro Def.R")

df_sum <- df_sum %>%
  mutate(day = case_when(
    ref_date == as.Date("2023-02-06") ~ 1,
    ref_date == as.Date("2023-02-07") ~ 2,
    ref_date == as.Date("2023-02-08") ~ 3,
    ref_date == as.Date("2023-02-09") ~ 4,
    ref_date == as.Date("2023-02-10") ~ 5
  )) %>% na.omit()

# Criação do gráfico principal -------------------------------------------------
df_sum2 <- filter(df_sum, ref_date == as.Date("2023-07-10"))

# -- plot
{
plot <- ggplot(df_sum2, aes(Bolsa, Valor, vjust = vjust)) +
  geom_bar(stat = "identity", aes(fill = colour), width = 0.5) +
  labs(x = "", y = "") +
  scale_x_discrete(breaks = NULL) +
  theme_bw() +
  ggtitle("") +
  geom_text(aes(label = Bolsa, y = 0),
            size = 3.6,
            fontface = "bold")

#-- Adicionando os rótulos

plot <- plot +
  geom_text(aes(label = scales::percent(Valor), 
                y = Valor),
              position = position_dodge(1.1),
              vjust = ifelse(df_sum2$Valor > 0, -1.05, 1.5),
              size = 3.3, 
              color = "#04225a",
              fontface = "bold")

#-- Alterando o tema

plot <- plot +
  scale_y_continuous(labels = NULL, breaks = NULL) +
  scale_fill_manual(values=c("#FF0000", "#00B04C")) +
  #scale_fill_manual(values=c( "#00B04C" , "#FF0000" )) +
  coord_cartesian(clip = "off") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        panel.border = element_blank(),
        strip.background = element_blank(),
        strip.text =  element_text(size = 13),
        legend.position = "none")

plot

}


#-- Salvando o gráfico em PNG
ggsave("data/radar5.png", plot, dpi = 200, height = 4.1, width = 5.3)


# Gráfico animado - Teste ------------------------------------------------------
df_sum$ref_date <- as.Date(df_sum$ref_date)

# Criando o gráfico animado
g <- ggplot(df_sum, aes(ref_date, price_close)) +
  geom_line(color = '#4364E8') +
  labs(x = 'Data', y = 'Preço de Fechamento', title = 'Ibov: Desempenho Semanal', subtitle = 'Bovespa',
       caption = 'Dados do Yahoo Finance') +
  theme_wsj() +
  gganimate::transition_reveal(ref_date)

# Verifique visualmente a animação no RStudio para confirmar que ela está funcionando
anim_save("data/grafico_animado.gif", animation = g)
