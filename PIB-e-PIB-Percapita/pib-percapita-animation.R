# Instalar o pacote gapminder caso não esteja instalado
# install.packages('gapminder')

# Carregar os pacotes necessários
library(ggplot2)
library(gganimate)
library(gapminder)

# Configurar o tema padrão para os gráficos
theme_set(theme_bw())

# Visualizar as primeiras linhas do conjunto de dados gapminder
head(gapminder)

# Criar um gráfico de pontos para explorar a relação entre o PIB per capita e a expectativa de vida,
# onde o tamanho dos pontos é proporcional à população e a cor representa o país.
p <- ggplot(
  gapminder, 
  aes(x = gdpPercap, y = lifeExp, size = pop, colour = country)
) +
  geom_point(show.legend = FALSE, alpha = 0.7) +  # Pontos sem legenda e parcialmente transparentes
  scale_color_viridis_d() +  # Escala de cores viridis
  scale_size(range = c(2, 12)) +  # Intervalo de tamanhos para os pontos
  scale_x_log10() +  # Escala logarítmica para o eixo x (PIB per capita)
  labs(x = "PIB per capita", y = "Expectativa de vida")  # Rótulos dos eixos

# Adicionar uma animação de tempo ao gráfico, exibindo a mudança ao longo dos anos
p + transition_time(year) +
  labs(title = "Ano: {frame_time}")  # Título dinâmico que mostra o ano da frame

# Criar uma versão do gráfico com separação por continentes usando facet_wrap
p + facet_wrap(~continent) +
  transition_time(year) +
  labs(title = "Ano: {frame_time}")  # Título dinâmico com o ano

# Exibir o gráfico estático
p
