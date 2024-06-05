library(ggplot2)
library(gapminder)
library(magrittr)

# Filtrar dados para o Brasil
brasil_data <- subset(gapminder, gapminder$country == "Brazil")


# Criar gráfico de linha para a expectativa de vida ao longo dos anos no Brasil
p_brasil_lifeExp <- ggplot(brasil_data, aes(x = year, y = lifeExp)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 3, shape = 21, fill = "white") +
  labs(title = "Expectativa de Vida no Brasil ao Longo dos Anos",
       x = "Ano",
       y = "Expectativa de Vida") +
  theme_minimal()

# Exibir o gráfico
p_brasil_lifeExp

p_brasil_trajectory <- ggplot(brasil_data, aes(x = gdpPercap, y = lifeExp, size = pop)) +
  geom_point(aes(color = year), alpha = 0.7) +
  scale_color_viridis_c(option = "D", begin = 0.3, end = 0.7, direction = 1, guide = "none") +
  scale_size(range = c(3, 10)) +
  scale_x_log10() +
  labs(title = "Trajetória do Brasil: PIB per Capita vs. Expectativa de Vida",
       x = "PIB per Capita (log)",
       y = "Expectativa de Vida") +
  theme_minimal()

# Exibir o gráfico
p_brasil_trajectory


p <- ggplot(
  brasil_data, 
  aes(x = gdpPercap, y = lifeExp, size = pop, colour = country)
) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "PIB per capita", y = "Expectativa de vida")

p + transition_time(year) +
  labs(title = "Trajetória do Brasil: PIB per Capita vs. Expectativa de Vida",
       title = "Ano: {frame_time}") +
  shadow_wake(wake_length = 0.1, alpha = FALSE)

