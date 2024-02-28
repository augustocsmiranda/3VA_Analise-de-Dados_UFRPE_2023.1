# Carregar a biblioteca necessária
library(ggplot2)

# Criar um dataframe com os dados
dados <- data.frame(
  Animal = c("Escorpião", "Serpente", "Aranha", "Outros Animais"),
  Total = c(8208, 4944, 4661, 5834),
  Porcentagem = c(34.71, 20.91, 19.71, 24.67)
)

# Gráfico de barras
grafico_barras <- ggplot(dados, aes(x = Animal, y = Total, fill = Animal)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribuição de Animais",
       x = "Tipo de Animal",
       y = "Total",
       fill = "Tipo de Animal") +
  theme_minimal()

# Gráfico de pizza
grafico_pizza <- ggplot(dados, aes(x = "", y = Porcentagem, fill = Animal)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start=0) +
  labs(title = "Distribuição de Animais",
       fill = "Tipo de Animal") +
  theme_minimal() +
  theme(legend.position = "right")

# Visualizar os gráficos
print(grafico_barras)
print(grafico_pizza)
