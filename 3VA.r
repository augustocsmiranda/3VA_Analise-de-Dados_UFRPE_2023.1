##### QUESTÃO 1
# Carregue o dataset 'mtcars' do RStudio
data(mtcars)

# Obtenha os nomes das linhas (observações) do dataset e atribua à variável 'nomes'
nomes <- rownames(mtcars)

# Defina a expressão regular para substituir os nomes dos carros Mercedes
padrao <- "Merc\\s*(\\d+)([A-Z]*)"

# Substitua os nomes dos carros Mercedes
nomes_substituidos <- gsub(pattern = padrao, replacement = "MercedesX\\1\\2", nomes)

# Visualize os novos nomes dos carros Mercedes
print(nomes_substituidos)



##### QUESTÃO 2

# Carregue o arquivo CSV usando a função read.csv() se o separador for uma vírgula
dados <- read.csv("compdataset_V2.csv")

ds <- dados
x <- ds$COMMAND

# Use grep() para retornar somente as observações com colchetes
observacoes_com_colchetes <- grep(pattern = "\\[", x, value = TRUE)

# Visualize as observações com colchetes
print(observacoes_com_colchetes)


#### Questão 4

# Instalar e carregar os pacotes necessários
install.packages("ggplot2")
install.packages("dplyr")

library(ggplot2)
library(dplyr)

# Carregar o conjunto de dados msleep
data(msleep)

# Visualizar os tipos de colunas e seus nomes
str(msleep)

print(msleep)
# Contar o número de mamíferos primatas
num_primatas <- msleep %>%
  filter(order == "Primates") %>%
  nrow()

print(num_primatas)

#### Questão 5

# Calcular a média de sono dos mamíferos domesticados
media_sono_domesticados <- msleep %>%
  filter(conservation == "domesticated") %>%
  summarise(media_sono = mean(sleep_total, na.rm = TRUE))

print(media_sono_domesticados)

##### Questão 6

# Filtro para mamíferos com peso corporal maior que 10 quilos
subconjunto <- msleep %>%
  filter(bodywt > 10)
# Calcular a proporção de mamíferos que não são carnívoros
proporcao_nao_carnivoros <- subconjunto %>%
  summarise(proporcao_nao_carnivoros = mean(order != "Carnivora")) %>%
  pull(proporcao_nao_carnivoros) * 100

# Arredondar o resultado para uma casa decimal
proporcao_nao_carnivoros <- round(proporcao_nao_carnivoros, 1)

print(proporcao_nao_carnivoros)




##### questão 7

library(dplyr)

# Remover linhas com NA em bodywt e brainwt
msleep_completos <- msleep %>%
  na.omit()

# Adicionar uma nova coluna com o peso médio
msleep_completos <- msleep_completos %>%
  mutate(peso_medio = (bodywt + brainwt) / 2)

# Filtrar para remover valores ausentes
msleep_completos <- msleep_completos %>%
  filter(!is.na(peso_medio))

# Identificar o animal com o terceiro menor valor médio
terceiro_menor <- msleep_completos %>%
  arrange(peso_medio) %>%
  slice(3) %>%
  select(name, peso_medio)

print(terceiro_menor)


##### QUESTÃO 8

# Carregar o conjunto de dados weather
dataurl <- "https://jozef.io/post/data/"
weather <- readRDS(url(paste0(dataurl, "r006/weather.rds")))

# Carregar o conjunto de dados flights
flights <- readRDS(url(paste0(dataurl, "r006/flights.rds")))

# Fusão dos conjuntos de dados usando as colunas "time_hour" e "origin"
dados_combinados <- merge(weather, flights, by.x = c("time_hour", "origin"), by.y = c("time_hour", "origin"))

# Visualizar os primeiros registros do conjunto de dados combinado
head(dados_combinados)


###### QUESTÂO 8 CONT

library(dplyr)

# Encontrar os voos que decolaram com a menor temperatura
voos_menor_temperatura <- dados_combinados %>%
  filter(dep_time == min(dep_time))

# Verificar se há voos na seleção
if (nrow(voos_menor_temperatura) > 0) {
  # Verificar o voo que percorreu a maior distância entre esses voos
  voo_maior_distancia <- voos_menor_temperatura %>%
    filter(distance == max(distance))
  
  # Imprimir o destino do voo que percorreu a maior distância
  print(voo_maior_distancia$dest)
} else {
  print("Nenhum voo encontrado com a menor temperatura de decolagem.")
}


##### Questão 9

# Filtrar os voos com temperatura superior a 100 Fahrenheit
voos_temperatura_alta <- subset(dados_combinados, Temp > 100)

# Encontrar o voo com o maior atraso de partida entre esses voos
voo_maior_delay <- voos_temperatura_alta[which.max(voos_temperatura_alta$dep_delay), ]

# Imprimir o destino do voo com o maior atraso
# Listar os nomes das colunas dos dados combinados
print(colnames(dados_combinados))


##### Questão 10

# Filtrar os dados para o mês de maio
dados_maio <- subset(airquality, Month == 5)

# Calcular a média da temperatura
media_temperatura <- mean(dados_maio$Temp, na.rm = TRUE)

# Calcular o desvio padrão da temperatura
desvio_padrao_temperatura <- sd(dados_maio$Temp, na.rm = TRUE)

# Calcular o tamanho da amostra
tamanho_amostra <- length(dados_maio$Temp)

# Calcular o intervalo de confiança de 90%
intervalo_confianca <- t.test(dados_maio$Temp, conf.level = 0.90)$conf.int

# Arredondar os limites do intervalo de confiança para duas casas decimais
limite_inferior <- round(intervalo_confianca[1], 2)
limite_superior <- round(intervalo_confianca[2], 2)

# Formatar a resposta
resposta <- paste0("[", limite_inferior, "-", limite_superior, "]")

print(resposta)
