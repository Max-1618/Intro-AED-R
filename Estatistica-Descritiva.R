# Análise Exploratória #

### Estatística Descritiva

# o objetivo é sintetizar os dados de maneira direta, preocupando-se menos 
# com variações e intervalos de confiança dos dados.

# ------------------------------------------------------------------------------
# Conjunto de Dados
# Dataset Iris
dados <- iris
head(dados)

# Funções para efetuar estatística descritiva
# média: mean()
# desvio padrão: sd()
# variância: var()
# valor mínimo: min()
# valor máximo: max()
# mediana: median()
# amplitude de valores (min e máximo): range()
# quartis: quantile()
# função genérica: summary()

# medidas de tendência central
mean(dados$Sepal.Length)
median(dados$Sepal.Length)

# medidas de variabilidade
min(dados$Sepal.Length)
max(dados$Sepal.Length)
range(dados$Sepal.Length)
quantile(dados$Sepal.Length)
var(dados$Sepal.Length)
sd(dados$Sepal.Length)

# Resumo estatístico
summary(dados)

# Gráficos de distribuições
library(ggpubr)

# Box plots
ggboxplot(dados, y="Sepal.Length", width = 0.5)

# Analisando o boxplot (outliers)
v = c(10.2, 14.1, 14.4, 14.4, 14.4, 14.5, 14.5, 14.6, 14.7, 14.7, 14.7, 14.9, 15.1, 15.9, 16.4)
v
summary(v)
IQR(v)  # Interquartile range

# Se um valor estiver abaixo de Q1-1.5xIQR ou acima de Q3+1.5xIQR é considerado um outlier
q1 = summary(v)[2]
q3 = summary(v)[5]

limite_inferior = q1-1.5*IQR(v)
limite_inferior
limite_superior = q3+1.5*IQR(v)
limite_superior

ggboxplot(v, width = 0.5)

# Histograma
gghistogram(dados, x = "Sepal.Length", bins = 9, add = "mean")

# Gráfico de densidade
ggdensity(dados, x = "Sepal.Length", add = "median")

# Análise de distribuição normal
ggqqplot(dados, x="Sepal.Length")

# Combinando os gráficos na mesma área de plotagem
bp <- ggboxplot(dados, y="Sepal.Length", width = 0.5)
ht <- gghistogram(dados, x = "Sepal.Length", bins = 9, add = "mean")
ds <- ggdensity(dados, x = "Sepal.Length", add = "median")
dn <- ggqqplot(dados, x="Sepal.Length")

ggarrange(bp, ht, ds, dn)

# Gráficos para dados agrupados
ggboxplot(dados, x = "Species", y = "Sepal.Length",
          color = "Species",
          palette = "Dark2")

# Os valores permitidos para o parâmetro palette são:
# "grey" para paleta de cores cinzas;
# paletas de cores "brewer": "RdBu", "Blues", "Dark2"
# Para ver todas: RColorBrewer::display.brewer.all()
# paletas customizadas: c("blue", "red")
# paletas de jornais científicos do pacote "ggsci": “npg”, “aaas”, “lancet”, “jco”, 
# “ucscgb”, “uchicago”, “simpsons” and “rickandmorty”.

ggstripchart(dados, x = "Species", y = "Sepal.Length",
             color = "Species",
             palette = "ggsci",
             add = "mean_sd")


# Tabelas de frequência
# Usada para descrever variáveis categóricas. Contém as contagens de cada combinação
# dos níveis dos fatores.

# ----------------------------------------------------------------------------
# Conjunto de Dados
# Lendo o dataset credito.csv
df <- read.csv("credito.csv")
View(df)
str(df)

# Variáveis tipo.financiamento, ocupacao e parecer
tipo.financiamento <- df$tipo_financ
ocupacao <- df$ocupacao
parecer <- df$decisao_financ

# Distribuição de frequência:
table(tipo.financiamento)
table(ocupacao)
table(parecer)

# Convertendo as tabelas para dataframes
df_fin <- as.data.frame(table(tipo.financiamento))
df_oc <- as.data.frame(table(ocupacao))
df_par <- as.data.frame(table(parecer))

head(df_fin)

# Gráfico de barras
fin <- ggbarplot(df_fin, x="tipo.financiamento", y="Freq")
oc <- ggbarplot(df_oc, x="ocupacao", y="Freq")
par <- ggbarplot(df_par, x="parecer", y="Freq")

ggarrange(fin, oc, par)

# Gráfico de pizza
ggpie(df_par, x="Freq", label = "parecer",
      color = "white",
      fill = "parecer",
      palette = c("blue", "red", "gray"))

# Tabela com duas variáveis categóricas
tbl2 <- table(tipo.financiamento, ocupacao)
tbl2

# Transformando a tabela em um dataframe para visualizar o gráfico de barras
df2 <- as.data.frame(tbl2)

ggbarplot(df2, x="ocupacao", y="Freq",
          color = "tipo.financiamento",
          palette = c("brown", "blue", "gold", "green"))

ggbarplot(df2, x = "ocupacao", y = "Freq",
          color = "tipo.financiamento", position = position_dodge(),
          palette = c("brown", "blue", "gold", "green"))

# Frequência relativa com a função prop.table()
# Segundo parâmetro: 1 para linhas e 2 para colunas
prop.table(tbl2, 1)
prop.table(tbl2, 2)

# Percentuais
round(prop.table(tbl2, 1), 2)*100
