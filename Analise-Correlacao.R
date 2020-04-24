##----- Análise de Correlação

# O teste de correlação é usado para avaliar a associação entre duas ou mais 
# variáveis

# Correlação de Pearson 
# O coeficiente de correlação de Pearson (r), também chamado de correlação linear 
# ou r de Pearson, é um grau de relação entre duas variáveis quantitativas 
# e exprime o grau de correlação através de valores situados entre -1 e 1.

# Valor-p (nível de significância)
# se o valor-p é menor que 5%, então a correlação entre x e y é significativa.

# Dataset mtcars (built-in)
dados <- mtcars
head(dados)

# Pretende-se verificar a correlação entre as variáveis "mpg" e "wt" 

# Visualizando os dados 
library(ggpubr)
ggscatter(dados, x = "mpg", y = "wt",
          xlab = "Milhas por Galão", ylab = "Peso")

# Acrescentando parâmetros estatísticos
ggscatter(dados, x = "mpg", y = "wt",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Milhas por Galão", ylab = "Peso")


# Testes preliminares

# Analisando o gráfico é possível verificar uma covariação linear. 

# Inspeção visual da normalidade dos dados usando Q-Q plots (quantile-quantile plots)
# Esse gráfico mostra a correlação entre uma amostra e uma distribuição normal.

# Variável "mpg"
ggqqplot(dados$mpg, ylab = "MPG")
# Variável "wt"
ggqqplot(dados$wt, ylab = "WT")

# Importante! Se os dados não estiverem normalmente distribuídos e recomendável 
# usar outro método de correlação.

# Teste de correlação de Pearson
res <- cor.test(dados$wt, dados$mpg, method = "pearson")
res

# Acessando os valores calculados pela função cor.test(): p.value e estimate
res$p.value
res$estimate

###--------- Matriz de Correlação
dados2 <- mtcars[, c(1,3,4,5,6,7)]
head(dados2)

mtx_cor <- cor(dados2)
round(mtx_cor, 2)

# Se o dataset contém valores em branco (missing) use: 
# cor(dados2, use = "complete.obs")

# Matriz de correlação com valores-p (p-value)
# A função rcorr()[Hmisc] pode ser usada para calcular os níveis de significância.
# A função retorna ambos os coeficientes de correlção e valores-p.
library(Hmisc)
res2 <- rcorr(as.matrix(dados2))
res2

# Extraindo os coeficientes de correlação
res2$r

# Extraindo os valores-p
res2$P

# Visualizando a matriz de correlação
# Gráfico de correlação com a função corrplot()[corrplot]
# O parâmetro type pode ter os seguintes valores: "upper", "lower", "full"
library(corrplot)
corrplot(mtx_cor, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)


# Combinando os coeficientes de correlação e teste de significância no mesmo gráfico.
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.01, insig = "blank")


# Construindo scatter plots
# A função char.Correlation()[PerformanceAnalytics] pode ser usada para mostrar
# uma coleção de gráficos de uma matriz de correlação
library(PerformanceAnalytics)
chart.Correlation(dados2, histogram = TRUE, pch = 19)
