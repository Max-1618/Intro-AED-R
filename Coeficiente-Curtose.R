# Coeficiente de Curtose (Kurtosis)

# O coeficiente de curtose é uma medida que caracteriza o achatamento
# da curva da função de distribuição.

# Exemplo: Os seguintes dados representam o número de falhas mensais 
# nos equipamentos em uma industria durante o período de 50 mesess. 

dados = c(18, 20, 20, 21, 22, 24, 25, 25, 26, 27, 29, 29, 
          30, 30, 31, 31, 32, 33, 34, 35, 36, 36, 37, 37, 
          37, 37, 38, 38, 38, 40, 41, 43, 44, 44, 45, 45, 
          45, 46, 47, 48, 49, 50, 51, 53, 54, 54, 56, 58, 62, 65)

# Histograma
hist(dados, main = "Número de Acidentes Diários", 
     xlab = "Acidentes", ylab = "Frequência")

mean(dados)
sd(dados)
median(dados)

library(moments)
?kurtosis
CK = kurtosis(dados)
print(CK)

# CK ≈ 0: Distribuição normal. 
# CK < 0: Cauda mais leve que a normal. Coeficiente de Curtose negativo.
# CK > 0: Cauda mais pesada que a normal. Coeficiente de Curtose positivo.

# O coeficiente de curtose é igual a 2.37652. 

# Skewness e Kurtosis
skewness(dados)
kurtosis(dados)





