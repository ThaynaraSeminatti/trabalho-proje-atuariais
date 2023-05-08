####pacotes usados####
library(dplyr)
############# ler dados de todos os estados do brasil #####

serie_br = read.csv('serie_temporal_estados_br.csv')
serie_br = serie_br[,c('year', 'mx','location_name', 'sex', 'age_group_name')]

#### filtra rio de aneiro ####

rio_de_janeiro_init = serie_br[serie_br$location_name == "Rio de Janeiro" & 
                            serie_br$sex == "both" , ]

rio_de_janeiro <- na.omit(rio_de_janeiro_init)

ordenado =  order(rio_de_janeiro$year)
rio_de_janeiro =  rio_de_janeiro[ordenado,]



#### separar anos####
grupos_idade = unique(rio_de_janeiro$age_group_name)
lista = list()


for (i in 1:length(grupos_idade)) {
  lista[[i]] = data.frame(rio_de_janeiro[rio_de_janeiro$location_name == "Rio de Janeiro" &
                                           rio_de_janeiro$sex == "both"&
                                           rio_de_janeiro$age_group_name == grupos_idade[i], ] )
  
}

###### montar a matriz
mx <- lapply(lista, function(df) df[["mx"]])
matrix_rio_de_janeiro <- do.call(rbind, mx)




##### funcao #### parte do Bernardo
estimate.leecarter <- function(nmx){
  
  # Usando mortalidade abaixo de 85 anos, em grupos quinquenais, estima o parametros 
  # do Lee-Carter.  Entrada é uma matriz de taxas de mortaliudade, linhas são datas
  # e as colunas são as idades
  
  log.nmx <- log(nmx)
  ax <- apply(log.nmx, 2, mean)
  swept.mx <- sweep(log.nmx, 2, ax) # calcula a diferen?a por ano e o ax para cada coluna 
  svd.mx <- svd(swept.mx)
  bx <- svd.mx$v[, 1]/sum(svd.mx$v[, 1])
  kt <- svd.mx$d[1] * svd.mx$u[, 1] * sum(svd.mx$v[, 1])
  result <- list(ax = ax, bx = bx, kt = kt)
  return(result)}

# Primeiro passo é ler a base de dados. Vamos usar em grupos quinquenais, mas
# podemos trabalhar com idades simples ou outras idades desejadas. No exemplo
# estamos trabalhando com dados quinquenais, mas a rotina pode ser ajustada
# facilmente para fazer de outras formas. 



####### lendo os dados com a função ####
matrix.mx.sw.both <- matrix(1,21,47)
matrix.mx.sw.both <- matrix_rio_de_janeiro
dimnames(matrix.mx.sw.both) <- list(c(0,1,seq(5,95,by=5)),seq(1970, 2016))
matriz_lee_carter = t(matrix.mx.sw.both)


## Passo 2 - rodar o modelo de Lee-Carter original usando a funçao ####
## que foi lida anteriormente

model.sw.both <- estimate.leecarter(matriz_lee_carter)

# preparar os dados de ax e bx #####
ax.bx.kt.model.sw.both<- data.frame(model.sw.both$ax, model.sw.both$bx)

# preparar os dados de kt####
kt <- model.sw.both$kt

#Calcular a serie temporal de kt####

kt.diff <- diff(kt)
model.kt <- summary(lm(kt.diff ~ 1  ))
kt.drift <- model.kt$coefficients[1,1]
sec <- model.kt$coefficients[1,2]
see <- model.kt$sigma


## Projetar os kts####
x <- seq(0,99)
kt.stderr <- ( (x*see^2) + (x*sec)^2 )^.5
kt.forecast <- -9.3756565 + (x * kt.drift)
kt.lo.forecast <- kt.forecast + (1.96*kt.stderr)
kt.hi.forecast <- kt.forecast - (1.96*kt.stderr)

## Alguns gráficos ####
plot (seq(1970, 2016), kt)
plot(x, kt.forecast)
plot(model.sw.both$ax)
plot(model.sw.both$bx)


###### outros graficos #####


### Gráficos
age = c(0,1,seq(5,95,by=5))
axis(1,at = 1:21,labels = age, las = 2)

plot(model.sw.both$ax, xaxt = "n", xlab= "", ylab = "ax", main = "Gráfico de ax, Lee-Carter, Pop.Total, Brasil",
     type = "b", col = '#EB2559') #gráfico dos ax's
#axis(1,at = 1:23,labels = age, las = 2)

plot(model.sw.both$bx, xaxt = "n", xlab= "", ylab = "bx", main = "Gráfico de bx, Lee-Carter, Pop.Total, Brasil",
     type = "b", col = '#EB2559') #gráfico dos bx's



# preparar os dados de ax e bx
Parametros_f<- data.frame(model.sw.both$ax, model.sw.both$bx)

# preparar os dados de kt
kt_both <- model.sw.both$kt #kt's observados

#Calcular a serie temporal de kt
kt_both.diff <- diff(kt_both)
model.kt_both <- summary(lm(kt_both.diff ~ 1  ))
kt_both.drift <- model.kt_both$coefficients[1,1]
sec <- model.kt_both$coefficients[1,2]
see <- model.kt_both$sigma

## Projetar os kts
x <- seq(1,50)     #numero de anos a ser estimado - 2035
xanos = x + 2016
kt_both.stderr <- ( (x*see^2) + (x*sec)^2 )^.5
kt_both.forecast <- tail(kt_both, 1) + (x * kt_both.drift) #previsão dos kt's
kt_both.lo.forecast <- kt_both.forecast + (1.96*kt_both.stderr) #limite inferior do intervalo de previs??o de kt
kt_both.hi.forecast <- kt_both.forecast - (1.96*kt_both.stderr)#limite superior do intervalo de previs??o de kt

## Alguns gráficos
par(mfrow=c(1,2))
plot (seq(1970, 2016), kt_both, xlab= "Ano", ylab = "Kt observado", type = "b", col = '#EB2559',
      main = c("Kt Total" ,"Brasil, 1970 a 2016")) #grafico dos kt's observados
plot(xanos, kt_both.forecast, xlab= "Ano", ylab = "Kt estimado", type = "b", col = '#EB2559',
     main = c("Kt Total" ,"Brasil, 2017 a 2066")) #gráfico dos kt's estimados


both.nmx <- matrix(nrow = length(kt_both.forecast), ncol = length(age))
for (i in 1:length(kt_both.forecast)){
  both.nmx [i, ] <- exp((model.sw.both$bx * kt_both.forecast[i]) + model.sw.both$ax)
}

both.nmx <- t(both.nmx) 
both.nmx <- as.data.frame(both.nmx) 

plot.kt_both <- kt_both
plot.kt_both <- as.data.frame(plot.kt_both)
plot.kt_both$years <- seq(1970, 2016)
year <- seq(1970,2016,1)
Year<- seq(1970,2066,1)
require(ggplot2)
qplot(data = plot.kt_both, x = year, y = kt_both, main = "LFPR Model kt Total", 
      geom = "line", ylab = "kt", xlab = "Ano")


plot.both.kt <- c(kt_both, kt_both.forecast, kt_both.lo.forecast, kt_both.hi.forecast)
plot.both.kt <- as.data.frame(plot.both.kt)

plot.both.kt$Type <- c(rep("Forecast", times = sum(length(kt_both), length(kt_both.forecast))),
                    rep("Low", times = length(kt_both.lo.forecast)),
                    rep("High", times = length(kt_both.hi.forecast)))
plot.both.kt$Year <- c(rep(seq(1970, 2066), times = 1),
                    rep(seq(2017, 2066), times = 1),
                    rep(seq(2017, 2066), times = 1))

colnames(plot.both.kt) <- c("kt", "Type", "Year")

qplot(data = plot.both.kt, x = Year, y = kt, geom = "line", colour = Type, main = "Death Rates Model kt with Forecasts 
      and Confidence Interval", xlab = "Years") +
  scale_color_manual(values=c("#000000", "#FF0040","#58D3F7")) + 
  theme(legend.position = "none")

## Gráfico bem simples das taxas de mortalidade em alguns pontos do tempo
plot(age, log(both.nmx$V1), type="l", col="green", pch=21, lty=1, lwd = 1.5, ylab = "log(nmx)", ylim =c(-10,0))
lines(age,log(both.nmx$V11), type="l", pch=22, lty=2, col="red", lwd = 1.5)
lines(age,log(both.nmx$V26), type="l", pch=22, lty=3, col="blue", lwd = 1.5)
title(main="Age Specific Death Rates, Total, BR, 2017-2042", col.main="black", font.main=4)
legend("bottomright", c("2018","2027", "2042"), cex=0.8, 
       col=c("green","red", "blue"), lty=1:3)

#### Gráfico de superficie (heatplot - permite avaliar mais dados juntos)
both.nmx = as.matrix(both.nmx)
row = c(0,1,seq(5,105, by=5)) #0 a 100 tamanho do seu intervalo etario
colunm = seq(2017,2066) #intervalo de projeção
dimnames(both.nmx) =list(row,colunm)
require(lattice)
levelplot(both.nmx, ylab="Ano", xlab="Idade",strip = FALSE, main="Mortality Rates, Females, BR, 2017 a 2066")





