install.packages("corrplot")
install.packages("dplyr")
install.packages("GGally")
install.packages("ggplot2")
install.packages("Hmisc")
install.packages("modeest")
install.packages("modeest") 
install.packages("PerformanceAnalytics")
install.packages("plyr")
install.packages("readr")
install.packages("VIM")
install.packages("writexl")
install.packages("EnvStats")
install.packages("dplyr")
library(readr)
library(dplyr)
library(modeest)#Para obtener la moda
library(psych)
library(writexl)
library(modeest)
library(tidyverse)
library(knitr)
library(stats)
library(VIM)
library(plyr)
library(ggplot2)
library(GGally)
library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)
library(EnvStats)

setwd("/Users/king/Desktop/ESTADISTICA_PARA_CIENCIA_-DE-DATOS/")

ART <- read_csv("articulos.csv")
colnames(ART)
#Cada vez que se hable de valor unitario de los art?culos ya esta calculada,debo trabajar para el sector 10
ART=ART %>% filter(SECTOR==10,na.rm=TRUE )
View(ART)
#1- Se pide una tabla de contingencia, pensando en lo que vimos con TEO de Bayes y TEO de probalidades totales.# 1. [5 puntos] Obtenga una tabla que entregue la probabilidad de utilizar cierto METODO_DE_PAGO,# dado el TIPO_ARTICULO, y analice los resultados.Total de pagos efectivo, con calidad simple.nrow(ART)#Espacio muestral metodos de pago 1.306

table(ART$METODO_DE_PAGO, ART$TIPO_ARTICULO) %>%
  prop.table(margin=2)

# 2. [5 puntos] Obtenga el valor unitario de los productos y realice un an?lisis descriptivo univariado de
# todas las columnas. Puede utilizar "EDAs autom?ticos" para simplificar la programaci?n

var_continuas=ART[c("ACCOUNTID","SUCURSAL","DESCUENTO","MONTO_PAGADO","CANTIDAD_ARTICULO","MARGEN","SECTOR","VALOR_ARTICULO")]
psych::describe(var_continuas)


#---------------------------------------------PUNTO 2 variable de categoría
TIPO_ARTICULO= data.frame(table(ART$TIPO_ARTICULO))
colnames(TIPO_ARTICULO)=c("TIPO_ARTICULO","FREQ")
TIPO_ARTICULO_RESUMEN <- data.frame(t(TIPO_ARTICULO[-1]))
# A?adimos los nombres de las columnas
colnames(TIPO_ARTICULO_RESUMEN) <- TIPO_ARTICULO[, 1]

TIPO_ARTICULO_RESUMEN= TIPO_ARTICULO_RESUMEN %>% mutate(Maximo=round(max(TIPO_ARTICULO$FREQ, na.rm = TRUE),2),Minimo=round(min(TIPO_ARTICULO$FREQ, na.rm = TRUE),2
))
View(TIPO_ARTICULO$FREQ)
summary(TIPO_ARTICULO$FREQ)
#---------------------------------------------FECHA_COMPRA
FECHA_COMPRA=data.frame(table(ART$FECHA_COMPRA))
colnames(FECHA_COMPRA)= c("FECHA_COMPRA","FREQ")

summary(FECHA_COMPRA$FREQ)

FECHA_COMPRA_RESUMEN <- data.frame(t(FECHA_COMPRA[-1]))
# A?adimos los nombres de las columnas
colnames(FECHA_COMPRA_RESUMEN) <- FECHA_COMPRA[, 1]

FECHA_COMPRA_RESUMEN= FECHA_COMPRA_RESUMEN %>% mutate(Maximo=max(FECHA_COMPRA$FREQ, na.rm = TRUE),Minimo=min(FECHA_COMPRA$FREQ, na.rm = TRUE)
)
View(FECHA_COMPRA_RESUMEN)
#---------------------------------------------METODO_DE_PAGO
METODO_DE_PAGO= data.frame(table(ART$METODO_DE_PAGO))
colnames(METODO_DE_PAGO)=c("METODO_DE_PAGO","FREQ")

METODO_DE_PAGOA_RESUMEN <- data.frame(t(METODO_DE_PAGO[-1]))
# A?adimos los nombres de las columnas
colnames(METODO_DE_PAGOA_RESUMEN) <- METODO_DE_PAGO[, 1]

METODO_DE_PAGOA_RESUMEN= METODO_DE_PAGOA_RESUMEN %>% mutate(Maximo=max(METODO_DE_PAGO$FREQ, na.rm = TRUE),Minimo=min(METODO_DE_PAGO$FREQ, na.rm = TRUE)
)
View(METODO_DE_PAGOA_RESUMEN)
summary(METODO_DE_PAGO$FREQ)


#write_xlsx(punto2,"C:/Users/fdono/Desktop/tallerFinalEstad?stica/punto2.xlsx") esto es solo para cexportar a Excel

#----------------------------------PREGUNTA 3
#5 puntos] Realice un tratamiento de datos: determine datos atípicos, NA y tome acciones con ellos.
#Limpieza y manipulación 
#Valores nulos, variable ACCOUNTID, para este punto se volverá a cargar el data frame pero sin omitir nulos
ART=ART %>% filter(SECTOR==10)
nrow(ART)
any(is.na(ART$ACCOUNTID))# si hubiese indicado True, habría al menos un valor NA
any(is.na(ART$METODO_DE_PAGO))
any(is.na(ART$FECHA_COMPRA))
any(is.na(ART$SUCURSAL))
any(is.na(ART$DESCUENTO))
any(is.na(ART$MONTO_PAGADO))# Se detecta que al menos hay un valor NA.
any(is.na(ART$TIPO_ARTICULO))
any(is.na(ART$CANTIDAD_ARTICULO))
any(is.na(ART$MARGEN))
any(is.na(ART$VALOR_ARTICULO))
which(is.na(ART$VALOR_ARTICULO))


boxplot.stats(margen_NA$VALOR_ARTICULO)
boxplot(ART$VALOR_ARTICULO)
plot(margen_NA$MARGEN, type="b", pch=16)
atipico_valorArticulo=c(boxplot.stats(ART$VALOR_ARTICULO)$out) 
ordenado=order(atipico_valorArticulo)
atipico_valorArticulo[ordenado]

ART$VALOR_ARTICULO
boxplot.stats(ART$VALOR_ARTICULO)$n

which(is.na(ART$MARGEN))
margen_NA=ART %>% filter(is.na(MARGEN)==TRUE)
boxplot.stats(margen_NA$MARGEN)
boxplot(margen_NA$MARGEN)
plot(margen_NA$MARGEN, type="b", pch=16)
boxplot.stats(margen_NA$MARGEN)$out 
boxplot.stats(margen_NA$MARGEN)$n

View(ART[32,])
unique(ART$MARGEN)

which(is.na(ART$MONTO_PAGADO))#45,341,466,633,673,767,1084,1222, son 8 en total
monto_pagoNA=ART %>% filter(is.na(MONTO_PAGADO)==TRUE)
summary(ART$MARGEN)
max(ART$MARGEN,na.rm=TRUE)
min(ART$MARGEN,na.rm=TRUE)#Con respecto al valor de Margen no hay datos atipicos comparando con el máximo y mínimo
View(monto_pagoNA)
sum(ART$MARGEN==2950, na.rm=TRUE)
boxplot.stats(ART$MARGEN)
boxplot(ART$MARGEN)
plot(ART$MARGEN, type="b", pch=16)
boxplot.stats(ART$MARGEN)$out 
boxplot.stats(ART$MARGEN)$n
#####1### Pregunta 3 
# Verificar que las variables descriptivas no tengan errores ortograficos
Tipo_articulo = ART$TIPO_ARTICULO
table(Tipo_articulo)

Metodo_pago = ART$METODO_DE_PAGO
table(Metodo_pago)
# Considerando que transferencia y Transbank son métodos distintos, se concluye que no hay error de manipulación al ingresar los registros
#Se selecciona una matriz de correlación con los valores númericos
articulos_num = ART %>%
  select(ACCOUNTID, SUCURSAL, DESCUENTO, MONTO_PAGADO, CANTIDAD_ARTICULO, MARGEN, VALOR_ARTICULO) %>%
  na.omit()
View(articulos_num)
### Destermina la distancia de los valores
dist_m = mahalanobis(articulos_num, colMeans(articulos_num), var(articulos_num))
boxplot(dist_m, las=1)
mean(dist_m) + c(-3,3)*sd(dist_m)
# Menos del 1% esta sobre 500
mean(dist_m < -47.46135 )
mean(dist_m> 61.45049)
#Revisar valores atipico
articulos_num[dist_m > 61.45049,] %>% kable() #Se puede inferir que los valores atipicos son debido a que el margen es menor con respecto a su Monto pagado
summary(ART)
# Revisa Missing Values+
par(cex=0.6)
VIM::aggr(ART,numbers=T,sortVar=T)
## Como los valores de Metodo de pago no es una pregunta directa del negocio se puede imputar


#######     PREGUNTA 4
ART2 <- ART %>%
  select(MARGEN, VALOR_ARTICULO, TIPO_ARTICULO,CANTIDAD_ARTICULO) %>%
  group_by(TIPO_ARTICULO) %>%
  mutate(VALOR_UNITARIO = VALOR_ARTICULO / CANTIDAD_ARTICULO) %>%
  select(MARGEN,VALOR_UNITARIO) 

ART4 <- ART %>%
  select(MARGEN, VALOR_ARTICULO, TIPO_ARTICULO,CANTIDAD_ARTICULO) %>%
  mutate(VALOR_UNITARIO = VALOR_ARTICULO / CANTIDAD_ARTICULO) %>%
  select(MARGEN,VALOR_UNITARIO) 


ART3<-na.omit(ART2) 
ddply(ART3, .(TIPO_ARTICULO), summarise, "corr" = cor(MARGEN, VALOR_UNITARIO, method = "spearman"))

View(ART3)

chart.Correlation(ART4, histogram = F, pch = 19)

# A menor calidad hay mayor correlacion entre el MARGEN y el VALOR_UNITARIO
#  


options(scipen = 999)
#>>> Cálculo automático con la función de R cor.test()
(cor.test(ART3$MARGEN, ART3$VALOR_UNITARIO))


M1 <- ART3 %>% filter(TIPO_ARTICULO=="1. Calidad Simple")
M2 <- ART3 %>% filter(TIPO_ARTICULO=="2. Calidad Media")
M3 <- ART3 %>% filter(TIPO_ARTICULO=="3. Calidad Altae")
M4 <- ART3 %>% filter(TIPO_ARTICULO=="4. Calidad Premium")

View(M4)

(cor.test(M1$MARGEN, M1$VALOR_UNITARIO))
(cor.test(M2$MARGEN, M2$VALOR_UNITARIO))
(cor.test(M3$MARGEN, M3$VALOR_UNITARIO))
(cor.test(M4$MARGEN, M4$VALOR_UNITARIO))

# no hay correlación significativa a mayor calidad del producto con un intervalo de 95%

# pregunta 5---------------

COMPRAS_POR_DIA <- ART %>% count(FECHA_COMPRA)
lambda <- mean(COMPRAS_POR_DIA$n)

#0.9994002,es la probabilidad teorìca y 0,8 es la probabilidad Empirìca  

#prob_teo
ppois(30, lambda=lambda, lower=FALSE)

#prob_emp
sum(COMPRAS_POR_DIA$n>=30) / count(COMPRAS_POR_DIA, "n")$n

#PREGUNTA 6
alta = ART %>% filter(TIPO_ARTICULO =="3. Calidad Alta")
calidad_alta = as.numeric(na.omit(alta$VALOR_ARTICULO))


hist(calidad_alta, nclass=20, col="darkseagreen3", border="white", main="Calidad Alta", prob=TRUE)

## A travez de ggplot se puede hacer un primer acercamiento para determinar si cumple una distribución normal

EnvStats::qqPlot(calidad_alta, dist="norm", add.line=TRUE)

##Con prueba de hipotesis H0 = Calidad_alta distribuye normal vs H1 que no distribuye normal, para la prueba de hipotesis se ocupa Kolmogorov-smirnov

ks.test(calidad_alta, "pnorm", mean = mean(calidad_alta), sd = sd(calidad_alta))

##Como p-value es mayor al 5% de significa, no se puede rechazar que los valores unitarios de los productos de alta calidad siguen una distribucion normal

###### PREGUNTA 7

mu = mean(na.omit(ART$DESCUENTO))

t.test(na.omit(ART$DESCUENTO), mu = mu, alternative = "less")
# a) con un 95% de confianza tenemos un intervalo de confianza de:
#       ]-Inf 937.5249]


n = sum(na.omit(ART$MONTO_PAGADO))

x=sum(na.omit(ART$DESCUENTO))
prop.test(x = x, n = n)$conf.int

# b) para una confianza de 95% tenemos un intervalo para la proporcion de:
#       ]0.02087647 0.02095193]

###### PREGUNTA 8
#Se crea dataset con solo los tipos de artìculos C. media y C alta.
ART_TIPO = ART %>% 
  filter(TIPO_ARTICULO %in% c("2. Calidad Media" , "3. Calidad Alta"))

CM = ART_TIPO %>% 
  filter(TIPO_ARTICULO =="2. Calidad Media") %>% 
  select(MARGEN)

CA = ART_TIPO %>% 
  filter(TIPO_ARTICULO =="3. Calidad Alta") %>% 
  select(MARGEN)
#Luego se calcula la media de cada margen del artìculo
media_margen=mean(CM$MARGEN,na.rm = TRUE)
alta_margen=mean(CA$MARGEN, na.rm = TRUE)
#Luego se calcula la varianza de cada margen del artìculo
varianza_media=var(CM$MARGEN,na.rm = TRUE)
varianza_alta=var(CA$MARGEN, na.rm = TRUE)

media_margen - alta_margen

#Para el test de hipótesis se estimo como H0 que la diferencia de las medias es igual a cero.

t.test(ART_TIPO$MARGEN ~ ART_TIPO$TIPO_ARTICULO, conf.level=0.95,var.equal = FALSE)
#### Debido a que p-value es menor a un 5% de significancia se rechaza la hipótesis nula de igualdad de medias.

#### Para comparar 2 Varianzas se realiza la divisiòn entre ellas, si el resultado es igual a 1 son iguales.
varianza_media / varianza_alta
#Como conclusión, dado como resultado un p-value inferior a un 5% de significancia se rechaza la hipótesis nula de igualdad de varianza.
var.test(ART_TIPO$MARGEN ~ ART_TIPO$TIPO_ARTICULO, conf.level=0.95)