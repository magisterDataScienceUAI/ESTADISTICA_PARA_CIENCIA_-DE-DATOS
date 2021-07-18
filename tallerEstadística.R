install.packages("readr")
install.packages("dplyr")
install.packages("modeest")
install.packages("writexl")
library(readr)
library(dplyr)
library(modeest)#Para obtener la moda
library(psych)
library(writexl)

setwd("C:/Users/fdono/Desktop/tallerFinalEstadística")

ART <- read_csv("articulos.csv")
colnames(ART)
#Cada vez que se hable de valor unitario de los artículos ya esta calculada,debo trabajar para el sector 10
ART=ART %>% filter(SECTOR==10,na.rm=TRUE )
View(ART)
#1- Se pide una tabla de contingencia, pensando en lo que vimos con TEO de Bayes y TEO de probalidades totales.
# 1. [5 puntos] Obtenga una tabla que entregue la probabilidad de utilizar cierto METODO_DE_PAGO,
# dado el TIPO_ARTICULO, y analice los resultados.Total de pagos efectivo, con calidad simple.
nrow(ART)#Espacio muestral metodos de pago 1.306

efectivo= ART %>% filter(ART$METODO_DE_PAGO=="EFECTIVO",)
#Solamente selecciono los campos necesarios
efectivo=efectivo[,c("METODO_DE_PAGO","TIPO_ARTICULO")]
#Lo llevo a una tabla para obtener la frecuencia de cada tipo de artículo
efectivo=data.frame(table(efectivo$TIPO_ARTICULO))
colnames(efectivo)=c("TIPO_ARTICULO","EFECTIVO")
efectivo= efectivo%>% mutate(PROB_EFEC= round(100*EFECTIVO/nrow(ART),2) )
efectivo=efectivo[,c("TIPO_ARTICULO","PROB_EFEC")]

TRANSBANK=ART %>% filter(ART$METODO_DE_PAGO=="TRANSBANK",)
#Solamente selecciono los campos necesarios
TRANSBANK=TRANSBANK[,c("METODO_DE_PAGO","TIPO_ARTICULO")]
#Lo llevo a una tabla para obtener la frecuencia de cada tipo de artículo
TRANSBANK=data.frame(table(TRANSBANK$TIPO_ARTICULO))
colnames(TRANSBANK)=c("TIPO_ARTICULO","TRANSBANK")
TRANSBANK= TRANSBANK%>% mutate(PROB_TRANSB=round(100*TRANSBANK/nrow(ART),2))
TRANSBANK=TRANSBANK[,c("TIPO_ARTICULO","PROB_TRANSB")]

OTRO=ART %>% filter(ART$METODO_DE_PAGO=="OTRO",)
#Solamente selecciono los campos necesarios
OTRO=OTRO[,c("METODO_DE_PAGO","TIPO_ARTICULO")]
#Lo llevo a una tabla para obtener la frecuencia de cada tipo de artículo
OTRO=data.frame(table(OTRO$TIPO_ARTICULO))
colnames(OTRO)=c("TIPO_ARTICULO","OTRO")
OTRO= OTRO%>% mutate(PROB_OTRO=round(100*OTRO/nrow(ART),2))
OTRO=OTRO[,c("TIPO_ARTICULO","PROB_OTRO")]

CHEQUE=ART %>% filter(ART$METODO_DE_PAGO=="CHEQUE",)
#Solamente selecciono los campos necesarios
CHEQUE=CHEQUE[,c("METODO_DE_PAGO","TIPO_ARTICULO")]
#Lo llevo a una tabla para obtener la frecuencia de cada tipo de artículo
CHEQUE=data.frame(table(OTRO$TIPO_ARTICULO))
colnames(CHEQUE)=c("TIPO_ARTICULO","CHEQUE")
CHEQUE= CHEQUE%>% mutate(PROB_CHEQUE=round(100*CHEQUE/nrow(ART),2))
CHEQUE=CHEQUE[,c("TIPO_ARTICULO","PROB_CHEQUE")]


TRANSFERENCIA=ART %>% filter(ART$METODO_DE_PAGO=="TRANSFERENCIA",)
#Solamente selecciono los campos necesarios
TRANSFERENCIA=TRANSFERENCIA[,c("METODO_DE_PAGO","TIPO_ARTICULO")]
#Lo llevo a una tabla para obtener la frecuencia de cada tipo de artículo
TRANSFERENCIA=data.frame(table(TRANSFERENCIA$TIPO_ARTICULO))
colnames(TRANSFERENCIA)=c("TIPO_ARTICULO","TRANSFERENCIA")

TRANSFERENCIA= TRANSFERENCIA%>% mutate(PROB_TRANSFERENCIA=round(100*TRANSFERENCIA/nrow(ART),2))
TRANSFERENCIA=TRANSFERENCIA[,c("TIPO_ARTICULO","PROB_TRANSFERENCIA")]

#Se agregan 02 filas más ya que para las transferencias no había datos en Calidad Alta y calidad Premium
nuevas_filas=data.frame(TIPO_ARTICULO=c("3. Calidad Alta","4. Calidad Premium"),PROB_TRANSFERENCIA=c(0,0))
TRANSFERENCIA=rbind(TRANSFERENCIA,nuevas_filas)


#Acá hay que hacer merge por cada origen
final=efectivo
final=merge(final,TRANSBANK,by ="TIPO_ARTICULO")
final=merge(final,OTRO,by ="TIPO_ARTICULO")
final=merge(final,CHEQUE,by ="TIPO_ARTICULO")
final=merge(final,TRANSFERENCIA,by ="TIPO_ARTICULO")



colnames(final)=c("TIPO_ARTICULO","PROB_EFECTIVO","PROB_TRANSBANK","PROB_OTRO")

View(final)

# 2. [5 puntos] Obtenga el valor unitario de los productos y realice un análisis descriptivo univariado de
# todas las columnas. Puede utilizar "EDAs automáticos" para simplificar la programación

VALOR_ARTICULO= data.frame(Variable="VALOR_ARTICULO",
                           Media=mean(ART$VALOR_ARTICULO,na.rm=TRUE),
                           Mediana=median(ART$VALOR_ARTICULO,na.rm=TRUE),
                           Moda_max=max(mfv(ART$VALOR_ARTICULO,na.rm=TRUE)),
                           Moda_min=min(mfv(ART$VALOR_ARTICULO,na.rm=TRUE)),
                           Desv_Stand=sd(ART$VALOR_ARTICULO, na.rm = TRUE),
                           Cuantil_25=quantile(ART$VALOR_ARTICULO,prob = 0.25, na.rm = TRUE),
                           Cuantil_50=quantile(ART$VALOR_ARTICULO,prob = 0.5, na.rm = TRUE),
                           Cuantil_75=quantile(ART$VALOR_ARTICULO,prob = 0.75, na.rm = TRUE),
                           Coef_Simetria=skew(ART$VALOR_ARTICULO, na.rm = TRUE),
                           Kurtosis=kurtosi(ART$VALOR_ARTICULO, na.rm = TRUE),
                           NA_values=sum(is.na(ART$VALOR_ARTICULO)),
                           Máximo=max(ART$VALOR_ARTICULO, na.rm = TRUE),
                           Mínimo=min(ART$VALOR_ARTICULO, na.rm = TRUE)
)

MARGEN= data.frame(Variable="MARGEN",
  Media=mean(ART$MARGEN,na.rm=TRUE),
                   Mediana=median(ART$MARGEN,na.rm=TRUE),
                   Moda_max=max(mfv(ART$MARGEN,na.rm=TRUE)),
                   Moda_min=min(mfv(ART$MARGEN,na.rm=TRUE)),
                   Desv_Stand=sd(ART$MARGEN, na.rm = TRUE),
                   Cuantil_25=quantile(ART$MARGEN,prob = 0.25, na.rm = TRUE),
                   Cuantil_50=quantile(ART$MARGEN,prob = 0.5, na.rm = TRUE),
                   Cuantil_75=quantile(ART$MARGEN,prob = 0.75, na.rm = TRUE),
                   Coef_Simetria=skew(ART$MARGEN, na.rm = TRUE),
                   Kurtosis=kurtosi(ART$MARGEN, na.rm = TRUE),
                   NA_values=sum(is.na(ART$MARGEN)),
                   Máximo=max(ART$MARGEN, na.rm = TRUE),
                   Mínimo=min(ART$MARGEN, na.rm = TRUE)
)
CANTIDAD_ARTICULO= data.frame(Variable="CANTIDAD_ARTICULO",
  Media=mean(ART$CANTIDAD_ARTICULO,na.rm=TRUE),
                              Mediana=median(ART$CANTIDAD_ARTICULO,na.rm=TRUE),
                              Moda_max=max(mfv(ART$CANTIDAD_ARTICULO,na.rm=TRUE)),
                              Moda_min=min(mfv(ART$CANTIDAD_ARTICULO,na.rm=TRUE)),
                              Desv_Stand=sd(ART$CANTIDAD_ARTICULO, na.rm = TRUE),
                              Cuantil_25=quantile(ART$CANTIDAD_ARTICULO,prob = 0.25, na.rm = TRUE),
                              Cuantil_50=quantile(ART$CANTIDAD_ARTICULO,prob = 0.5, na.rm = TRUE),
                              Cuantil_75=quantile(ART$CANTIDAD_ARTICULO,prob = 0.75, na.rm = TRUE),
                              Coef_Simetria=skew(ART$CANTIDAD_ARTICULO, na.rm = TRUE),
                              Kurtosis=kurtosi(ART$CANTIDAD_ARTICULO, na.rm = TRUE),
                              NA_values=sum(is.na(ART$CANTIDAD_ARTICULO)),
                              Máximo=max(ART$CANTIDAD_ARTICULO, na.rm = TRUE),
                              Mínimo=min(ART$CANTIDAD_ARTICULO, na.rm = TRUE)
)


MONTO_PAGADO= data.frame(Variable="MONTO_PAGADO",
  Media=mean(ART$MONTO_PAGADO,na.rm=TRUE),
                         Mediana=median(ART$MONTO_PAGADO,na.rm=TRUE),
                         Moda_max=max(mfv(ART$MONTO_PAGADO,na.rm=TRUE)),
                         Moda_min=min(mfv(ART$MONTO_PAGADO,na.rm=TRUE)),
                         Desv_Stand=sd(ART$MONTO_PAGADO, na.rm = TRUE),
                         Cuantil_25=quantile(ART$MONTO_PAGADO,prob = 0.25, na.rm = TRUE),
                         Cuantil_50=quantile(ART$MONTO_PAGADO,prob = 0.5, na.rm = TRUE),
                         Cuantil_75=quantile(ART$MONTO_PAGADO,prob = 0.75, na.rm = TRUE),
                         Coef_Simetria=skew(ART$MONTO_PAGADO, na.rm = TRUE),
                         Kurtosis=kurtosi(ART$MONTO_PAGADO, na.rm = TRUE),
                         NA_values=sum(is.na(ART$MONTO_PAGADO)),
                         Máximo=max(ART$MONTO_PAGADO, na.rm = TRUE),
                         Mínimo=min(ART$MONTO_PAGADO, na.rm = TRUE)
)
DESCUENTO= data.frame(Variable="DESCUENTO",
  Media=mean(ART$DESCUENTO,na.rm=TRUE),
                      Mediana=median(ART$DESCUENTO,na.rm=TRUE),
                      Moda_max=max(mfv(ART$DESCUENTO,na.rm=TRUE)),
                      Moda_min=min(mfv(ART$DESCUENTO,na.rm=TRUE)),
                      Desv_Stand=sd(ART$DESCUENTO, na.rm = TRUE),
                      Cuantil_25=quantile(ART$DESCUENTO,prob = 0.25, na.rm = TRUE),
                      Cuantil_50=quantile(ART$DESCUENTO,prob = 0.5, na.rm = TRUE),
                      Cuantil_75=quantile(ART$DESCUENTO,prob = 0.75, na.rm = TRUE),
                      Coef_Simetria=skew(ART$DESCUENTO, na.rm = TRUE),
                      Kurtosis=kurtosi(ART$DESCUENTO, na.rm = TRUE),
                      NA_values=sum(is.na(ART$DESCUENTO)),
                      Máximo=max(ART$DESCUENTO, na.rm = TRUE),
                      Mínimo=min(ART$DESCUENTO, na.rm = TRUE)
)
SUCURSAL= data.frame(Variable="SUCURSAL",
  Media=mean(ART$SUCURSAL,na.rm=TRUE),
                     Mediana=median(ART$SUCURSAL,na.rm=TRUE),
                     Moda_max=max(mfv(ART$SUCURSAL,na.rm=TRUE)),
                     Moda_min=min(mfv(ART$SUCURSAL,na.rm=TRUE)),
                     Desv_Stand=sd(ART$SUCURSAL, na.rm = TRUE),
                     Cuantil_25=quantile(ART$SUCURSAL,prob = 0.25, na.rm = TRUE),
                     Cuantil_50=quantile(ART$SUCURSAL,prob = 0.5, na.rm = TRUE),
                     Cuantil_75=quantile(ART$SUCURSAL,prob = 0.75, na.rm = TRUE),
                     Coef_Simetria=skew(ART$SUCURSAL, na.rm = TRUE),
                     Kurtosis=kurtosi(ART$SUCURSAL, na.rm = TRUE),
                     NA_values=sum(is.na(ART$SUCURSAL)),
                     Máximo=max(ART$SUCURSAL, na.rm = TRUE),
                     Mínimo=min(ART$SUCURSAL, na.rm = TRUE)
)

ACCOUNTID= data.frame(Variable="ACCOUNTID",
  Media=mean(ART$ACCOUNTID,na.rm=TRUE),
                      Mediana=median(ART$ACCOUNTID,na.rm=TRUE),
                      Moda_max=max(mfv(ART$ACCOUNTID,na.rm=TRUE)),
                      Moda_min=min(mfv(ART$ACCOUNTID,na.rm=TRUE)),
                      Desv_Stand=sd(ART$ACCOUNTID, na.rm = TRUE),
                      Cuantil_25=quantile(ART$ACCOUNTID,prob = 0.25, na.rm = TRUE),
                      Cuantil_50=quantile(ART$ACCOUNTID,prob = 0.5, na.rm = TRUE),
                      Cuantil_75=quantile(ART$ACCOUNTID,prob = 0.75, na.rm = TRUE),
                      Coef_Simetria=skew(ART$ACCOUNTID, na.rm = TRUE),
                      Kurtosis=kurtosi(ART$ACCOUNTID, na.rm = TRUE),
                      NA_values=sum(is.na(ART$ACCOUNTID)),
                      Máximo=max(ART$ACCOUNTID, na.rm = TRUE),
                      Mínimo=min(ART$ACCOUNTID, na.rm = TRUE)
)

#punto2=merge(VALOR_ARTICULO,MARGEN,,MONTO_PAGADO,DESCUENTO,SUCURSAL,ACCOUNTID)
#TIPO_ARTICULO_RESUMEN,FECHA_COMPRA_RESUMEN,METODO_DE_PAGO
punto2=VALOR_ARTICULO
colnames(punto2)
punto2 <- as.list(punto2)

# Add the new row MARGEN
punto2$Variable <- rbind(punto2$Variable, CANTIDAD_ARTICULO$Variable)
punto2$Media <- rbind(punto2$Media, CANTIDAD_ARTICULO$Media)
punto2$Mediana <- rbind(punto2$Mediana, CANTIDAD_ARTICULO$Mediana)
punto2$Moda_max <- rbind(punto2$Moda_max, CANTIDAD_ARTICULO$Moda_max)
punto2$Moda_min  <- rbind(punto2$Moda_min , CANTIDAD_ARTICULO$Moda_min )
punto2$Desv_Stand  <- rbind(punto2$Desv_Stand , CANTIDAD_ARTICULO$Desv_Stand )
punto2$Cuantil_25   <- rbind(punto2$Cuantil_25  , CANTIDAD_ARTICULO$Cuantil_25  )
punto2$Cuantil_50   <- rbind(punto2$Cuantil_50  , CANTIDAD_ARTICULO$Cuantil_50  )
punto2$Cuantil_75   <- rbind(punto2$Cuantil_75  , CANTIDAD_ARTICULO$Cuantil_75  )
punto2$Coef_Simetria    <- rbind(punto2$Coef_Simetria   , CANTIDAD_ARTICULO$Coef_Simetria   )
punto2$Kurtosis    <- rbind(punto2$Kurtosis   , CANTIDAD_ARTICULO$Kurtosis   )
punto2$NA_values    <- rbind(punto2$NA_values   , CANTIDAD_ARTICULO$NA_values   )
punto2$Máximo     <- rbind(punto2$Máximo    , CANTIDAD_ARTICULO$Máximo    )
punto2$Mínimo    <- rbind(punto2$Mínimo   , CANTIDAD_ARTICULO$Mínimo   )
punto2$Variable <- rbind(punto2$Variable, MARGEN$Variable)
punto2$Media <- rbind(punto2$Media, MARGEN$Media)
punto2$Mediana <- rbind(punto2$Mediana, MARGEN$Mediana)
punto2$Moda_max <- rbind(punto2$Moda_max, MARGEN$Moda_max)
punto2$Moda_min  <- rbind(punto2$Moda_min , MARGEN$Moda_min )
punto2$Desv_Stand  <- rbind(punto2$Desv_Stand , MARGEN$Desv_Stand )
punto2$Cuantil_25   <- rbind(punto2$Cuantil_25  , MARGEN$Cuantil_25  )
punto2$Cuantil_50   <- rbind(punto2$Cuantil_50  , MARGEN$Cuantil_50  )
punto2$Cuantil_75   <- rbind(punto2$Cuantil_75  , MARGEN$Cuantil_75  )
punto2$Coef_Simetria    <- rbind(punto2$Coef_Simetria   , MARGEN$Coef_Simetria   )
punto2$Kurtosis    <- rbind(punto2$Kurtosis   , MARGEN$Kurtosis   )
punto2$NA_values    <- rbind(punto2$NA_values   , MARGEN$NA_values   )
punto2$Máximo     <- rbind(punto2$Máximo    , MARGEN$Máximo    )
punto2$Mínimo    <- rbind(punto2$Mínimo   , MARGEN$Mínimo   )
punto2$Variable <- rbind(punto2$Variable, TIPO_ARTICULO$Variable)
punto2$Media <- rbind(punto2$Media, TIPO_ARTICULO$Media)
punto2$Mediana <- rbind(punto2$Mediana, TIPO_ARTICULO$Mediana)
punto2$Moda_max <- rbind(punto2$Moda_max, TIPO_ARTICULO$Moda_max)
punto2$Moda_min  <- rbind(punto2$Moda_min , TIPO_ARTICULO$Moda_min )
punto2$Desv_Stand  <- rbind(punto2$Desv_Stand , TIPO_ARTICULO$Desv_Stand )
punto2$Cuantil_25   <- rbind(punto2$Cuantil_25  , TIPO_ARTICULO$Cuantil_25  )
punto2$Cuantil_50   <- rbind(punto2$Cuantil_50  , TIPO_ARTICULO$Cuantil_50  )
punto2$Cuantil_75   <- rbind(punto2$Cuantil_75  , TIPO_ARTICULO$Cuantil_75  )
punto2$Coef_Simetria    <- rbind(punto2$Coef_Simetria   , TIPO_ARTICULO$Coef_Simetria   )
punto2$Kurtosis    <- rbind(punto2$Kurtosis   , TIPO_ARTICULO$Kurtosis   )
punto2$NA_values    <- rbind(punto2$NA_values   , TIPO_ARTICULO$NA_values   )
punto2$Máximo     <- rbind(punto2$Máximo    , TIPO_ARTICULO$Máximo    )
punto2$Mínimo    <- rbind(punto2$Mínimo   , TIPO_ARTICULO$Mínimo   )
punto2$Variable <- rbind(punto2$Variable, MONTO_PAGADO$Variable)
punto2$Media <- rbind(punto2$Media, MONTO_PAGADO$Media)
punto2$Mediana <- rbind(punto2$Mediana, MONTO_PAGADO$Mediana)
punto2$Moda_max <- rbind(punto2$Moda_max, MONTO_PAGADO$Moda_max)
punto2$Moda_min  <- rbind(punto2$Moda_min , MONTO_PAGADO$Moda_min )
punto2$Desv_Stand  <- rbind(punto2$Desv_Stand , MONTO_PAGADO$Desv_Stand )
punto2$Cuantil_25   <- rbind(punto2$Cuantil_25  , MONTO_PAGADO$Cuantil_25  )
punto2$Cuantil_50   <- rbind(punto2$Cuantil_50  , MONTO_PAGADO$Cuantil_50  )
punto2$Cuantil_75   <- rbind(punto2$Cuantil_75  , MONTO_PAGADO$Cuantil_75  )
punto2$Coef_Simetria    <- rbind(punto2$Coef_Simetria   , MONTO_PAGADO$Coef_Simetria   )
punto2$Kurtosis    <- rbind(punto2$Kurtosis   , MONTO_PAGADO$Kurtosis   )
punto2$NA_values    <- rbind(punto2$NA_values   , MONTO_PAGADO$NA_values   )
punto2$Máximo     <- rbind(punto2$Máximo    , MONTO_PAGADO$Máximo    )
punto2$Mínimo    <- rbind(punto2$Mínimo   , MONTO_PAGADO$Mínimo   )
punto2$Variable <- rbind(punto2$Variable, DESCUENTO$Variable)
punto2$Media <- rbind(punto2$Media, DESCUENTO$Media)
punto2$Mediana <- rbind(punto2$Mediana, DESCUENTO$Mediana)
punto2$Moda_max <- rbind(punto2$Moda_max, DESCUENTO$Moda_max)
punto2$Moda_min  <- rbind(punto2$Moda_min , DESCUENTO$Moda_min )
punto2$Desv_Stand  <- rbind(punto2$Desv_Stand , DESCUENTO$Desv_Stand )
punto2$Cuantil_25   <- rbind(punto2$Cuantil_25  , DESCUENTO$Cuantil_25  )
punto2$Cuantil_50   <- rbind(punto2$Cuantil_50  , DESCUENTO$Cuantil_50  )
punto2$Cuantil_75   <- rbind(punto2$Cuantil_75  , DESCUENTO$Cuantil_75  )
punto2$Coef_Simetria    <- rbind(punto2$Coef_Simetria   , DESCUENTO$Coef_Simetria   )
punto2$Kurtosis    <- rbind(punto2$Kurtosis   , DESCUENTO$Kurtosis   )
punto2$NA_values    <- rbind(punto2$NA_values   , DESCUENTO$NA_values   )
punto2$Máximo     <- rbind(punto2$Máximo    , DESCUENTO$Máximo    )
punto2$Mínimo    <- rbind(punto2$Mínimo   , DESCUENTO$Mínimo   )
punto2$Variable <- rbind(punto2$Variable, SUCURSAL$Variable)
punto2$Media <- rbind(punto2$Media, SUCURSAL$Media)
punto2$Mediana <- rbind(punto2$Mediana, SUCURSAL$Mediana)
punto2$Moda_max <- rbind(punto2$Moda_max, SUCURSAL$Moda_max)
punto2$Moda_min  <- rbind(punto2$Moda_min , SUCURSAL$Moda_min )
punto2$Desv_Stand  <- rbind(punto2$Desv_Stand , SUCURSAL$Desv_Stand )
punto2$Cuantil_25   <- rbind(punto2$Cuantil_25  , SUCURSAL$Cuantil_25  )
punto2$Cuantil_50   <- rbind(punto2$Cuantil_50  , SUCURSAL$Cuantil_50  )
punto2$Cuantil_75   <- rbind(punto2$Cuantil_75  , SUCURSAL$Cuantil_75  )
punto2$Coef_Simetria    <- rbind(punto2$Coef_Simetria   , SUCURSAL$Coef_Simetria   )
punto2$Kurtosis    <- rbind(punto2$Kurtosis   , SUCURSAL$Kurtosis   )
punto2$NA_values    <- rbind(punto2$NA_values   , SUCURSAL$NA_values   )
punto2$Máximo     <- rbind(punto2$Máximo    , SUCURSAL$Máximo    )
punto2$Mínimo    <- rbind(punto2$Mínimo   , SUCURSAL$Mínimo   )
punto2$Variable <- rbind(punto2$Variable, ACCOUNTID$Variable)
punto2$Media <- rbind(punto2$Media, ACCOUNTID$Media)
punto2$Mediana <- rbind(punto2$Mediana, ACCOUNTID$Mediana)
punto2$Moda_max <- rbind(punto2$Moda_max, ACCOUNTID$Moda_max)
punto2$Moda_min  <- rbind(punto2$Moda_min , ACCOUNTID$Moda_min )
punto2$Desv_Stand  <- rbind(punto2$Desv_Stand , ACCOUNTID$Desv_Stand )
punto2$Cuantil_25   <- rbind(punto2$Cuantil_25  , ACCOUNTID$Cuantil_25  )
punto2$Cuantil_50   <- rbind(punto2$Cuantil_50  , ACCOUNTID$Cuantil_50  )
punto2$Cuantil_75   <- rbind(punto2$Cuantil_75  , ACCOUNTID$Cuantil_75  )
punto2$Coef_Simetria    <- rbind(punto2$Coef_Simetria   , ACCOUNTID$Coef_Simetria   )
punto2$Kurtosis    <- rbind(punto2$Kurtosis   , ACCOUNTID$Kurtosis   )
punto2$NA_values    <- rbind(punto2$NA_values   , ACCOUNTID$NA_values   )
punto2$Máximo     <- rbind(punto2$Máximo    , ACCOUNTID$Máximo    )
punto2$Mínimo    <- rbind(punto2$Mínimo   , ACCOUNTID$Mínimo   )
# Convert to data.frame
punto2 <- data.frame(punto2, stringsAsFactors = FALSE)

#---------------------------------------------PUNTO 2 variable de categoría
TIPO_ARTICULO= data.frame(table(ART$TIPO_ARTICULO))
colnames(TIPO_ARTICULO)=c("TIPO_ARTICULO","FREQ")
TIPO_ARTICULO_RESUMEN <- data.frame(t(TIPO_ARTICULO[-1]))
# Añadimos los nombres de las columnas
colnames(TIPO_ARTICULO_RESUMEN) <- TIPO_ARTICULO[, 1]

TIPO_ARTICULO_RESUMEN= TIPO_ARTICULO_RESUMEN %>% mutate(Máximo=max(TIPO_ARTICULO$FREQ, na.rm = TRUE),Mínimo=min(TIPO_ARTICULO$FREQ, na.rm = TRUE)
)
View(TIPO_ARTICULO_RESUMEN)
#---------------------------------------------FECHA_COMPRA
FECHA_COMPRA=data.frame(table(ART$FECHA_COMPRA))
colnames(FECHA_COMPRA)= c("FECHA_COMPRA","FREQ")

FECHA_COMPRA_RESUMEN <- data.frame(t(FECHA_COMPRA[-1]))
# Añadimos los nombres de las columnas
colnames(FECHA_COMPRA_RESUMEN) <- FECHA_COMPRA[, 1]

FECHA_COMPRA_RESUMEN= FECHA_COMPRA_RESUMEN %>% mutate(Máximo=max(FECHA_COMPRA$FREQ, na.rm = TRUE),Mínimo=min(FECHA_COMPRA$FREQ, na.rm = TRUE)
)
View(FECHA_COMPRA_RESUMEN)
#---------------------------------------------METODO_DE_PAGO
METODO_DE_PAGO= data.frame(table(ART$METODO_DE_PAGO))
colnames(METODO_DE_PAGO)=c("METODO_DE_PAGO","FREQ")

METODO_DE_PAGOA_RESUMEN <- data.frame(t(METODO_DE_PAGO[-1]))
# Añadimos los nombres de las columnas
colnames(METODO_DE_PAGOA_RESUMEN) <- METODO_DE_PAGO[, 1]

METODO_DE_PAGOA_RESUMEN= METODO_DE_PAGOA_RESUMEN %>% mutate(Máximo=max(METODO_DE_PAGO$FREQ, na.rm = TRUE),Mínimo=min(METODO_DE_PAGO$FREQ, na.rm = TRUE)
)
View(METODO_DE_PAGOA_RESUMEN)







write_xlsx(punto2,"C:/Users/fdono/Desktop/tallerFinalEstadística/punto2.xlsx")







unique(ART$VALOR_ARTICULO)



