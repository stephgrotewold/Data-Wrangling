library(dplyr)
library(ggplot2)
library(readxl)
library(stringr)
library(lubridate)

data <- read.csv("c1.csv")
str(data)

#Crear una tabla de frecuencia de los códigos
frecuencia_cod <- table(data$Cod)
frecuencia_cod

#Convertir la tabla de frecuencia en un data frame
df_frecuencia_cod <- as.data.frame(frecuencia_cod)
df_frecuencia_cod$Cod <- rownames(df_frecuencia_cod)
df_frecuencia_cod

#Crear el gráfico de barras con etiquetas inclinadas y color azul marino
ggplot(df_frecuencia_cod, aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  xlab("Códigos") +
  ylab("Frecuencia") +
  ggtitle("Frecuencia de Códigos") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#----*Tratamiento de Datos*----

#Reemplaza "Q-" por "0" en la columna Camion_5
data$Camion_5 <- sub("Q-", "Q0.00", data$Camion_5)
data$Camion_5 <- sub("Q", "", data$Camion_5)
#Convierte la columna a formato numérico
data$Camion_5 <- as.numeric(data$Camion_5)
#Suma los valores
sum_result_Camion_5 <- sum(data$Camion_5)
sum_result_Camion_5

#Reemplaza "Q-" por "0" en la columna Moto
data$Moto <- sub("Q-", "Q0.00", data$Moto)
data$Moto <- sub("Q", "", data$Moto)
#Convierte la columna a formato numérico
data$Moto <- as.numeric(data$Moto)
#Suma los valores
sum_result_moto <- sum(data$Moto)
sum_result_moto

#Reemplaza "Q-" por "0" en la columna Pickup
data$Pickup <- sub("Q-", "Q0.00", data$Pickup)
data$Pickup <- sub("Q", "", data$Pickup)
#Convierte la columna a formato numérico
data$Pickup <- as.numeric(data$Pickup)
#Suma los valores
sum_result_Pickup <- sum(data$Pickup)
sum_result_Pickup

#Reemplaza "Q-" por "0" en la columna directoCamion_5
data$directoCamion_5 <- sub("Q-", "Q0.00", data$directoCamion_5)
data$directoCamion_5 <- sub("Q", "", data$directoCamion_5)
#Convierte la columna a formato numérico
data$directoCamion_5 <- as.numeric(data$directoCamion_5)
#Suma los valores
sum_result_directoCamion_5 <- sum(data$directoCamion_5)
sum_result_directoCamion_5

#Reemplaza "Q-" por "0" en la columna directoMoto
data$directoMoto <- sub("Q-", "Q0.00", data$directoMoto)
data$directoMoto <- sub("Q", "", data$directoMoto)
#Convierte la columna a formato numérico
data$directoMoto <- as.numeric(data$directoMoto)
#Suma los valores
sum_result_directoMoto <- sum(data$directoMoto)
sum_result_directoMoto

#Reemplaza "Q-" por "0" en la columna directoPickup
data$directoPickup <- sub("Q-", "Q0.00", data$directoPickup)
data$directoPickup <- sub("Q", "", data$directoPickup)
#Convierte la columna a formato numérico
data$directoPickup <- as.numeric(data$directoPickup)
#Suma los valores
sum_result_directoPickup <- sum(data$directoPickup)
sum_result_directoPickup


#Reemplaza "Q-" por "0" en la columna fijoCamion_5
data$fijoCamion_5 <- sub("Q-", "Q0.00", data$fijoCamion_5)
data$fijoCamion_5 <- sub("Q", "", data$fijoCamion_5)
#Convierte la columna a formato numérico
data$fijoCamion_5 <- as.numeric(data$fijoCamion_5)
#Suma los valores
sum_result_fijoCamion_5 <- sum(data$fijoCamion_5)
sum_result_fijoCamion_5

#Reemplaza "Q-" por "0" en la columna fijoMoto
data$fijoMoto <- sub("Q-", "Q0.00", data$fijoMoto)
data$fijoMoto <- sub("Q", "", data$fijoMoto)
#Convierte la columna a formato numérico
data$fijoMoto <- as.numeric(data$fijoMoto)
#Suma los valores
sum_result_fijoMoto <- sum(data$fijoMoto)
sum_result_fijoMoto

#Reemplaza "Q-" por "0" en la columna fijoPickup
data$fijoPickup <- sub("Q-", "Q0.00", data$fijoPickup)
data$fijoPickup <- sub("Q", "", data$fijoPickup)
#Convierte la columna a formato numérico
data$fijoPickup <- as.numeric(data$fijoPickup)
#Suma los valores
sum_result_fijoPickup <- sum(data$fijoPickup)
sum_result_fijoPickup


data$factura <- sub("Q", "", data$factura)
#Convierte la columna a formato numérico
data$factura <- as.numeric(data$factura)

