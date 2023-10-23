library(dplyr)
library(ggplot2)
library(readxl)
library(stringr)
library(tidyverse)
library(lubridate)

data <- read.csv("c1.csv")
str(data)

#----*Tratamiento de Datos*----
data$Cod <- as.factor(data$Cod)

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

data <- data[, !names(data) %in% c("Long", "Lat")]
head(data)


#----*Exploracion*----
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

# Agrupa el conjunto de datos por la variable Cod y calcula la suma de facturas por tipo
facturas_por_tipo <- aggregate(data$factura, by=list(data$Cod), FUN=sum)
# Agrega nombres descriptivos a las columnas
colnames(facturas_por_tipo) <- c("Tipo", "Suma_Facturas")
# Muestra el resultado
facturas_por_tipo

frecuencia_origen <- table(data$Cod)
# Creamos un data frame con los resultados de la frecuencia
df_frecuencia_origen <- as.data.frame(frecuencia_origen)
# Nombramos las columnas para que sean más descriptivas
colnames(df_frecuencia_origen) <- c("Origen", "Frecuencia")

# Crear el gráfico de barras con barras de color darkgreen y recuento encima de las barras
ggplot(data = df_frecuencia_origen, aes(x = Origen, y = Frecuencia)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_text(aes(label = Frecuencia), vjust = -0.5, color = "black", size = 3) + 
  labs(title = "Frecuencia de Origen", x = "Origen", y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Calcular la tabla de frecuencia de la variable ID
tabla_frecuencia_ID <- table(data$ID)

# Convertir la tabla de frecuencia en un data frame para visualización
df_tabla_frecuencia_ID <- as.data.frame(tabla_frecuencia_ID)

# Renombrar las columnas para mayor claridad
colnames(df_tabla_frecuencia_ID) <- c("ID", "Frecuencia")

# Mostrar la tabla de frecuencia
print(df_tabla_frecuencia_ID)


# Filtrar los valores que se repiten al menos dos veces
tabla_frecuencia_ID_filtrada <- tabla_frecuencia_ID[tabla_frecuencia_ID >= 100]

# Convertir la tabla de frecuencia filtrada en un data frame para visualización
df_tabla_frecuencia_ID_filtrada <- as.data.frame(tabla_frecuencia_ID_filtrada)

# Renombrar las columnas para mayor claridad
colnames(df_tabla_frecuencia_ID_filtrada) <- c("ID", "Frecuencia")

# Ordenar la tabla filtrada por la columna "Frecuencia" en orden descendente
df_tabla_frecuencia_ID_filtrada_ordenada <- df_tabla_frecuencia_ID_filtrada[order(-df_tabla_frecuencia_ID_filtrada$Frecuencia), ]
df_tabla_frecuencia_ID_filtrada_ordenada




# Calcular la suma de facturas por ID
suma_facturas_por_ID <- aggregate(data$factura, by=list(data$ID), FUN=sum)

# Renombrar las columnas
colnames(suma_facturas_por_ID) <- c("ID", "Suma_Facturas")

# Ordenar el resultado en orden descendente por la columna "Suma_Facturas"
suma_facturas_por_ID_ordenada <- suma_facturas_por_ID[order(-suma_facturas_por_ID$Suma_Facturas), ]

# Tomar los 5 postes con las sumas de facturas más altas
top_5_postes <- head(suma_facturas_por_ID_ordenada, 5)
top_5_postes



#SUMA directos y fijos top 5

# Filtrar los datos originales para obtener solo las filas con los ID en top_5_postes
postes_seleccionados <- data[data$ID %in% top_5_postes$ID, ]

# Calcular la suma de los gastos fijos y directos para estos postes
suma_gastos_fijos <- sum(postes_seleccionados$fijoCamion_5 + postes_seleccionados$fijoPickup)
suma_gastos_directos <- sum(postes_seleccionados$directoCamion_5 + postes_seleccionados$directoPickup)

cat("Suma de Gastos Fijos de los postes seleccionados:", suma_gastos_fijos, "\n")
cat("Suma de Gastos Directos de los postes seleccionados:", suma_gastos_directos, "\n")



# Filtrar los datos originales para obtener solo las filas con los ID en top_5_postes
postes_seleccionados <- data[data$ID %in% top_5_postes$ID, ]

# Calcular la suma de gastos fijos y directos por poste
suma_gastos_por_poste <- aggregate(cbind(fijoCamion_5, fijoPickup,fijoMoto, directoCamion_5, directoPickup,directoMoto) ~ ID, data=postes_seleccionados, FUN=sum)

# Renombrar las columnas
colnames(suma_gastos_por_poste) <- c("ID", "Suma_FijoCamion_5", "Suma_FijoPickup","Suma_FijoMoto", "Suma_DirectoCamion_5", "Suma_DirectoPickup", "Suma_DirectoMoto")
suma_gastos_por_poste



# Calcular la suma total de gastos por poste (incluyendo gastos fijos y directos)
suma_gastos_por_poste$total_gastos <- rowSums(suma_gastos_por_poste[, c("Suma_FijoCamion_5", "Suma_FijoPickup", "Suma_FijoMoto", "Suma_DirectoCamion_5", "Suma_DirectoPickup", "Suma_DirectoMoto")])

# Encontrar el poste que gasta más
poste_mas_costoso <- suma_gastos_por_poste[which.max(suma_gastos_por_poste$total_gastos), ]

# Mostrar el resultado
print(poste_mas_costoso)




# Filtrar los datos solo para los top 5 postes
datos_top_5_postes <- data[data$ID %in% top_5_postes, ]
datos_top_5_postes

# Calcular la suma de gastos por poste (gastos fijos y directos)
suma_gastos_por_poste <- aggregate(datos_top_5_postes[, c("fijoCamion_5", "fijoPickup", "fijoMoto", "directoCamion_5", "directoPickup", "directoMoto")], by=list(datos_top_5_postes$ID), FUN=sum)

# Renombrar las columnas
colnames(suma_gastos_por_poste) <- c("ID", "Suma_FijoCamion_5", "Suma_FijoPickup", "Suma_FijoMoto", "Suma_DirectoCamion_5", "Suma_DirectoPickup", "Suma_DirectoMoto", "Total_gasto")

# Mostrar el resultado
print(suma_gastos_por_poste)


# Seleccionar los IDs de los top 5 postes
top_5_IDs <- top_5_postes$ID

# Filtrar la tabla de suma de gastos por poste para incluir solo los top 5 postes
top_5_gastos <- suma_gastos_por_poste[suma_gastos_por_poste$ID %in% top_5_IDs, ]

# Combinar la información de suma de gastos con la de suma de facturas
comparacion_top_5 <- merge(top_5_gastos, top_5_postes, by="ID")

# Calcular el total de gastos
comparacion_top_5$Total_gasto <- rowSums(comparacion_top_5[, 2:7])

# Mostrar la tabla de comparación
print(comparacion_top_5)


# Calcular la ganancia (Profit)
comparacion_top_5$Profit <- comparacion_top_5$Suma_Facturas - comparacion_top_5$Total_gasto

# Mostrar la tabla con la ganancia
print(comparacion_top_5)




#Tipos de Vehiculos
# Agrupar por el campo "Cod" (tipo de vehículo) y calcular las sumas de costos directos, fijos y totales
costos_por_tipo <- aggregate(data[, c("directoCamion_5", "directoPickup", "directoMoto", "fijoCamion_5", "fijoPickup", "fijoMoto")],
                             by=list(data$Cod), FUN=sum)

# Agregar nombres descriptivos a las columnas
colnames(costos_por_tipo) <- c("Tipo", "Total_DirectoCamion_5", "Total_DirectoPickup", "Total_DirectoMoto", 
                               "Total_FijoCamion_5", "Total_FijoPickup", "Total_FijoMoto")

# Calcular los costos totales sumando costos directos y costos fijos
costos_por_tipo$Total_Camion_5 <- costos_por_tipo$Total_DirectoCamion_5 + costos_por_tipo$Total_FijoCamion_5
costos_por_tipo$Total_Pickup <- costos_por_tipo$Total_DirectoPickup + costos_por_tipo$Total_FijoPickup
costos_por_tipo$Total_Moto <- costos_por_tipo$Total_DirectoMoto + costos_por_tipo$Total_FijoMoto
costos_por_tipo

cst_camion <- sum(costos_por_tipo$Total_Camion_5)
cst_camion
cst_pickup <- sum(costos_por_tipo$Total_Pickup)
cst_pickup
cst_moto <-sum(costos_por_tipo$Total_Moto)

# Crear un data frame con los datos
costos_por_tipo <- data.frame(
  Tipo = c("Camion_5", "Pickup", "Moto"),
  Costo_Total = c(
    cst_camion, cst_pickup, cst_moto
  )
)
costos_por_tipo

# Crear el gráfico de barras

ggplot(costos_por_tipo, aes(x = Tipo, y = Costo_Total, fill = Tipo)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Costo_Total), vjust = -0.5, size = 3) +  # Agregar etiquetas de texto
  labs(title = "Costos Totales por Tipo de Vehículo", y = "Costo Total") +
  scale_fill_manual(values = c("darkblue", "darkgreen", "darkred")) +
  scale_y_continuous(labels = scales::number_format(scale = 1e-6)) +  # Desactivar notación científica
  theme_minimal()


# Calcular la suma de los costos directos para la categoría Pickup
suma_directo_pickup <- sum(data$directoPickup)
suma_directo_pickup
# Calcular la suma de los costos fijos para la categoría Pickup
suma_fijo_pickup <- sum(data$fijoPickup)
suma_fijo_pickup


# Calcular la suma de los costos directos para la categoría 
suma_directo_moto <- sum(data$directoMoto)
suma_directo_moto
# Calcular la suma de los costos fijos para la categoría
suma_fijo_moto <- sum(data$fijoMoto)
suma_fijo_moto


# Calcular la suma de los costos directos para la categoría 
suma_directo_camion <- sum(data$directoCamion_5)
suma_directo_camion
# Calcular la suma de los costos fijos para la categoría
suma_fijo_camion <- sum(data$fijoCamion_5)
suma_fijo_camion


# Crear un marco de datos para las sumas
costos_sumarios <- data.frame(
  Categoria = c("Pickup", "Moto", "Camion_5"),
  Suma_Directo = c(
    sum(data$directoPickup),
    sum(data$directoMoto),
    sum(data$directoCamion_5)
  ),
  Suma_Fijo = c(
    sum(data$fijoPickup),
    sum(data$fijoMoto),
    sum(data$fijoCamion_5)
  )
)

costos_sumarios$Suma_Total_Costos <- costos_sumarios$Suma_Directo + costos_sumarios$Suma_Fijo
costos_sumarios




#PROFIT por vehiculo
# Crear una nueva columna "Categoria" en función de las observaciones de Moto, Camion_5 y Pickup
data$Categoria <- ifelse(!is.na(data$Moto), "Moto", 
                         ifelse(!is.na(data$Camion_5), "Camion_5", 
                                ifelse(!is.na(data$Pickup), "Pickup", NA)))

# Calcular la suma de facturación por tipo de vehículo
suma_facturas_moto <- sum(data$factura[data$Moto > 0])
suma_facturas_camion <- sum(data$factura[data$Camion_5 > 0])
suma_facturas_pickup <- sum(data$factura[data$Pickup > 0])

# Crear un nuevo data frame
facturacion_por_tipo <- data.frame(
  Tipo = c("Moto", "Camion_5", "Pickup"),
  Suma_Facturacion = c(suma_facturas_moto, suma_facturas_camion, suma_facturas_pickup)
)
facturacion_por_tipo

resultados <- merge(facturacion_por_tipo, costos_sumarios, by="Categoria", all=TRUE)
resultados$Profit <- round(resultados$Suma_Facturacion - resultados$Suma_Total_Costos,2)
resultados




# Crear una nueva columna "Tipo" en función de las observaciones de Moto, Camion_5 y Pickup
data$Tipo <- ifelse(!is.na(data$Moto), "Moto", 
                    ifelse(!is.na(data$Camion_5), "Camion_5", 
                           ifelse(!is.na(data$Pickup), "Pickup", NA)))

# Calcular la suma de facturación por tipo de vehículo
suma_facturas_moto <- sum(data$factura[data$Moto > 0])
suma_facturas_camion <- sum(data$factura[data$Camion_5 > 0])
suma_facturas_pickup <- sum(data$factura[data$Pickup > 0])

# Crear un nuevo data frame
facturacion_por_tipo <- data.frame(
  Tipo = c("Moto", "Camion_5", "Pickup"),
  Suma_Facturacion = c(suma_facturas_moto, suma_facturas_camion, suma_facturas_pickup)
)

# Calcular la suma total de costos
costos_sumarios$Suma_Total_Costos <- costos_sumarios$Suma_Directo + costos_sumarios$Suma_Fijo

# Combinar los datos de facturación y costos
resultados <- merge(facturacion_por_tipo, costos_sumarios, by.x = "Tipo", by.y = "Categoria", all = TRUE)

# Calcular el Profit
resultados$Profit <- resultados$Suma_Facturacion - resultados$Suma_Total_Costos
resultados


# Crear un gráfico de barras
ggplot(resultados, aes(x = Tipo)) +
  geom_bar(aes(y = Suma_Facturacion, fill = "Facturación"), stat = "identity") +
  geom_bar(aes(y = Suma_Total_Costos, fill = "Costos"), stat = "identity") +
  labs(title = "Suma de Facturación y Costos por Tipo de Vehículo", y = "Valor") +
  scale_fill_manual(values = c("Facturación" = "blue", "Costos" = "red")) +
  theme_minimal()

# Crear una tabla de frecuencia de la columna "Tipo"
frecuencia_tipos <- table(data$Tipo)

# Convertir la tabla de frecuencia en un data frame
df_frecuencia_tipos <- as.data.frame(frecuencia_tipos)

# Nombrar las columnas para que sean más descriptivas
colnames(df_frecuencia_tipos) <- c("Tipo", "Frecuencia")

# Mostrar la tabla de frecuencia
print(df_frecuencia_tipos)







# Crear una tabla de frecuencia de la variable "origen"
frecuencia_origen <- table(data$origen)

# Convertir la tabla de frecuencia en un data frame para mejor visualización
df_frecuencia_origen <- as.data.frame(frecuencia_origen)

# Renombrar las columnas para mayor claridad
colnames(df_frecuencia_origen) <- c("Origen", "Frecuencia")

# Mostrar la tabla de frecuencia
print(df_frecuencia_origen)



# Supongamos que tienes un dataframe llamado 'data' con las columnas 'Cod', 'Moto', 'Camion_5' y 'Pickup'
# Para saber qué códigos se asocian con qué vehículos, puedes utilizar dplyr para filtrar las combinaciones
resultados <- data %>%
  filter(Moto > 0) %>% 
  select(Cod, Moto) %>% 
  mutate(Vehiculo = "Moto") %>%
  bind_rows(
    data %>%
      filter(Camion_5 > 0) %>% 
      select(Cod, Camion_5) %>% 
      mutate(Vehiculo = "Camion_5")
  ) %>%
  bind_rows(
    data %>%
      filter(Pickup > 0) %>% 
      select(Cod, Pickup) %>% 
      mutate(Vehiculo = "Pickup")
  )

# Muestra los resultados
print(resultados)



# Calcular la frecuencia de los códigos por vehículo
frecuencia_por_vehiculo <- data %>%
  select(Cod, Moto, Camion_5, Pickup) %>%
  pivot_longer(cols = -Cod, names_to = "Vehiculo", values_to = "Valor") %>%
  filter(Valor > 0) %>%
  group_by(Vehiculo, Cod) %>%
  summarize(Frecuencia = n())

# Muestra los resultados
print(frecuencia_por_vehiculo,n=21)

# Crear un gráfico de barras
ggplot(frecuencia_por_vehiculo, aes(x = Vehiculo, y = Frecuencia, fill = Cod)) +
  geom_bar(stat = "identity") +
  labs(title = "Frecuencia de Códigos por Vehículo", y = "Frecuencia") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() 



# Crear una nueva columna "Tipo" basada en las facturas
data$Tipo <- ifelse(data$Moto > 0 & !is.na(data$Moto), "Moto", 
                    ifelse(data$Camion_5 > 0 & !is.na(data$Camion_5), "Camion_5",
                           ifelse(data$Pickup > 0 & !is.na(data$Pickup), "Pickup", NA)))

# Verificar la estructura de la nueva columna "Tipo"
table(data$Tipo)


# Calcular la suma de costos fijos y directos por tipo de vehículo
costos_por_tipo <- data %>%
  group_by(Tipo) %>%
  summarize(Suma_Fijo = sum(fijoCamion_5 + fijoPickup + fijoMoto),
            Suma_Directo = sum(directoCamion_5 + directoPickup + directoMoto)
            , Suma_Total = Suma_Fijo + Suma_Directo)

# Mostrar la tabla con los resultados
print(costos_por_tipo)




# Filtrar las observaciones correspondientes a cambios de fusible
cambios_fusible <- data %>%
  filter(Cod == "CAMBIO_FUSIBLE")

# Calcular la suma de costos fijos y directos por tipo de vehículo
costos_cambios_fusible <- cambios_fusible %>%
  group_by(Tipo) %>%
  summarize(
    Suma_Fijo = sum(fijoCamion_5 + fijoPickup + fijoMoto),
    Suma_Directo = sum(directoCamion_5 + directoPickup + directoMoto),
    Suma_Total = Suma_Fijo + Suma_Directo,
    n = n()
  )
costos_cambios_fusible

# Crear el gráfico de barras con etiquetas "n"
ggplot(costos_cambios_fusible, aes(x = Tipo, y = Suma_Total, fill = Tipo)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5, size = 4) +  # Agregar etiquetas "n"
  labs(title = "Costo de Cambios de Fusible por Tipo de Vehículo", y = "Costo Total") +
  scale_fill_manual(values = c("darkblue", "darkgreen", "darkred")) +
  theme_minimal()





# Crear un nuevo dataframe con la suma de las columnas por origen
suma_por_origen <- aggregate(data[c("Camion_5", "Moto", "Pickup")], by=list(data$origen), FUN=sum)

# Renombrar las columnas
colnames(suma_por_origen) <- c("Origen", "Suma_Camion_5", "Suma_Moto", "Suma_Pickup")

# Mostrar el dataframe con la suma por origen
print(suma_por_origen)




# Crear un nuevo dataframe con la suma de facturación por origen
suma_facturacion_por_origen <- aggregate(data$factura, by=list(data$origen), FUN=sum)

# Renombrar las columnas
colnames(suma_facturacion_por_origen) <- c("Origen", "Suma_Facturacion")

# Mostrar el dataframe con la suma de facturación por origen
print(suma_facturacion_por_origen)


# Unir las tablas "suma_por_origen" y "suma_facturacion_por_origen" por la columna "Origen"
tabla_unida <- merge(suma_por_origen, suma_facturacion_por_origen, by="Origen", all=TRUE)

# Calcular el "Profit" restando la suma de costos de Camion_5, Moto y Pickup de la suma de facturación
tabla_unida$Profit <- tabla_unida$Suma_Facturacion - (tabla_unida$Suma_Camion_5 + tabla_unida$Suma_Moto + tabla_unida$Suma_Pickup)

# Mostrar la tabla final con el "Profit" por origen
print(tabla_unida)



tabla_unida <- tabla_unida %>%
  mutate(Suma_Costos = Suma_Camion_5 + Suma_Moto + Suma_Pickup)
tabla_unida




# Crear una tabla de frecuencia para la columna 'Origen'
tabla_frecuencia <- table(data$origen)
tabla_frecuencia
# Convertir la tabla de frecuencia en un dataframe
df_frecuencia <- as.data.frame(tabla_frecuencia)

# Renombrar las columnas
colnames(df_frecuencia) <- c("Origen", "Frecuencia")




# Crear un gráfico de barras con valores de "n"
ggplot(df_frecuencia, aes(x = Origen, y = Frecuencia)) +
  geom_bar(stat = "identity", fill = "pink") +
  geom_text(aes(label = Frecuencia), vjust = -0.5, size = 4) +  # Agregar etiquetas de frecuencia
  labs(title = "Frecuencia por Origen", x = "Origen", y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


