library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

#leer archivo
coffee <- read_xlsx("datos_combinados.xlsx")
head(coffee)
summary(coffee)
str(coffee)  

#Tratamiento de variables
#Crear nuevas variables MONTH y YEAR, y eliminar la columna Fecha
coffee2 <- coffee %>% 
  mutate(MONTH = substr(Fecha, 1, 2),
         YEAR = substr(Fecha, 4, 7)) %>% 
  select(-Fecha)

#Convertir MONTH y UNIDAD a factores
coffee2 = transform(coffee2, 
                    MONTH = as.factor(MONTH),
                    UNIDAD = as.factor(UNIDAD))

#Separar la columna CLIENTE en CLIENTE y OBSERVACION, y convertir OBSERVACION a mayúsculas
coffee3 <- coffee2 %>%
  separate(CLIENTE, into = c("CLIENTE", "OBSERVACION"), sep = "[/|]+", extra = "drop") %>%
  mutate(OBSERVACION = toupper(OBSERVACION))

#Rellenar los valores faltantes en OBSERVACION y convertir OBSERVACION a factor
coffee3$OBSERVACION[is.na(coffee3$OBSERVACION)] <- "COMPLETO"
coffee3 = transform(coffee3, 
                    OBSERVACION = as.factor(OBSERVACION))

#Eliminar espacios en blanco al inicio y final de las variables
coffee3 <- coffee3 %>%
  mutate(OBSERVACION = str_trim(OBSERVACION),
         CLIENTE = str_trim(CLIENTE))


#---------*Hipótesis 1*-----------

#Contar la frecuencia de observaciones en la columna OBSERVACION
coffee3 %>%
  count(OBSERVACION)

#Calcular el porcentaje manualmente utilizando la cantidad de observaciones
#en "FALTANTE" y el total de observaciones
(588/2180)*100

#Filtrar y agrupar los datos para identificar clientes con observación "FALTANTE"
clientes_con_faltante <- coffee3 %>%
  filter(OBSERVACION == "FALTANTE") %>%
  group_by(CLIENTE) %>%
  summarise(total_faltante = n())
clientes_con_faltante

#Sumar el número de observaciones "FALTANTE" en toda la columna
sum(coffee3$OBSERVACION == "FALTANTE", na.rm = TRUE)

#Filtrar y agrupar los datos para identificar clientes con observación "FALTANTE"
#y la cantidad total de "FALTANTE" por unidad
clientes_faltantes_unidades <- coffee3 %>%
  filter(OBSERVACION == "FALTANTE") %>%
  group_by(CLIENTE, UNIDAD) %>%
  summarise(total_faltante = n())
clientes_faltantes_unidades

#Filtrar y agrupar los datos para identificar clientes con observación "DESPACHO A CLIENTE"
clientes_con_despacho <- coffee3 %>%
  filter(OBSERVACION == "DESPACHO A CLIENTE") %>%
  group_by(CLIENTE) %>%
  summarise(total_despacho = n())
clientes_con_despacho

#Crear un gráfico de barras para mostrar la cantidad de faltantes por tipo de unidad
ggplot(clientes_faltantes_unidades, aes(x = UNIDAD, y = total_faltante, fill = UNIDAD)) +
  geom_bar(stat = "identity") +
  labs(title = "Cantidad de Faltantes por Tipo de Unidad",
       x = "Tipo de Unidad",
       y = "Cantidad de Faltantes") +
  theme_minimal() +
  scale_fill_discrete(name = "Tipo de Unidad")

#Calcular y mostrar la suma de faltantes para la unidad "Camion Pequeño"
sum(clientes_faltantes_unidades$total_faltante[clientes_faltantes_unidades$UNIDAD == "Camion Pequeño"])
#Calcular y mostrar la suma de faltantes para la unidad "Panel"
sum(clientes_faltantes_unidades$total_faltante[clientes_faltantes_unidades$UNIDAD == "Panel"])
#Calcular y mostrar la suma de faltantes para la unidad "Camion Grande"
sum(clientes_faltantes_unidades$total_faltante[clientes_faltantes_unidades$UNIDAD == "Camion Grande"])

#Calcular la capacidad total por unidad sumando la columna CANTIDAD
capacidad_por_unidad_total <- coffee3 %>%
  group_by(UNIDAD) %>%
  summarize(capacidad_total = sum(CANTIDAD, na.rm = TRUE))
capacidad_por_unidad_total

#Definir valores para el cálculo de la capacidad por unidad por día
num_meses <- 11
dias_semana <- 6
dias_mes <- 30
viajes_por_dia <- 8

#Calcular la capacidad por unidad por día dividiendo la capacidad total entre el tiempo
capacidad_por_unidad_dia <- coffee3 %>%
  group_by(UNIDAD) %>%
  summarize(capacidad_por_dia = sum(CANTIDAD, na.rm = TRUE) / (num_meses * dias_semana * dias_mes * viajes_por_dia))
capacidad_por_unidad_dia

#CAMION GRANDE

#Filtrar los viajes que corresponden a la unidad "Camion Grande"
viajes_camion_grande <- coffee3 %>%
  filter(UNIDAD == "Camion Grande")

#Contar el número de viajes de camiones grandes
num_viajes_camion_grande <- nrow(viajes_camion_grande)
num_viajes_camion_grande

#Calcular la capacidad total de un camión grande en unidades
capacidad_camion_grande <- sum(viajes_camion_grande$CANTIDAD, na.rm = TRUE)
capacidad_camion_grande

#Calcular la capacidad promedio por viaje de un camión grande en unidades
average_capacidad_x_viaje_camion_grande <- capacidad_camion_grande / num_viajes_camion_grande
average_capacidad_x_viaje_camion_grande

#Calcular el número total de viajes de camiones grandes necesarios para el total de unidades entregadas
viajes_CG_total <- sum(coffee3$CANTIDAD, na.rm = TRUE) / average_capacidad_x_viaje_camion_grande
viajes_CG_total

#Calcular el número total de viajes en los datos
total_viaje <- nrow(coffee3)
total_viaje

#Calcular la diferencia entre el número total de viajes y los viajes de camiones grandes
dif_viajes <- total_viaje - viajes_CG_total
dif_viajes

#Calcular el porcentaje de diferencia entre los viajes de camiones grandes y el total de viajes
porcentaje <- (dif_viajes / total_viaje) * 100
porcentaje

#Contar la cantidad de observaciones por unidad utilizando dplyr
conteo_por_unidad <- coffee3 %>% 
  group_by(UNIDAD) %>% 
  count()

#---------*Hipotesis 2*-----------
#Definición de variables
num_meses <- 11
dias_semana <- 5
semanas <- 4

#Cálculo de total de viajes dividido por 9 (esto parece un cálculo intermedio)
total_viaje / 9

#Cálculo de total de viajes dividido por 5 (esto también parece un cálculo intermedio)
total_viajes_5 <- total_viaje / 5
total_viajes_5

#Cálculo de viajes por día (aproximado) considerando semanas y meses
viajes_x_dia_5 <- round(total_viajes_5 / (num_meses * dias_semana * semanas))
viajes_x_dia_5

#Obtener la cantidad de viajes anuales por piloto
viajes_anuales_por_piloto <- coffee3 %>%
  group_by(PILOTO) %>%
  summarise(total_viajes = n()) %>% 
  arrange(desc(total_viajes))
viajes_anuales_por_piloto


#---------*Hipotesis 3*-----------
#Mostrar los clientes únicos en la columna CLIENTE
unique(coffee3$CLIENTE)

#Filtrar los viajes correspondientes al cliente "EL GALLO NEGRO"
viajes_el_gallo_negro <- coffee3 %>%
  filter(CLIENTE == "EL GALLO NEGRO")

#Contar el número de viajes para el cliente "EL GALLO NEGRO"
num_viajes_el_gallo_negro <- nrow(viajes_el_gallo_negro)
num_viajes_el_gallo_negro

#Filtrar observaciones de "DEVOLUCION" y contar la cantidad de devoluciones por cliente
devoluciones_por_cliente <- coffee3 %>%
  filter(OBSERVACION == "DEVOLUCION") %>%
  group_by(CLIENTE) %>%
  summarise(total_devoluciones = n())
devoluciones_por_cliente

#Calcular el porcentaje (redondeado) de devoluciones en comparación al total de observaciones
round((119/245)*100)

#Filtrar datos para el cliente "EL GALLO NEGRO" y calcular el total de "Q" (cantidad) en observaciones
gallo_negro <- coffee3 %>%
  filter(CLIENTE == "EL GALLO NEGRO") %>%
  summarise(total = sum(Q, na.rm = TRUE))
gallo_negro

#Filtrar devoluciones para el cliente "EL GALLO NEGRO" y calcular el total de "Q" en observaciones de devolución
devoluciones_gallo_negro <- coffee3 %>%
  filter(CLIENTE == "EL GALLO NEGRO" & OBSERVACION == "DEVOLUCION") %>%
  summarise(total_devoluciones = sum(Q, na.rm = TRUE))
devoluciones_gallo_negro

#Contar las unidades en observaciones de devolución para el cliente "EL GALLO NEGRO"
unidades_gallo_negro <- coffee3 %>%
  filter(CLIENTE == "EL GALLO NEGRO" & OBSERVACION == "DEVOLUCION") %>%
  group_by(UNIDAD) %>%
  count()
unidades_gallo_negro

#Filtrar los datos para el cliente "EL GALLO NEGRO"
viajes_gallo_negro <- coffee3 %>%
  filter(CLIENTE == "EL GALLO NEGRO")

#Crear un gráfico de barras que muestra la distribución de viajes por mes y observación para el cliente "EL GALLO NEGRO"
ggplot(viajes_gallo_negro, aes(x = MONTH)) +
  geom_bar(aes(fill = OBSERVACION), position = "stack") +
  geom_text(aes(label = ..count..), stat = "count", position = position_stack(vjust = 1.1)) +
  scale_fill_manual(values = c("DESPACHO A CLIENTE" = "darkblue", "DEVOLUCION" = "red")) +
  labs(x = "Mes",
       y = "Cantidad de Viajes",
       fill = "Observación") +
  theme_minimal()

#---------*EXTRA*-----------


#Agrupar la columna Q en rangos y contar la frecuencia en cada rango
Qs <- coffee3 %>%
  group_by(Q = cut(Q, breaks = seq(0, 500, by = 50))) %>%
  summarise(n = n())
Qs

#Crear un gráfico de barras para la distribución de la columna Q en rangos
ggplot(Qs, aes(x = Q, y = n)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(title = "Q en Rangos de 100",
       x = "Valor de Q",
       y = "Frecuencia") +
  geom_text(aes(label = n), vjust = -0.4)


#Agrupar por mes y sumar los valores de Q
monthly_gains <- coffee3 %>%
  group_by(MONTH) %>%
  summarise(total_gains = sum(Q))
monthly_gains

#Ordenar por ganancias totales y seleccionar la columna del mes
monthly_gains_by_ammount <- monthly_gains %>%
  arrange(desc(total_gains))
monthly_gains_by_ammount

#Crear un gráfico de barras para visualizar las ganancias mensuales
ggplot(monthly_gains, aes(x = MONTH, y = total_gains)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  labs(title = "Ganancias Mensuales",
       x = "Mes",
       y = "Ganancias")

#Crear un gráfico de línea para visualizar la tendencia de las ganancias mensuales
ggplot(monthly_gains, aes(x = MONTH, y = total_gains)) +
  geom_line(aes(group = 1), color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Tendencia de Ganancias Mensuales",
       x = "Mes",
       y = "Ganancias")

#Encontrar el valor máximo de las columnas Q y CANTIDAD
max(coffee3$Q)
max(coffee3$CANTIDAD)

#Agrupar por mes y sumar las cantidades
monthly_ammount <- coffee3 %>%
  group_by(MONTH) %>%
  summarise(total_ammounts = sum(CANTIDAD))
monthly_ammount

#Agrupar por mes y sumar las ganancias y las cantidades
monthly_totals <- coffee3 %>%
  group_by(MONTH) %>%
  summarise(total_gains = sum(Q),
            total_ammounts = sum(CANTIDAD))
monthly_totals

#Sumar la columna Q
sum(coffee3$Q)

#Crear un gráfico de barras con líneas superpuestas y etiquetas para las dos variables
ggplot(monthly_totals, aes(x = MONTH)) +
  geom_bar(aes(y = total_ammounts, fill = "Cantidad"),
           stat = "identity", alpha = 0.7, position = position_dodge(width = 0.5)) +
  geom_line(aes(y = total_gains, group = 1), color = "blue", size = 1.5) +
  geom_point(aes(y = total_gains, color = "Ganancias"), size = 3) +
  geom_text(aes(y = total_gains, label = scales::comma(total_gains)),
            vjust = -1, position = position_dodge(width = 0.5), size = 3, color = "blue") +
  geom_text(aes(y = total_ammounts, label = scales::comma(total_ammounts)),
            vjust = -0.4, position = position_dodge(width = 0.5), size = 3, color = "black") +
  scale_color_manual(values = c("Ganancias" = "blue")) +
  scale_fill_manual(values = c("Cantidad" = "green")) +
  labs(title = "Tendencia de Ganancias y Cantidad Mensuales",
       x = "Mes",
       y = "Ganancias y Cantidad") +
  theme(legend.position = "top") +
  guides(color = guide_legend(title = NULL), fill = guide_legend(title = NULL)) +
  scale_y_continuous(labels = scales::comma)

#Encontrar los 5 meses con las cantidades más bajas de crédito otorgado
top_lowest_amounts <- monthly_totals %>% 
  select(MONTH, total_ammounts, total_gains) %>% 
  filter(total_ammounts < 220000)
top_lowest_amounts

#Obtener la unidad utilizada en esos 5 meses
units_used <- coffee3 %>%
  filter(MONTH %in% top_lowest_amounts$MONTH) %>%
  group_by(MONTH, UNIDAD) %>%
  count() %>%
  arrange(MONTH, desc(n)) %>%
  slice(1) %>%
  ungroup()
units_used

#Obtener las unidades más utilizadas por mes para todos los registros
units_used_all <- coffee3 %>%
  group_by(MONTH, UNIDAD) %>%
  count() %>%
  arrange(MONTH, desc(n)) %>%
  slice(1) %>%
  ungroup()
units_used_all

#Crear un gráfico de barras para mostrar la unidad más utilizada por mes
ggplot(units_used_all, aes(x = MONTH, y = n, fill = UNIDAD)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Unidad más utilizada por mes",
       x = "Mes",
       y = "Frecuencia") +
  scale_fill_discrete(name = "Unidad") +
  theme_minimal()

#Encontrar la unidad más utilizada por mes para todos los registros
units_used %>% 
  group_by(MONTH) %>%
  filter(n == max(n)) %>%
  select(MONTH, UNIDAD, n) %>%
  ungroup()
units_used

#Calcular los ingresos totales de cada cliente
client_incomes <- coffee3 %>%
  group_by(CLIENTE) %>%
  summarise(total_income = sum(Q))

#Ordenar los clientes por ingresos
sorted_clients <- client_incomes %>%
  arrange(desc(total_income))

#Calcular el 80% de los ingresos totales
total_income_80_percent <- sum(sorted_clients$total_income) * 0.8

#Seleccionar los clientes más importantes que contribuyen al 80% de los ingresos
important_clients <- sorted_clients %>%
  mutate(cumulative_income = cumsum(total_income)) %>%
  filter(cumulative_income <= total_income_80_percent)
important_clients

#Crear un gráfico de barras para mostrar los ingresos totales de los clientes importantes
ggplot(important_clients, aes(x = reorder(CLIENTE, -total_income), y = total_income)) +
  geom_bar(stat = "identity", fill = "darkgreen") +  #Crear barras de altura proporcional a los ingresos
  geom_text(aes(label = total_income), vjust = -0.4, color = "black") +  #Agregar etiquetas de valores en las barras
  labs(x = "Cliente",  #Etiqueta del eje x
       y = "Ingresos Totales") +  #Etiqueta del eje y
  theme_minimal() +  #Utilizar un tema minimalista
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  #Rotar etiquetas del eje x

#Agrupar por mes y piloto, y contar la frecuencia de viajes
pilot_frequency <- coffee3 %>%
  group_by(MONTH, PILOTO) %>%
  count()
pilot_frequency

#Agrupar por piloto y contar el total de viajes
pilot_total_count <- coffee3 %>%
  group_by(PILOTO) %>%
  tally()
pilot_total_count

#Crear una tabla que relaciona a cada piloto con las unidades que maneja
pilots_units <- coffee3 %>%
  group_by(PILOTO) %>%
  summarise(unidades_manjeadas = list(unique(UNIDAD)))

#Convertir las listas de unidades a vectores de texto
pilots_units$unidades_texto <- sapply(pilots_units$unidades_manjeadas, paste, collapse = ", ")

#Visualizar la tabla resultante con los pilotos y las unidades que manejan
print(pilots_units)

#Contar la frecuencia de cada piloto con cada unidad y seleccionar la unidad más frecuente
unit_frequency_pilot <- coffee3 %>%
  group_by(PILOTO, UNIDAD) %>%
  count() %>%
  arrange(PILOTO, desc(n)) %>%
  group_by(PILOTO) %>%
  slice(1) %>%
  ungroup()
unit_frequency_pilot

#Agrupar por piloto y calcular el total de entregas realizadas
pilot_deliveries <- coffee3 %>%
  group_by(PILOTO) %>%
  summarise(total_delivered = sum(CANTIDAD)) %>%
  arrange(desc(total_delivered))
pilot_deliveries

#Calcular el costo promedio por unidad para cada mes
average_cost_per_unit <- coffee3 %>%
  group_by(MONTH) %>%
  summarise(average_cost = sum(Q) / sum(CANTIDAD))
average_cost_per_unit

#Calcular la productividad de los pilotos en términos de entregas y ganancias
pilots_productivity <- coffee3 %>%
  group_by(PILOTO) %>%
  summarise(total_deliveries = sum(CANTIDAD), total_income = sum(Q)) %>%
  arrange(desc(total_deliveries))
pilots_productivity

#Calcular el promedio de días de crédito por mes
average_days_credit <- coffee3 %>%
  group_by(MONTH) %>%
  summarise(average_days = mean(CREDITO))
average_days_credit

#Crear un dataframe con los datos de los pilotos y transportes
pilotos_transportes <- coffee3 %>%
  group_by(PILOTO, UNIDAD) %>%
  summarise(total_viajes = n())

#Crear un gráfico de barras para mostrar la cantidad de viajes por piloto y unidad
ggplot(pilotos_transportes, aes(x = reorder(PILOTO, -total_viajes), y = total_viajes, fill = UNIDAD)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mejores Pilotos y Transportes más Efectivos",
       x = "Piloto",
       y = "Cantidad de Viajes",
       fill = "Tipo de Unidad") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  #Rotar etiquetas del eje x
