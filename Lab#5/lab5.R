library(dplyr)
library(ggplot2)
library(readxl)
library(stringr)
library(lubridate)
library(nycflights13)


#----*Parte 1: Predecir un eclipse solar*----
fecha_eclipse_historico <- with_tz(ymd_hms("2017-08-21 18:26:40"), tzone = "America/New_York")

#Duración de un Synodic Month
synodic_month <- ddays(29) + dhours(12) + dminutes(44) + dseconds(3)

#Duración de un Saros (223 Synodic Months)
saros <- synodic_month * 223

#Calcular la fecha del siguiente eclipse solar en la zona horaria de América del Norte
fecha_siguiente_eclipse <- fecha_eclipse_historico + saros
fecha_siguiente_eclipse


#----*Parte 2: Agrupaciones y Operaciones con Fechas*----
data_df <- read_excel("data.xlsx")
str(data_df)
data_df2 <- data_df %>%
  rename(
    Fecha_Creacion = "Fecha Creación",
    Fecha_Final = "Fecha Final",
    Hora_Creacion = "Hora Creación",
    Hora_Final = "Hora Final"
  )
head(data_df2$Fecha_Creacion,10)

data_df2 <- data_df2 %>% 
  mutate(
    Fecha_Creacion_Temp = Fecha_Creacion,
    Fecha_Creacion = dmy(Fecha_Creacion),
    Fecha_Creacion_Temp = as.Date(as.numeric(Fecha_Creacion_Temp), origin = "1899-12-30"),
    Fecha_Final_Temp = Fecha_Final,
    Fecha_Final = dmy(Fecha_Final),
    Fecha_Final_Temp = as.Date(as.numeric(Fecha_Final_Temp), origin = "1899-12-30")
  )

data_df2 <- data_df2 %>% 
  mutate(
    Fecha_Creacion_Final = coalesce(Fecha_Creacion_Temp, Fecha_Creacion),
    Fecha_Final_Finalito = coalesce(Fecha_Final_Temp, Fecha_Final)
  )
 
str(data_df2)


#1. ¿En qué meses existe una mayor cantidad de llamadas por código?
meses_mas_llamadas <- data_df2 %>%
  mutate(Month = month(Fecha_Creacion_Final)) %>%
  group_by(Month, Cod) %>%
  summarise(Total_Llamadas = n()) %>%
  arrange(desc(Total_Llamadas))
meses_mas_llamadas


#2. ¿Qué día de la semana es el más ocupado?
dia_semana_mas_ocupado <- data_df2 %>%
  filter(!is.na(Fecha_Creacion_Final)) %>%
  mutate(DayOfWeek = weekdays(Fecha_Creacion_Final)) %>%
  group_by(DayOfWeek) %>%
  summarise(Total_Llamadas = n()) %>%
  arrange(desc(Total_Llamadas))
dia_semana_mas_ocupado


#3. ¿Qué mes es el más ocupado?
mes_mas_ocupado <- data_df2 %>%
  filter(!is.na(Fecha_Creacion_Final)) %>%
  mutate(Month = month(Fecha_Creacion_Final)) %>%
  group_by(Month) %>%
  summarise(Total_Llamadas = n()) %>%
  arrange(desc(Total_Llamadas))
mes_mas_ocupado

#4. ¿Existe una concentración o estacionalidad en la cantidad de llamadas?
llamadas_por_fecha <- data_df2 %>%
  filter(!is.na(Fecha_Creacion_Final)) %>%
  group_by(Fecha_Creacion_Final) %>%
  summarise(Total_Llamadas = n())

ggplot(llamadas_por_fecha, aes(x = Total_Llamadas)) +
  geom_histogram(binwidth = 1, fill = "pink", color = "black") +
  labs(
    title = "Histograma de la Cantidad de Llamadas por Fecha",
    x = "Cantidad de Llamadas",
    y = "Frecuencia"
  )

#5. ¿Cuántos minutos dura la llamada promedio?
head(data_df2$Hora_Creacion)

data_df2$Hora_Creacion <- as.POSIXct(data_df2$Hora_Creacion, format = "%H:%M:%S")
data_df2$Hora_Final <- as.POSIXct(data_df2$Hora_Final, format = "%H:%M:%S")

data_df2$Duracion <- as.numeric(difftime(data_df2$Hora_Final, data_df2$Hora_Creacion, units = "mins"))

duracion_promedio <- mean(data_df2$Duracion, na.rm = TRUE)
cat("La duración promedio es de:", duracion_promedio, "minutos\n")

#6. Realice una tabla de frecuencias con el tiempo de llamada.
tabla_frecuencias <- table(data_df2$Duracion)
tabla_frecuencias


#----* Parte 3: Signo Zodiacal*----
zodiac <- function(año, mes, dia) {
  #Verificar si el año es bisiesto
  es_bisiesto <- lubridate::leap_year(año)
  
  #Verificar si la fecha es válida
  if ((mes >= 1 && mes <= 12) && (dia >= 1 && dia <= 31)) {
    #Verificar si la fecha es válida para febrero
    if (mes == 2) {
      if ((dia >= 1 && dia <= 29 && es_bisiesto) || (dia >= 1 && dia <= 28 && !es_bisiesto)) {
        return("Tu signo zodiacal es: Piscis")
      } else {
        return("La fecha no es válida.")
      }
    }
    
  
    if ((mes == 3 && dia >= 21) || (mes == 4 && dia <= 19)) {
      return("Tu signo zodiacal es: Aries")
    } else if ((mes == 4 && dia >= 20) || (mes == 5 && dia <= 20)) {
      return("Tu signo zodiacal es: Tauro")
    } else if ((mes == 5 && dia >= 21) || (mes == 6 && dia <= 20)) {
      return("Tu signo zodiacal es: Géminis")
    } else if ((mes == 6 && dia >= 21) || (mes == 7 && dia <= 22)) {
      return("Tu signo zodiacal es: Cáncer")
    } else if ((mes == 7 && dia >= 23) || (mes == 8 && dia <= 22)) {
      return("Tu signo zodiacal es: Leo")
    } else if ((mes == 8 && dia >= 23) || (mes == 9 && dia <= 22)) {
      return("Tu signo zodiacal es: Virgo")
    } else if ((mes == 9 && dia >= 23) || (mes == 10 && dia <= 22)) {
      return("Tu signo zodiacal es: Libra")
    } else if ((mes == 10 && dia >= 23) || (mes == 11 && dia <= 21)) {
      return("Tu signo zodiacal es: Escorpio")
    } else if ((mes == 11 && dia >= 22) || (mes == 12 && dia <= 21)) {
      return("Tu signo zodiacal es: Sagitario")
    } else if ((mes == 12 && dia >= 22) || (mes == 1 && dia <= 19)) {
      return("Tu signo zodiacal es: Capricornio")
    } else if ((mes == 1 && dia >= 20) || (mes == 2 && dia <= 18)) {
      return("Tu signo zodiacal es: Acuario")
    } else {
      return("La fecha no es válida.")
    }
  } else {
    return("La fecha no es válida.")
  }
}

año_nacimiento <- 2000
mes_nacimiento <- 2
dia_nacimiento <- 25
resultado <- zodiac(año_nacimiento, mes_nacimiento, dia_nacimiento)
cat(resultado, "\n")


#----* Parte 4: Flights*----
#View(flights)
flights2 <- flights


#View(flights)
flights2 <- flights %>%
  mutate(
    #Columna para dep_time en formato fecha y hora
    dep_datetime = make_datetime(year, month, day, dep_time %/% 100, dep_time %% 100),
    
    #Columna para arr_time en formato fecha y hora
    arr_datetime = make_datetime(year, month, day, arr_time %/% 100, arr_time %% 100),
    
    #Columna para sched_dep_time en formato fecha y hora
    sched_dep_datetime = make_datetime(year, month, day, sched_dep_time %/% 100, sched_dep_time %% 100),
    
    #Columna para sched_arr_time en formato fecha y hora
    sched_arr_datetime = make_datetime(year, month, day, sched_arr_time %/% 100, sched_arr_time %% 100)
  )
head(flights)
head(flights2)


flights2 <- flights2 %>%
  mutate(
    total_delay = dep_delay + arr_delay
  )
View(flights2)
