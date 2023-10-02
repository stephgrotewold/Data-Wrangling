library(dplyr)
library(readxl)
library(stringr)
library(lubridate)
library(nycflights13)


#----*Parte 1: Predecir un eclipse solar*----
# Fecha del eclipse histórico en la zona horaria de América del Norte (por ejemplo, Nueva York)
fecha_eclipse_historico <- with_tz(ymd_hms("2017-08-21 18:26:40"), tzone = "America/New_York")

# Duración de un Synodic Month
synodic_month <- ddays(29) + dhours(12) + dminutes(44) + dseconds(3)

# Duración de un Saros (223 Synodic Months)
saros <- synodic_month * 223

# Calcular la fecha del siguiente eclipse solar en la zona horaria de América del Norte
fecha_siguiente_eclipse <- fecha_eclipse_historico + saros

# Imprimir la fecha del siguiente eclipse solar en la zona horaria de América del Norte
print(fecha_siguiente_eclipse)




#----*Parte 2: Agrupaciones y Operaciones con Fechas*----
data_df <- read_excel("data.xlsx")
data_df2 <- data_df %>%
  rename(
    Fecha_Creacion = "Fecha Creación",
    Fecha_Final = "Fecha Final",
    Hora_Creacion = "Hora Creación",
    Hora_Final = "Hora Final"
  )
head(data_df2$Fecha_Creacion)

#----* Parte 3: Signo Zodiacal*----
zodiac <- function(año, mes, dia) {
  # Verificar si el año es bisiesto
  es_bisiesto <- lubridate::leap_year(año)
  
  # Verificar si la fecha es válida
  if ((mes >= 1 && mes <= 12) && (dia >= 1 && dia <= 31)) {
    # Verificar si la fecha es válida para febrero
    if (mes == 2) {
      if ((dia >= 1 && dia <= 29 && es_bisiesto) || (dia >= 1 && dia <= 28 && !es_bisiesto)) {
        return("Tu signo zodiacal es: Piscis")
      } else {
        return("La fecha no es válida.")
      }
    }
    
    # Resto de los meses
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

# Ejemplo de uso:
año_nacimiento <- 2000
mes_nacimiento <- 2
dia_nacimiento <- 25
resultado <- zodiac(año_nacimiento, mes_nacimiento, dia_nacimiento)
cat(resultado, "\n")


#----* Parte 4: Flights*----
View(flights)
flights2 <- flights


View(flights)
flights2 <- flights %>%
  mutate(
    # Columna para dep_time en formato fecha y hora
    dep_datetime = make_datetime(year, month, day, dep_time %/% 100, dep_time %% 100),
    
    # Columna para arr_time en formato fecha y hora
    arr_datetime = make_datetime(year, month, day, arr_time %/% 100, arr_time %% 100),
    
    # Columna para sched_dep_time en formato fecha y hora
    sched_dep_datetime = make_datetime(year, month, day, sched_dep_time %/% 100, sched_dep_time %% 100),
    
    # Columna para sched_arr_time en formato fecha y hora
    sched_arr_datetime = make_datetime(year, month, day, sched_arr_time %/% 100, sched_arr_time %% 100)
  )
head(flights)
head(flights2)


flights2 <- flights2 %>%
  mutate(
    total_delay = dep_delay + arr_delay
  )
View(flights2)
