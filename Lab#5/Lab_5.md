Lab5
================
2023-11-13

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
library(readxl)
library(stringr)
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(nycflights13)
```

## Parte 1: Predecir un eclipse solar

``` r
#----*Parte 1: Predecir un eclipse solar*----
fecha_eclipse_historico <- with_tz(ymd_hms("2017-08-21 18:26:40"), tzone = "America/New_York")

#Duración de un Synodic Month
synodic_month <- ddays(29) + dhours(12) + dminutes(44) + dseconds(3)

#Duración de un Saros (223 Synodic Months)
saros <- synodic_month * 223

#Calcular la fecha del siguiente eclipse solar en la zona horaria de América del Norte
fecha_siguiente_eclipse <- fecha_eclipse_historico + saros
fecha_siguiente_eclipse
```

    ## [1] "2035-09-01 22:09:49 EDT"

## Parte 2: Agrupaciones y Operaciones con Fechas

``` r
#----*Parte 2: Agrupaciones y Operaciones con Fechas*----
data_df <- read_excel("data.xlsx")
str(data_df)
```

    ## tibble [263,725 × 9] (S3: tbl_df/tbl/data.frame)
    ##  $ Fecha Creación: chr [1:263725] "43020" "19-03-17" "13-03-17" "14-04-17" ...
    ##  $ Hora Creación : POSIXct[1:263725], format: "1899-12-31 22:18:36" "1899-12-31 17:35:32" ...
    ##  $ Caller ID     : num [1:263725] 368224 368224 368224 368224 748633 ...
    ##  $ Cod           : chr [1:263725] "Cancelaciones" "Otros/Varios" "Consultas" "Consultas" ...
    ##  $ Email         : num [1:263725] 0 0 0 0 0 0 0 0 0 0 ...
    ##  $ SMS           : num [1:263725] 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Call          : num [1:263725] 0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Fecha Final   : chr [1:263725] "43020" "19-03-17" "13-03-17" "14-04-17" ...
    ##  $ Hora Final    : POSIXct[1:263725], format: "1899-12-31 22:29:36" "1899-12-31 17:52:32" ...

``` r
data_df2 <- data_df %>%
  rename(
    Fecha_Creacion = "Fecha Creación",
    Fecha_Final = "Fecha Final",
    Hora_Creacion = "Hora Creación",
    Hora_Final = "Hora Final"
  )
head(data_df2$Fecha_Creacion,10)
```

    ##  [1] "43020"    "19-03-17" "13-03-17" "14-04-17" "43043"    "29-04-17"
    ##  [7] "42745"    "20-09-17" "13-04-17" "23-03-17"

``` r
data_df2 <- data_df2 %>% 
  mutate(
    Fecha_Creacion_Temp = Fecha_Creacion,
    Fecha_Creacion = dmy(Fecha_Creacion),
    Fecha_Creacion_Temp = as.Date(as.numeric(Fecha_Creacion_Temp), origin = "1899-12-30"),
    Fecha_Final_Temp = Fecha_Final,
    Fecha_Final = dmy(Fecha_Final),
    Fecha_Final_Temp = as.Date(as.numeric(Fecha_Final_Temp), origin = "1899-12-30")
  )
```

    ## Warning: There were 4 warnings in `mutate()`.
    ## The first warning was:
    ## ℹ In argument: `Fecha_Creacion = dmy(Fecha_Creacion)`.
    ## Caused by warning:
    ## !  104237 failed to parse.
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 3 remaining warnings.

``` r
data_df2 <- data_df2 %>% 
  mutate(
    Fecha_Creacion_Final = coalesce(Fecha_Creacion_Temp, Fecha_Creacion),
    Fecha_Final_Finalito = coalesce(Fecha_Final_Temp, Fecha_Final)
  )
 
str(data_df2)
```

    ## tibble [263,725 × 13] (S3: tbl_df/tbl/data.frame)
    ##  $ Fecha_Creacion      : Date[1:263725], format: NA "2017-03-19" ...
    ##  $ Hora_Creacion       : POSIXct[1:263725], format: "1899-12-31 22:18:36" "1899-12-31 17:35:32" ...
    ##  $ Caller ID           : num [1:263725] 368224 368224 368224 368224 748633 ...
    ##  $ Cod                 : chr [1:263725] "Cancelaciones" "Otros/Varios" "Consultas" "Consultas" ...
    ##  $ Email               : num [1:263725] 0 0 0 0 0 0 0 0 0 0 ...
    ##  $ SMS                 : num [1:263725] 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Call                : num [1:263725] 0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Fecha_Final         : Date[1:263725], format: NA "2017-03-19" ...
    ##  $ Hora_Final          : POSIXct[1:263725], format: "1899-12-31 22:29:36" "1899-12-31 17:52:32" ...
    ##  $ Fecha_Creacion_Temp : Date[1:263725], format: "2017-10-12" NA ...
    ##  $ Fecha_Final_Temp    : Date[1:263725], format: "2017-10-12" NA ...
    ##  $ Fecha_Creacion_Final: Date[1:263725], format: "2017-10-12" "2017-03-19" ...
    ##  $ Fecha_Final_Finalito: Date[1:263725], format: "2017-10-12" "2017-03-19" ...

``` r
#1. ¿En qué meses existe una mayor cantidad de llamadas por código?
meses_mas_llamadas <- data_df2 %>%
  mutate(Month = month(Fecha_Creacion_Final)) %>%
  group_by(Month, Cod) %>%
  summarise(Total_Llamadas = n()) %>%
  arrange(desc(Total_Llamadas))
```

    ## `summarise()` has grouped output by 'Month'. You can override using the
    ## `.groups` argument.

``` r
meses_mas_llamadas
```

    ## # A tibble: 84 × 3
    ## # Groups:   Month [12]
    ##    Month Cod       Total_Llamadas
    ##    <dbl> <chr>              <int>
    ##  1    10 Consultas          10790
    ##  2     3 Consultas          10673
    ##  3     7 Consultas          10629
    ##  4     5 Consultas          10618
    ##  5     1 Consultas          10592
    ##  6     8 Consultas          10557
    ##  7    12 Consultas          10465
    ##  8    11 Consultas          10287
    ##  9     9 Consultas          10274
    ## 10     4 Consultas          10153
    ## # ℹ 74 more rows

``` r
#2. ¿Qué día de la semana es el más ocupado?
dia_semana_mas_ocupado <- data_df2 %>%
  filter(!is.na(Fecha_Creacion_Final)) %>%
  mutate(DayOfWeek = weekdays(Fecha_Creacion_Final)) %>%
  group_by(DayOfWeek) %>%
  summarise(Total_Llamadas = n()) %>%
  arrange(desc(Total_Llamadas))
dia_semana_mas_ocupado
```

    ## # A tibble: 7 × 2
    ##   DayOfWeek Total_Llamadas
    ##   <chr>              <int>
    ## 1 Sunday             38254
    ## 2 Thursday           37726
    ## 3 Tuesday            37710
    ## 4 Saturday           37614
    ## 5 Wednesday          37511
    ## 6 Monday             37501
    ## 7 Friday             37409

``` r
#3. ¿Qué mes es el más ocupado?
mes_mas_ocupado <- data_df2 %>%
  filter(!is.na(Fecha_Creacion_Final)) %>%
  mutate(Month = month(Fecha_Creacion_Final)) %>%
  group_by(Month) %>%
  summarise(Total_Llamadas = n()) %>%
  arrange(desc(Total_Llamadas))
mes_mas_ocupado
```

    ## # A tibble: 12 × 2
    ##    Month Total_Llamadas
    ##    <dbl>          <int>
    ##  1     3          22708
    ##  2    10          22601
    ##  3     5          22525
    ##  4     7          22514
    ##  5     1          22425
    ##  6     8          22316
    ##  7    12          22151
    ##  8     9          21891
    ##  9    11          21681
    ## 10     4          21611
    ## 11     6          21370
    ## 12     2          19932

``` r
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
```

![](Lab_5_files/figure-gfm/parte%202-1.png)<!-- -->

``` r
#5. ¿Cuántos minutos dura la llamada promedio?
head(data_df2$Hora_Creacion)
```

    ## [1] "1899-12-31 22:18:36 UTC" "1899-12-31 17:35:32 UTC"
    ## [3] "1899-12-31 22:03:45 UTC" "1899-12-31 17:55:33 UTC"
    ## [5] "1899-12-31 09:08:42 UTC" "1899-12-31 07:19:37 UTC"

``` r
data_df2$Hora_Creacion <- as.POSIXct(data_df2$Hora_Creacion, format = "%H:%M:%S")
data_df2$Hora_Final <- as.POSIXct(data_df2$Hora_Final, format = "%H:%M:%S")

data_df2$Duracion <- as.numeric(difftime(data_df2$Hora_Final, data_df2$Hora_Creacion, units = "mins"))

duracion_promedio <- mean(data_df2$Duracion, na.rm = TRUE)
cat("La duración promedio es de:", duracion_promedio, "minutos\n")
```

    ## La duración promedio es de: 14.88962 minutos

``` r
#6. Realice una tabla de frecuencias con el tiempo de llamada.
tabla_frecuencias <- table(data_df2$Duracion)
tabla_frecuencias
```

    ## 
    ##    0    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15 
    ## 9706 8741 8508 8445 8513 8413 8501 8420 8576 8504 8481 8388 8462 8614 8462 8445 
    ##   16   17   18   19   20   21   22   23   24   25   26   27   28   29   30 
    ## 8464 8576 8451 8331 8578 8288 8414 8630 8465 8346 8417 8431 8299 8452 8404

## Parte 3: Signo Zodiacal

``` r
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
```

    ## Tu signo zodiacal es: Piscis

## Parte 4: Flights

``` r
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
```

    ## # A tibble: 6 × 19
    ##    year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##   <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ## 1  2013     1     1      517            515         2      830            819
    ## 2  2013     1     1      533            529         4      850            830
    ## 3  2013     1     1      542            540         2      923            850
    ## 4  2013     1     1      544            545        -1     1004           1022
    ## 5  2013     1     1      554            600        -6      812            837
    ## 6  2013     1     1      554            558        -4      740            728
    ## # ℹ 11 more variables: arr_delay <dbl>, carrier <chr>, flight <int>,
    ## #   tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>,
    ## #   hour <dbl>, minute <dbl>, time_hour <dttm>

``` r
head(flights2)
```

    ## # A tibble: 6 × 23
    ##    year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##   <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ## 1  2013     1     1      517            515         2      830            819
    ## 2  2013     1     1      533            529         4      850            830
    ## 3  2013     1     1      542            540         2      923            850
    ## 4  2013     1     1      544            545        -1     1004           1022
    ## 5  2013     1     1      554            600        -6      812            837
    ## 6  2013     1     1      554            558        -4      740            728
    ## # ℹ 15 more variables: arr_delay <dbl>, carrier <chr>, flight <int>,
    ## #   tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>,
    ## #   hour <dbl>, minute <dbl>, time_hour <dttm>, dep_datetime <dttm>,
    ## #   arr_datetime <dttm>, sched_dep_datetime <dttm>, sched_arr_datetime <dttm>

``` r
flights2 <- flights2 %>%
  mutate(
    total_delay = dep_delay + arr_delay
  )
flights2
```

    ## # A tibble: 336,776 × 24
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      517            515         2      830            819
    ##  2  2013     1     1      533            529         4      850            830
    ##  3  2013     1     1      542            540         2      923            850
    ##  4  2013     1     1      544            545        -1     1004           1022
    ##  5  2013     1     1      554            600        -6      812            837
    ##  6  2013     1     1      554            558        -4      740            728
    ##  7  2013     1     1      555            600        -5      913            854
    ##  8  2013     1     1      557            600        -3      709            723
    ##  9  2013     1     1      557            600        -3      838            846
    ## 10  2013     1     1      558            600        -2      753            745
    ## # ℹ 336,766 more rows
    ## # ℹ 16 more variables: arr_delay <dbl>, carrier <chr>, flight <int>,
    ## #   tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>,
    ## #   hour <dbl>, minute <dbl>, time_hour <dttm>, dep_datetime <dttm>,
    ## #   arr_datetime <dttm>, sched_dep_datetime <dttm>, sched_arr_datetime <dttm>,
    ## #   total_delay <dbl>
