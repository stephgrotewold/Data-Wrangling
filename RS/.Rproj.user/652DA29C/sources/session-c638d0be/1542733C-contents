setwd("~/Desktop/UFM/Sexto Semestre/Data Wrangling/Data-Wrangling/RS/Labs/Lab1")
library(readxl)
library(writexl)
library(lubridate)
library(dplyr)
library(readr)

archivos <- c("01-2018.xlsx","02-2018.xlsx","03-2018.xlsx",
                   "04-2018.xlsx","05-2018.xlsx","06-2018.xlsx",
                   "07-2018.xlsx","08-2018.xlsx","09-2018.xlsx",
                   "10-2018.xlsx","11-2018.xlsx")

read_add_date <- function(archivo) {
  datos_applied <- read_xlsx(archivo) %>%
    select(COD_VIAJE, CLIENTE, UBICACION, CANTIDAD, PILOTO, Q, CREDITO, UNIDAD)
  m <- substr(archivo, 1, 2)
  yr <- substr(archivo, 4, 7)
  datos_applied$Fecha <- paste0(m, "-", yr)
  return(datos_applied)
}

datos_applied <- lapply(archivos, read_add_date)
datos_combinados <- bind_rows(datos_applied)

write_xlsx(datos_combinados, path = "datos_combinados.xlsx")
