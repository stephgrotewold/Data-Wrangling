library(readxl)
library(readr)
library(tidyverse)
library(tidytext)


# Cargar archivo de excel
excel <- readxl::read_excel("example_1.xlsx")
head(excel)
str(excel)


###QUIERO SABER LAS HOJAS DE MI EXCEL
excel_sheets('example_1.xlsx')


#selecionar por hoja
excel_2<-read_excel('example_1.xlsx', sheet =3)

write_excel_csv2(excel, "exportar_excel.xls", delim =",")


### 
csv = read_csv('example_2.csv')
csv 

txt_1 <-read_delim("example_3.txt", delim =";")
txt_1

txt_2 <-read_delim("example_4.txt", delim ="|")
txt_2

## cargar archivos de texto no tabulados

dorian = read_lines('dorian_gray.txt', skip_empty_rows = TRUE)
str(dorian)
