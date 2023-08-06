Lab1_SG
================
2023-08-02

``` r
library(readxl)
datos <- read_xlsx("datos_combinados.xlsx") 
head(datos)
```

    ## # A tibble: 6 × 9
    ##   COD_VIAJE CLIENTE         UBICACION CANTIDAD PILOTO     Q CREDITO UNIDAD Fecha
    ##       <dbl> <chr>               <dbl>    <dbl> <chr>  <dbl>   <dbl> <chr>  <chr>
    ## 1  10000001 EL PINCHE OBEL…     76002     1200 Ferna… 300        30 Camio… 01-2…
    ## 2  10000002 TAQUERIA EL CH…     76002     1433 Hecto… 358.       90 Camio… 01-2…
    ## 3  10000003 TIENDA LA BEND…     76002     1857 Pedro… 464.       60 Camio… 01-2…
    ## 4  10000004 TAQUERIA EL CH…     76002      339 Angel…  84.8      30 Panel  01-2…
    ## 5  10000005 CHICHARRONERIA…     76001     1644 Juan … 411        30 Camio… 01-2…
    ## 6  10000006 UBIQUO LABS ||…     76001     1827 Luis … 457.       30 Camio… 01-2…

``` r
paste("Datos combinados tiene ", nrow(datos)," filas y ", ncol(datos), " columnas", sep = "")
```

    ## [1] "Datos combinados tiene 2180 filas y 9 columnas"

``` r
vectores <- list(
  v1 = c(7, 10, 3, 3, 4, 4, 5),
  v2 = c(2, 2,81, 7, 81, 81, 5, 5,81),
  v3 = c(55, 2, 2, 3, 3, 55, 55)
)

calc_moda <- function(vector) {
  frecuencias <- table(vector)
  moda <- as.numeric(names(frecuencias[frecuencias == max(frecuencias)]))
  return(moda)
}

moda_vector <- lapply(vectores, calc_moda)
moda_vector
```

    ## $v1
    ## [1] 3 4
    ## 
    ## $v2
    ## [1] 81
    ## 
    ## $v3
    ## [1] 55

``` r
library(readr)
SAT_car <- read_delim("~/Desktop/UFM/INE_PARQUE_VEHICULAR_080219.txt", delim ="|")
```

    ## New names:
    ## • `` -> `...11`

    ## Warning: One or more parsing issues, call `problems()` on your data frame for details,
    ## e.g.:
    ##   dat <- vroom(...)
    ##   problems(dat)

    ## Rows: 2435294 Columns: 11
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: "|"
    ## chr (8): MES, NOMBRE_DEPARTAMENTO, NOMBRE_MUNICIPIO, MODELO_VEHICULO, LINEA_...
    ## dbl (2): ANIO_ALZA, CANTIDAD
    ## lgl (1): ...11
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
head(SAT_car)
```

    ## # A tibble: 6 × 11
    ##   ANIO_ALZA MES   NOMBRE_DEPARTAMENTO NOMBRE_MUNICIPIO MODELO_VEHICULO
    ##       <dbl> <chr> <chr>               <chr>            <chr>          
    ## 1      2007 05    HUEHUETENANGO       "HUEHUETENANGO"  2007           
    ## 2      2007 05    EL PROGRESO         "EL JICARO"      2007           
    ## 3      2007 05    SAN MARCOS          "OCOS"           2007           
    ## 4      2007 05    ESCUINTLA           "SAN JOS\xc9"    2006           
    ## 5      2007 05    JUTIAPA             "MOYUTA"         2007           
    ## 6      2007 05    GUATEMALA           "FRAIJANES"      1997           
    ## # ℹ 6 more variables: LINEA_VEHICULO <chr>, TIPO_VEHICULO <chr>,
    ## #   USO_VEHICULO <chr>, MARCA_VEHICULO <chr>, CANTIDAD <dbl>, ...11 <lgl>
