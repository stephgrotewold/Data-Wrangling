library(caret)

#Cargar los datos
titanic_MD <- read.csv("titanic_MD.csv")
titanic_og <- read.csv("titanic.csv")
str(titanic_MD)
summary(titanic_MD)

titanic_MD2 <- titanic_MD
titanic_MD3 <- titanic_MD
titanic_MD4 <- titanic_MD
titanic_final <- titanic_MD

#----*Parte 1*----
#1
#Resumen de valores faltantes en cada columna
missing_data_summary <- sapply(titanic_MD, function(x) sum(is.na(x)))
missing_data_summary




#3
complete_rows <- complete.cases(titanic_MD)
num_complete_rows <- sum(complete_rows)
num_complete_rows

#Porcentaje de filas completas
percentage_complete_rows <- (num_complete_rows / nrow(titanic_MD)) * 100
percentage_complete_rows

sex_table <- table(titanic_MD2$Sex)
sex_table
most_common_sex <- names(sex_table)[which.max(sex_table)]

count_question_marks <- sum(titanic_MD2$Sex == "?")
count_question_marks

count_male <- round(0.5152 * count_question_marks)
count_male
count_female <- count_question_marks - count_male
count_female

titanic_MD2$Sex <- ifelse(titanic_MD2$Sex == "?", ifelse(seq_along(titanic_MD2$Sex) <= count_male, "male", "female"), titanic_MD2$Sex)




#4
#REGRESION
#Crear un modelo de regresión lineal para predecir Age
lm_age <- lm(Age ~ Pclass + SibSp, data = titanic_MD2)

#Predecir los valores de Age faltantes
predicted_age <- predict(lm_age, newdata = titanic_MD2)
titanic_MD2$Age[is.na(titanic_MD2$Age)] <- predicted_age[is.na(titanic_MD2$Age)]


#Crear un modelo de regresión lineal para predecir SibSp
lm_sibsp <- lm(SibSp ~ Pclass + Age, data = titanic_MD2)

#Predecir los valores de SibSp faltantes
predicted_sibsp <- predict(lm_sibsp, newdata = titanic_MD2)
titanic_MD2$SibSp[is.na(titanic_MD2$SibSp)] <- predicted_sibsp[is.na(titanic_MD2$SibSp)]


#Crear un modelo de regresión lineal para predecir Parch
lm_parch <- lm(Parch ~ Pclass + Age, data = titanic_MD2)

#Predecir los valores de Parch faltantes
predicted_parch <- predict(lm_parch, newdata = titanic_MD2)
titanic_MD2$Parch[is.na(titanic_MD2$Parch)] <- predicted_parch[is.na(titanic_MD2$Parch)]


#Crear un modelo de regresión lineal para predecir Fare
lm_fare <- lm(Fare ~ Pclass + Age + SibSp, data = titanic_MD2)

#Predecir los valores de Fare faltantes
predicted_fare <- predict(lm_fare, newdata = titanic_MD2)
titanic_MD2$Fare[is.na(titanic_MD2$Fare)] <- predicted_fare[is.na(titanic_MD2$Fare)]



missing_data_summary2 <- sapply(titanic_MD2, function(x) sum(is.na(x)))
missing_data_summary2

complete_rows2 <- complete.cases(titanic_MD2)
num_complete_rows2 <- sum(complete_rows2)
num_complete_rows2

#Porcentaje de filas completas
percentage_complete_rows2 <- (num_complete_rows2 / nrow(titanic_MD2)) * 100
percentage_complete_rows2



#IMPUTACION GENERAL

mode_sibsp <- names(sort(table(titanic_MD3$SibSp), decreasing = TRUE)[1])
titanic_MD3$SibSp[is.na(titanic_MD3$SibSp)] <- mode_sibsp

mode_parch <- names(sort(table(titanic_MD3$Parch), decreasing = TRUE)[1])
titanic_MD3$Parch[is.na(titanic_MD3$Parch)] <- mode_parch

#Calcular la moda de Parch
mode_parch <- names(sort(table(titanic_MD3$Parch), decreasing = TRUE)[1])

#Imputar valores faltantes en Parch
titanic_MD3$Parch[is.na(titanic_MD3$Parch)] <- mode_parch

#Calcular la mediana de Age
median_age <- median(titanic_MD3$Age, na.rm = TRUE)

#Imputar los valores faltantes de Age con la mediana
titanic_MD3$Age[is.na(titanic_MD3$Age)] <- median_age


#Calcular la moda de Fare
mode_fare <- as.numeric(names(sort(table(titanic_MD3$Fare), decreasing = TRUE)[1]))

#Imputar los valores faltantes de Fare con la moda
titanic_MD3$Fare[is.na(titanic_MD3$Fare)] <- mode_fare


missing_data_summary3 <- sapply(titanic_MD3, function(x) sum(is.na(x)))
missing_data_summary3

complete_rows3<- complete.cases(titanic_MD3)
num_complete_rows3 <- sum(complete_rows3)
num_complete_rows3

#Porcentaje de filas completas
percentage_complete_rows3 <- (num_complete_rows3 / nrow(titanic_MD3)) * 100
percentage_complete_rows3




#OUTLIERS

#Definir los percentiles
percentile_lower <- 2.5  #Puedes ajustar el percentil inferior según tus preferencias
percentile_upper <- 97.5  #Puedes ajustar el percentil superior según tus preferencias

#Calcular los percentiles para Fare
lower_threshold_fare <- quantile(titanic_MD4$Fare, percentile_lower / 100, na.rm = TRUE)
upper_threshold_fare <- quantile(titanic_MD4$Fare, percentile_upper / 100, na.rm = TRUE)

#Identificar y tratar outliers en Fare
titanic_MD4$Fare[titanic_MD4$Fare < lower_threshold_fare] <- lower_threshold_fare
titanic_MD4$Fare[titanic_MD4$Fare > upper_threshold_fare] <- upper_threshold_fare

#Calcular los percentiles para SibSp
lower_threshold_SibSp <- quantile(titanic_MD4$SibSp, percentile_lower / 100, na.rm = TRUE)
upper_threshold_SibSp <- quantile(titanic_MD4$SibSp, percentile_upper / 100, na.rm = TRUE)

#Identificar y tratar outliers en SibSp
titanic_MD4$SibSp[titanic_MD4$SibSp < lower_threshold_SibSp] <- lower_threshold_SibSp
titanic_MD4$SibSp[titanic_MD4$SibSp > upper_threshold_SibSp] <- upper_threshold_SibSp

#Calcular los percentiles para Parch
lower_threshold_Parch <- quantile(titanic_MD4$Parch, percentile_lower / 100, na.rm = TRUE)
upper_threshold_Parch <- quantile(titanic_MD4$Parch, percentile_upper / 100, na.rm = TRUE)

#Identificar y tratar outliers en Parch
titanic_MD4$Parch[titanic_MD4$Parch < lower_threshold_Parch] <- lower_threshold_Parch
titanic_MD4$Parch[titanic_MD4$Parch > upper_threshold_Parch] <- upper_threshold_Parch

#Calcular los percentiles para Age
lower_threshold_Age <- quantile(titanic_MD4$Age, percentile_lower / 100, na.rm = TRUE)
upper_threshold_Age <- quantile(titanic_MD4$Age, percentile_upper / 100, na.rm = TRUE)

#Identificar y tratar outliers en Age
titanic_MD4$Age[titanic_MD4$Age < lower_threshold_Age] <- lower_threshold_Age
titanic_MD4$Age[titanic_MD4$Age > upper_threshold_Age] <- upper_threshold_Age


missing_data_summary4 <- sapply(titanic_MD4, function(x) sum(is.na(x)))
missing_data_summary4

complete_rows4 <- complete.cases(titanic_MD4)
num_complete_rows4 <- sum(complete_rows4)
num_complete_rows4


#Porcentaje de filas completas
percentage_complete_rows4 <- (num_complete_rows4 / nrow(titanic_MD4)) * 100
percentage_complete_rows4




#5

#Calcular RMSE para Age y Fare
rmse_age <- sqrt(mean((titanic_og$Age - titanic_MD2$Age[!is.na(titanic_og$Age)])^2, na.rm = TRUE))
rmse_age
rmse_fare <- sqrt(mean((titanic_og$Fare - titanic_MD2$Fare[!is.na(titanic_og$Fare)])^2, na.rm = TRUE))
rmse_fare
rmse_sibsp <- sqrt(mean((titanic_og$SibSp - titanic_MD2$SibSp[!is.na(titanic_og$SibSp)])^2, na.rm = TRUE))
rmse_sibsp
rmse_parch <- sqrt(mean((titanic_og$Parch - titanic_MD2$Parch[!is.na(titanic_og$Parch)])^2, na.rm = TRUE))
rmse_parch


#Calcular tasa de coincidencia para SibSp y Parch
coincidence_fare <- sum(titanic_og$Fare == titanic_MD3$Fare) / nrow(titanic_og)
coincidence_fare
coincidence_age <- sum(titanic_og$Age == titanic_MD3$Age) / nrow(titanic_og)
coincidence_age
coincidence_sibsp <- sum(titanic_og$SibSp == titanic_MD3$SibSp) / nrow(titanic_og)
coincidence_sibsp
coincidence_parch <- sum(titanic_og$Parch == titanic_MD3$Parch) / nrow(titanic_og)
coincidence_parch


#6
#FALTAN CONCLUSIONES
#En este caso es mejor imputacion general para fare y age. Para Sibsp y parch estan similares


#----* PARTE 2*----
#PRE-PROCESAMIENTO

sex_table <- table(titanic_final$Sex)
sex_table
most_common_sex <- names(sex_table)[which.max(sex_table)]

count_question_marks <- sum(titanic_final$Sex == "?")
count_question_marks

count_male <- round(0.5152 * count_question_marks)
count_male
count_female <- count_question_marks - count_male
count_female

titanic_final$Sex <- ifelse(titanic_final$Sex == "?", ifelse(seq_along(titanic_final$Sex)
                                                             <= count_male, "male", "female"), titanic_final$Sex)


median_age <- median(titanic_final$Age, na.rm = TRUE)
titanic_final$Age[is.na(titanic_final$Age)] <- median_age


mode_fare <- as.numeric(names(sort(table(titanic_final$Fare), decreasing = TRUE)[1]))
titanic_final$Fare[is.na(titanic_final$Fare)] <- mode_fare


mode_sibsp <- names(sort(table(titanic_final$SibSp), decreasing = TRUE)[1])
titanic_final$SibSp[is.na(titanic_final$SibSp)] <- mode_sibsp

mode_parch <- names(sort(table(titanic_final$Parch), decreasing = TRUE)[1])
titanic_final$Parch[is.na(titanic_final$Parch)] <- mode_parch

missing_data_summary_final<- sapply(titanic_final, function(x) sum(is.na(x)))
missing_data_summary_final

complete_rows_final <- complete.cases(titanic_final)
num_complete_rows_final <- sum(complete_rows_final)
num_complete_rows_final

percentage_complete_rows_final <- (num_complete_rows_final / nrow(titanic_final)) * 100
percentage_complete_rows_final

#Normalizar
data <- titanic_final[, c("Age", "Fare", "SibSp", "Parch")]
data <- as.data.frame(lapply(data, as.numeric))

#a. Standarization (Z-score)
preproc_stand <- preProcess(data, method = c("center", "scale"))
data_stand <- predict(preproc_stand, newdata = data)

#b. Min-Max Scaling
preproc_minmax <- preProcess(data, method = c("range"))
data_minmax <- predict(preproc_minmax, newdata = data)

#Calcular los valores máximos absolutos para cada columna
max_abs_values <- apply(data, 2, function(x) max(abs(x), na.rm = TRUE))

#Asegurarse de que los valores máximos absolutos no sean cero
max_abs_values[max_abs_values == 0] <- 1

#Realizar la normalización MaxAbsScaler
data_maxabs <- data / max_abs_values

#Ver los datos normalizados
print("MaxAbsScaler")
head(data_maxabs)

#Ver los datos normalizados
print("MaxAbsScaler")
head(data_maxabs)


#Ver los datos normalizados
print("Standarization (Z-score)")
head(data_stand)


print("Min-Max Scaling")
head(data_minmax)



#2
titanic_data_subset <- titanic_og[, c("Age", "Fare", "SibSp", "Parch")]
#Normalización MaxAbsScaler
max_abs_values_complete <- apply(titanic_data_subset, 2, function(x) max(abs(x), na.rm = TRUE))
max_abs_values_complete[max_abs_values_complete == 0] <- 1
titanic_data_maxabs <- titanic_data_subset / max_abs_values_complete

#Normalización Z-score (Standarization)
preproc_stand_complete <- preProcess(titanic_data_subset, method = c("center", "scale"))
titanic_data_stand <- predict(preproc_stand_complete, newdata = titanic_data_subset)

#Normalización Min-Max Scaling
preproc_minmax_complete <- preProcess(titanic_data_subset, method = c("range"))
titanic_data_minmax <- predict(preproc_minmax_complete, newdata = titanic_data_subset)

#Estadísticos de los datos normalizados
stats_data_maxabs <- summary(data_maxabs)
stats_data_stand <- summary(data_stand)
stats_data_minmax <- summary(data_minmax)

#Estadísticos de la data completa normalizada
stats_titanic_maxabs <- summary(titanic_data_maxabs)
stats_titanic_stand <- summary(titanic_data_stand)
stats_titanic_minmax <- summary(titanic_data_minmax)

#Calcular las medias (promedios) para Age, Fare, SibSp y Parch en cada conjunto de datos normalizado
mean_data_maxabs_age <- mean(data_maxabs$Age, na.rm = TRUE)
mean_data_stand_age <- mean(data_stand$Age, na.rm = TRUE)
mean_data_minmax_age <- mean(data_minmax$Age, na.rm = TRUE)

mean_titanic_maxabs_age <- mean(titanic_data_maxabs$Age, na.rm = TRUE)
mean_titanic_stand_age <- mean(titanic_data_stand$Age, na.rm = TRUE)
mean_titanic_minmax_age <- mean(titanic_data_minmax$Age, na.rm = TRUE)

mean_data_maxabs_fare <- mean(data_maxabs$Fare, na.rm = TRUE)
mean_data_stand_fare <- mean(data_stand$Fare, na.rm = TRUE)
mean_data_minmax_fare <- mean(data_minmax$Fare, na.rm = TRUE)

mean_titanic_maxabs_fare <- mean(titanic_data_maxabs$Fare, na.rm = TRUE)
mean_titanic_stand_fare <- mean(titanic_data_stand$Fare, na.rm = TRUE)
mean_titanic_minmax_fare <- mean(titanic_data_minmax$Fare, na.rm = TRUE)

mean_data_maxabs_sibsp <- mean(data_maxabs$SibSp, na.rm = TRUE)
mean_data_stand_sibsp <- mean(data_stand$SibSp, na.rm = TRUE)
mean_data_minmax_sibsp <- mean(data_minmax$SibSp, na.rm = TRUE)

mean_titanic_maxabs_sibsp <- mean(titanic_data_maxabs$SibSp, na.rm = TRUE)
mean_titanic_stand_sibsp <- mean(titanic_data_stand$SibSp, na.rm = TRUE)
mean_titanic_minmax_sibsp <- mean(titanic_data_minmax$SibSp, na.rm = TRUE)

mean_data_maxabs_parch <- mean(data_maxabs$Parch, na.rm = TRUE)
mean_data_stand_parch <- mean(data_stand$Parch, na.rm = TRUE)
mean_data_minmax_parch <- mean(data_minmax$Parch, na.rm = TRUE)

mean_titanic_maxabs_parch <- mean(titanic_data_maxabs$Parch, na.rm = TRUE)
mean_titanic_stand_parch <- mean(titanic_data_stand$Parch, na.rm = TRUE)
mean_titanic_minmax_parch <- mean(titanic_data_minmax$Parch, na.rm = TRUE)

#Comparación de medias (promedios) para Age, Fare, SibSp y Parch en MaxAbsScaler
mean_difference_maxabs_age <- abs(mean_data_maxabs_age - mean_titanic_maxabs_age)
mean_difference_maxabs_age
mean_difference_maxabs_fare <- abs(mean_data_maxabs_fare - mean_titanic_maxabs_fare)
mean_difference_maxabs_fare
mean_difference_maxabs_sibsp <- abs(mean_data_maxabs_sibsp - mean_titanic_maxabs_sibsp)
mean_difference_maxabs_sibsp
mean_difference_maxabs_parch <- abs(mean_data_maxabs_parch - mean_titanic_maxabs_parch)
mean_difference_maxabs_parch

#Comparación de medias (promedios) para Age, Fare, SibSp y Parch en Standarization (Z-score)
mean_difference_stand_age <- abs(mean_data_stand_age - mean_titanic_stand_age)
mean_difference_stand_age
mean_difference_stand_fare <- abs(mean_data_stand_fare - mean_titanic_stand_fare)
mean_difference_stand_fare
mean_difference_stand_sibsp <- abs(mean_data_stand_sibsp - mean_titanic_stand_sibsp)
mean_difference_stand_sibsp
mean_difference_stand_parch <- abs(mean_data_stand_parch - mean_titanic_stand_parch)
mean_difference_stand_parch

#Comparación de medias (promedios) para Age, Fare, SibSp y Parch en Min-Max Scaling
mean_difference_minmax_age <- abs(mean_data_minmax_age - mean_titanic_minmax_age)
mean_difference_minmax_age
mean_difference_minmax_fare <- abs(mean_data_minmax_fare - mean_titanic_minmax_fare)
mean_difference_minmax_fare
mean_difference_minmax_sibsp <- abs(mean_data_minmax_sibsp - mean_titanic_minmax_sibsp)
mean_difference_minmax_sibsp
mean_difference_minmax_parch <- abs(mean_data_minmax_parch - mean_titanic_minmax_parch)
mean_difference_minmax_parch

# Gráfico de barras para comparar las diferencias en las medias
barplot(c(mean_difference_maxabs_age, mean_difference_maxabs_fare, mean_difference_maxabs_sibsp, mean_difference_maxabs_parch,
          mean_difference_stand_age, mean_difference_stand_fare, mean_difference_stand_sibsp, mean_difference_stand_parch,
          mean_difference_minmax_age, mean_difference_minmax_fare, mean_difference_minmax_sibsp, mean_difference_minmax_parch),
        names.arg = c("MaxAbsScaler - Age", "MaxAbsScaler - Fare", "MaxAbsScaler - SibSp", "MaxAbsScaler - Parch",
                      "Standarization (Z-score) - Age", "Standarization (Z-score) - Fare", "Standarization (Z-score) - SibSp", "Standarization (Z-score) - Parch",
                      "Min-Max Scaling - Age", "Min-Max Scaling - Fare", "Min-Max Scaling - SibSp", "Min-Max Scaling - Parch"),
        main = "Diferencia en Medias de Age, Fare, SibSp y Parch entre Conjuntos de Datos Normalizados",
        ylab = "Diferencia en Medias")

