#Clase 1 -----------------------------------------------------------------------------------
#Tamaño muestral y power
#install.packages("pwr")
library(pwr)


datos_hombre <- c(1.8, 5.8, 7.1, 4.6, 5.5, 2.4, 8.3, 1.2)
datos_mujer <- c(9.5, 2.6, 3.7, 4.7, 6.4, 8.4, 3.1, 1.4)

mean_hombre <- mean(datos_hombre)
mean_mujer <- mean(datos_mujer)
sd_hombre <- sd(datos_hombre)
sd_mujer <- sd(datos_mujer)

d <- (mean_hombre - mean_mujer)/ sqrt((sd_hombre^2 + sd_mujer^2)/2)


d

pwr.t.test (d=d, sig.level =0.05, power = 0.8, 
            type ="two.sample", alternative="two.sided")


pwr.t.test (d=d, sig.level =0.05, n = 500.8317, 
            type ="two.sample", alternative="two.sided")





#Estadistica descriptiva
install.packages("medicaldata") #como son numericas mejor apaTables.
library(medicaldata)

datos <- medicaldata::cytomegalovirus
head(datos)
names(datos)
summary(datos)

##Histograma
hist(datos$age, main = "Histograma de la edad", xlab = "Edad (Años)", col="skyblue")



##Tablas de contingencia con tbl_summary

library(gtsummary)

### create dataset
data("Titanic")
df <- as.data.frame(Titanic)
df

### create the table
df %>%
  tbl_summary()

###Or
tbl_summary(df)


df %>%  #Tras ejecutar. The package "cardx" (>= 0.2.3) is required. The package "broom" (>= 1.0.5) is required.
  select(-Freq) %>%
  tbl_summary(by=Survived) %>%
  add_overall() %>%
  add_p()





##moda
library(modeest)
mfv(datos$age)

##Tabla de correlación con apaTables
##https://www.kaggle.com/datasets/uciml/pima-indians-diabetes-database?resource=download
library(apaTables)
data <- read.csv("diabetes.csv", sep = ",", header = TRUE)

apa.cor.table(data, filename="Table_cor.doc")


##Tabla de regresión básica
library(broom) 
library(rempsyc) 

basic.reg <- lm(Outcome ~ .,
                data = data)

apa.reg.table(basic.reg, filename="Table_reg.doc")

basic.reg

##or
library(flextable)
(stats.table <- tidy(basic.reg, conf.int = TRUE))
nice_table(stats.table, broom = "lm")




##tabla de medias y desviacion
library(medicaldata)
datos <- medicaldata::cytomegalovirus
head(datos)
apa.2way.table(iv1 = sex,
               iv2 = race, 
               dv = time.to.transplant, 
               data = datos,
               filename = "table_mean_sd.doc",
               show.marginal.means = TRUE,
               table.number = 8)




### Ejemplo 1


library(htmlTable)

### Creamos un ejemplo de dataframe relacionado con ciencias de la salud
salud_data <- data.frame(
  Paciente = c("Juan", "María", "Pedro"),
  Edad = c(30, 25, 40),
  Diagnóstico = c("Diabetes", "Hipertensión", "Asma"),
  Tratamiento = c("Insulina", "Medicamentos", "Inhalador")
)

### Usamos la función htmlTable para crear una tabla HTML
tabla_html <- htmlTable(salud_data, 
                        caption = "Ejemplo de Datos de Ciencias de la Salud")

### Mostramos la tabla HTML
print(tabla_html)






### Ejemplo 2


library(htmlTable)

# Creamos un dataframe más detallado
salud_data_complex <- data.frame(
  Paciente = c("Juan", "María", "Pedro", "Ana", "Luis"),
  Edad = c(30, 25, 40, 35, 50),
  Diagnóstico = c("Diabetes", "Hipertensión", "Asma", "Cáncer", "Artritis"),
  Tratamiento = c("Insulina", "Medicamentos", "Inhalador", "Quimioterapia", "Antiinflamatorios"),
  Fecha_Diagnóstico = as.Date(c("2020-01-15", "2019-06-20", "2021-03-10", "2022-08-05", "2018-11-30")),
  Seguimiento = c(TRUE, TRUE, FALSE, TRUE, FALSE)
)

# Usamos la función htmlTable para crear una tabla HTML personalizada
tabla_html_complex <- htmlTable(salud_data_complex, 
                                caption = "Datos Complejos de Ciencias de la Salud", 
                                align = "c", 
                                rnames = FALSE, 
                                colnames = c("Paciente", "Edad", "Diagnóstico", "Tratamiento", "Fecha de Diagnóstico", "Seguimiento"))

# Mostramos la tabla HTML
print(tabla_html_complex)








#Clase2-------------------------------------------------------------------------------------------------


#Ejercicio de imputacion
### Instalar paquetes si es necesario
install.packages("mice")      # Para imputación múltiple
#install.packages("ggplot2")   # Para visualización
#install.packages("dplyr")     # Para manipulación de datos

### Cargar librerías
library(mice)
library(ggplot2)
library(dplyr)


### Simular datos con valores faltantes
set.seed(123)
n <- 200
datos <- data.frame(
  edad = rnorm(n, mean = 50, sd = 10),
  presion = rnorm(n, mean = 120, sd = 15),
  colesterol = rnorm(n, mean = 200, sd = 30),
  glucosa = rnorm(n, mean = 90, sd = 15),
  imc = rnorm(n, mean = 25, sd = 5)
)

### Introducir valores faltantes (20% en cada variable)
set.seed(456)
for (i in 1:5) {
  datos[sample(1:n, size = 0.2 * n), i] <- NA
}

### --- Imputación múltiple con mice (Regresión) ---
imputado <- mice(datos, method = "pmm", m = 5, seed = 123)
datos_imputados <- complete(imputado)

### --- Modelos de regresión ---
modelo_original <- lm(imc ~ edad + presion + colesterol + glucosa, data = datos, na.action = na.omit)
modelo_imputado <- lm(imc ~ edad + presion + colesterol + glucosa, data = datos_imputados)

### --- Calcular RMSE ---
### Para el modelo original, solo se usa el subset sin NA
### Función para calcular RMSE
rmse <- function(y_true, y_pred) {
  sqrt(mean((y_true - y_pred)^2, na.rm = TRUE))
}

rmse_original <- rmse(modelo_original$fitted.values, na.omit(datos)$imc)

### Para el modelo con imputación
rmse_imputado <- rmse(modelo_imputado$fitted.values, datos_imputados$imc)

### Mostrar resultados
print(paste("RMSE del modelo original (con datos faltantes):", round(rmse_original, 3)))
print(paste("RMSE del modelo imputado:", round(rmse_imputado, 3)))

### --- Comparación visual ---
par(mfrow = c(1, 2))
hist(datos$glucosa, main = "Glucosa Original", xlab = "Glucosa", col = "lightblue", breaks = 15)
hist(datos_imputados$glucosa, main = "Glucosa Imputada", xlab = "Glucosa", col = "lightgreen", breaks = 15)







#Multiplicidad


##Bonferroni
###Puedes modificar los p-valores de forma individual o el umbral.
###Aquí modificaremos todos los p-valores.

### Instalar y cargar tidyverse si es necesario
install.packages("tidyverse")
library(tidyverse)

### P-valores obtenidos de 5 pruebas estadísticas
p_vals <- c(0.04, 0.003, 0.25, 0.01, 0.06)

### Aplicar la corrección de Benjamini-Hochberg (FDR)
p_vals_b <- p.adjust(p_vals, method = "bonferroni")
p_vals_b

### Crear un dataframe con resultados
resultados <- data.frame(
  P_Valor = p_vals,
  P_Ajustado_B = p_vals_b
)

resultados




##Benjamini-Hochberg


library(tidyverse)

### P-valores obtenidos de 5 pruebas estadísticas
p_vals <- c(0.04, 0.003, 0.25, 0.01, 0.06)

### Aplicar la corrección de Benjamini-Hochberg (FDR)
p_vals_bh <- p.adjust(p_vals, method = "BH")
p_vals_bh

### Crear un dataframe con resultados
resultados <- data.frame(
  P_Valor = p_vals,
  P_Ajustado_BH = p_vals_bh
)

resultados










#Contraste de hipótesis
#Normalidad
set.seed(123)
# Generar datos de concentración de proteína más asimétricos antes y después del tratamiento
n <- 100  # número de muestras
antes <- rlnorm(n, meanlog = 4.5, sdlog = 0.8)  # concentración antes del tratamiento (distribución log-normal más asimétrica)
tras <- rlnorm(n, meanlog = 4.6, sdlog = 0.8)  # concentración después del tratamiento (distribución log-normal más asimétrica)
# Crear un dataframe
datos_asimetricos_mas <- data.frame(
  Grupo = rep(c('Antes', 'Después'), each = n),
  Concentracion = c(antes, tras)
)

# Mostrar las primeras filas del dataframe asimétrico
print(head(datos_asimetricos_mas))
library(ggplot2)

# Crear un boxplot asimétrico
boxplot_asimetrico_mas <- ggplot(datos_asimetricos_mas, aes(x = Grupo, y = Concentracion, fill = Grupo)) + 
  geom_boxplot() + 
  labs(title = 'Boxplot Asimétrico de Concentración de Proteína (Más Asimétrico)', y = 'Concentración', x = 'Grupo') + 
  theme_minimal()
print(boxplot_asimetrico_mas)

#Otra forma
par(mfrow = c(1, 1))
with(datos_asimetricos_mas, boxplot(Concentracion~Grupo))


# Crear un histograma asimétrico
histograma_asimetrico_mas <- ggplot(datos_asimetricos_mas, aes(x = Concentracion, fill = Grupo)) + 
  geom_histogram(position = 'identity', alpha = 0.5, bins = 30) + 
  labs(title = 'Histograma Asimétrico de Concentración de Proteína (Más Asimétrico)', x = 'Concentración', y = 'Frecuencia') + 
  theme_minimal()
print(histograma_asimetrico_mas)

#Otra forma
par(mfrow = c(1, 1))
with(datos_asimetricos_mas, hist(Concentracion~Grupo))
#No permite la fórmula (Concentracion ~ Grupo)



##QQ-plot Como no tengo grupos lo hago para toda la variable, en el caso de que los hubiera seria por grupos
par(mfrow = c(1, 1))
library(car)  # Para usar qqPlot()

### Función para remover outliers en Concentracion usando percentiles 5% y 95%
remove_outliers <- function(x, probs = c(0.05, 0.95)) {
  qnt <- quantile(x, probs = probs, na.rm = TRUE)  # Calcular percentiles
  x[x < qnt[1] | x > qnt[2]] <- NA  # Reemplazar outliers con NA
  return(x)
}

### Aplicar la función solo a la variable Concentracion
datos_filtrados <- datos_asimetricos_mas
datos_filtrados$Concentracion <- remove_outliers(datos_filtrados$Concentracion)

### Graficar el QQ-Plot sin outliers
qqPlot(datos_filtrados$Concentracion, col = "blue", main = "QQ Plot de Concentracion (sin outliers)")



### Con outliers y sin IC
with(datos_asimetricos_mas, qqnorm(Concentracion, main="Normal QQplot" ));with(datos_asimetricos_mas, qqline(Concentracion) )




#Shapiro-Wilks Como no tengo grupos lo hago para toda la variable, en el caso de que los hubiera seria por grupos

shapiro.test(datos_asimetricos_mas$Concentracion)


##En bucle iterado

library(dplyr)

# Generar datos biomédicos simulados
set.seed(123)  # Para reproducibilidad
n <- 100  # Número de pacientes

myfile <- data.frame(
  Edad = rnorm(n, mean = 50, sd = 15),  # Edad promedio de 50 años
  Presion_Arterial = rnorm(n, mean = 120, sd = 15),  # Presión arterial sistólica
  Colesterol = rnorm(n, mean = 200, sd = 40),  # Niveles de colesterol total
  Glucosa = rnorm(n, mean = 100, sd = 20),  # Glucosa en ayunas
  IMC = rnorm(n, mean = 25, sd = 5)  # Índice de masa corporal
)

# Ver primeras filas del dataset
head(myfile)

variables <- colnames(myfile)
shapiro_results <- vector("numeric", length = length(variables))

# Realizar el test de Shapiro-Wilk para cada variable después de remover outliers
for (i in 1:length(variables)) {
  # Remover outliers (valores por encima del percentil 0.95)
  data_without_outliers <- remove_outliers(myfile[[variables[i]]], c(0.95))
  
  # Ejecutar el test de Shapiro-Wilk con los datos sin outliers
  shapiro_results[i] <- shapiro.test(data_without_outliers)$p.value
}

# Crear un dataframe con los resultados
results_df <- data.frame(Variable = variables, p_value = shapiro_results)
print(results_df)




#Test de Levene

library(car)
leveneTest(Concentracion ~ Grupo, data = datos_asimetricos_mas)








#Contraste de hipótesis
##Ejercicio 1
library("MedDataSets")
View(anorexia_df)
str(anorexia_df)
anorexia_df$Change <- anorexia_df$Postwt - anorexia_df$Prewt

with(anorexia_df, boxplot(Change ~ Treat))

### Prueba de normalidad conjunta X
with(anorexia_df, shapiro.test(Change) )

### Prueba de normalidad de cada grupo por separado
shapiro.test(anorexia_df$Change[anorexia_df$Treat == "CBT"])
shapiro.test(anorexia_df$Change[anorexia_df$Treat == "Cont"])
shapiro.test(anorexia_df$Change[anorexia_df$Treat == "FT"])

###with(anorexia_df, leveneTest(Change ~ Treat))  Contrasta la igualdad de varianzas (no se usará en test no parametricos) 

with(anorexia_df, kruskal.test(Change ~ Treat))




##Ejercicio 2




data <- data.frame(
  Genero = c("Male", "Male", "Male", "Male", "Male", "Male", "Male", "Male", "Female", "Female", "Female", "Female", "Female", "Female", "Female", "Female"),
  Proteina = c(1.8, 5.8, 7.1, 4.6, 5.5, 2.4, 8.3, 1.2, 9.5, 2.6, 3.7, 4.7, 6.4, 8.4, 3.1, 1.4))

with(data, boxplot(Proteina ~ Genero))

### Prueba de normalidad de cada grupo por separado
shapiro.test(data$Proteina[data$Genero == "Male"])
shapiro.test(data$Proteina[data$Genero == "Female"])

###----------------------------------------------------
with(data, leveneTest(Proteina ~ Genero))
with(data, t.test(Proteina ~ Genero), var.equal=TRUE)
###----------------------------------------------------

with(data, t.test(Proteina ~ Genero), var.equal=FALSE) # test de Welch (t-test varianzas desiguales)




## Ejercicio 3 para vosotros

library("MedDataSets")
View(infert_df)

str(infert_df)

### infert_df$education <- as.factor(infert_df$education) Si no fuese factor, habría que pasarlo a este.

with(infert_df, boxplot(parity ~ education))

### Prueba de normalidad de cada grupo por separado
shapiro.test(infert_df$parity[infert_df$education == "0-5yrs"])
shapiro.test(infert_df$parity[infert_df$education == "6-11yrs"])
shapiro.test(infert_df$parity[infert_df$education == "12+ yrs"])

with(infert_df, kruskal.test(parity ~ education))



## Ejercicio 4 para vosotros

library("MedDataSets")
View(infert_df)

str(infert_df)

infert_df$case <- as.factor(infert_df$case)

with(infert_df, boxplot(spontaneous ~ case))

### Prueba de normalidad de cada grupo por separado
shapiro.test(infert_df$spontaneous[infert_df$case == "0"])
shapiro.test(infert_df$spontaneous[infert_df$case == "1"])


with(infert_df, kruskal.test(parity ~ education))

wilcox.test(spontaneous ~ case, data = infert_df)




