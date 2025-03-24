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

### --- Calcular RMSE (Promedio de cuanto se desvian las predicciones de los datos reales) ---
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

### Aplicar la corrección de bonferroni
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
with(datos_asimetricos_mas, hist(Concentracion))
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




wilcox.test(spontaneous ~ case, data = infert_df)






#Visualización










#Clase3-------------------------------------------------------------------------------------------------



#Análisis Multivariado
##Selección de variables

###Redundancia


library(medicaldata)

datos <- medicaldata::cytomegalovirus
head(datos)
names(datos)
summary(datos)

cor(datos)

datos <- subset(datos, select = -diagnosis)


#### Calcular la matriz de correlación evitando problemas con valores faltantes
cor_matrix <- cor(datos, use = "complete.obs") 

#### Convertir la matriz a un vector (excluyendo la diagonal)
cor_values <- cor_matrix[lower.tri(cor_matrix)]

#### Encontrar el valor máximo y mínimo
max_cor <- max(cor_values)  # Máxima correlación
min_cor <- min(cor_values)  # Mínima correlación

#### Mostrar los resultados
cat("Máxima correlación:", max_cor, "\n")
cat("Mínima correlación:", min_cor, "\n")


###EDA
library(caret)

datos$FUMAR <- 1
nearZeroVar(datos, freqCut = 95/5, uniqueCut = 10)





###PCA

canceres <- read.csv("pca.csv", header = TRUE, sep = ",")
tipo <- read.csv("class.csv", header = TRUE, sep = ",")

canceres <- canceres[,1:50 ]

pca <- prcomp(canceres, scale = TRUE)
summary(pca)


####Añadimos los componentes
myfile_cor <- canceres
xx<-pca$x
xx<-as.data.frame(xx)
myfile_cor$PC1<-xx$PC1
myfile_cor$PC2<-xx$PC2
myfile_cor$PC3<-xx$PC3
myfile_cor$PC4<-xx$PC4
myfile_cor$PC5<-xx$PC5
myfile_cor$PC6<-xx$PC6
myfile_cor$PC7<-xx$PC7
myfile_cor$PC8<-xx$PC8
myfile_cor$PC9<-xx$PC9
myfile_cor$PC10<-xx$PC10



correlacion <- cor(myfile_cor)

View(correlacion)


library(FactoMineR)
respca2 <- PCA(X = canceres, scale.unit = FALSE, ncp = 10, graph = TRUE) ####Seleccionamos las 10 primeras componentes


##### Extraemos los resultados por variable
library(factoextra)
var <- get_pca_var(respca2)
print(var)

head(var$contrib) #### contributions of variables

#####visualización

fviz_eig(respca2) ####visualizar eigenvalores (scree plot)

fviz_contrib(respca2,choice = "var") ####Representa la contribución de filas/columnas de los resultados de un pca.

fviz_contrib(respca2, choice="var", axes = 1:10)

a <- fviz_contrib(respca2, choice="var", axes = 1:10, top = 12)

####Estas serian las variables que mas contribuirían al PCA y que por tanto explican la mayor parte de la varianza
a



#Tipos de Análisis multivariado
##Correlacion de Pearson con 2 variables


set.seed(123) # Generamos un dataset sintético biomédico.
n <- 100 # tamaño

# variables
cholesterol <- rnorm(n, mean=200, sd=30)
blood_pressure <- 0.5 * cholesterol + rnorm(n, mean=0, sd=10) #Creamos la variable con una fuerte correlación.

# Cremos el data frame
biomedical_data <- data.frame(cholesterol, blood_pressure)

head(biomedical_data)

#Normalidad
shapiro.test(cholesterol)
shapiro.test(blood_pressure)

#Función de correlación
cor(cholesterol,blood_pressure)
cor(biomedical_data)

# cor.test analiza la significancia de la correlacion, utilizando el contraste t-student. Para no malinterpretar correlaciones al azar.
cor.test(cholesterol,blood_pressure)





##Correlacion de spearman con 2 variables

set.seed(123) ## Generamos un dataset sintético biomédico.
n <- 100 # tamaño

##variables
cholesterol <- rlnorm(n, meanlog=5, sdlog=0.5)
#Creamos la variable con una fuerte correlación.
blood_pressure <- rnorm(n, mean=120, sd=15)

# Creamos el data frame
biomedical_data <- data.frame(cholesterol, blood_pressure)

head(biomedical_data)


#Normalidad
shapiro.test(cholesterol)
shapiro.test(blood_pressure)

#Función de correlación
cor(cholesterol,blood_pressure, method = "spearman")

# cor.test analiza la significancia de la correlacion, utilizando el contraste t-student.
cor.test(cholesterol,blood_pressure, method = "spearman")














###Regresión Lineal Simple

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("readxl","factoextra","FactoMineR","caret","car","biotools","MVN","forestmodel","see",
              "jtools","olsrr","parameters","stats","ggplot2", "palmerpenguins", "plot3D" , 
              "plot3Drgl","apaTables","gvlma","broomExtra","performance","MASS","faraway","reshape2","RColorBrewer")
ipak(packages)


#Extraed una muestra de 3000 individuos a partir del archivo original y cread vuestro propio archivo de datos.

library(readxl)
datos <- read_excel("cardioData.xlsx")

df <- datos[1:3000,]
head(df)

df <- as.data.frame(df)
df <- df[complete.cases(df), ]
scatter.smooth(x=df$sbp, y=df$bmi, main="IMC ~ Presión Sistólica")


#Creamos el modelo de regresión lineal simple
linearMod <- lm(bmi ~ sbp, data=df)  #Variable dependiente primero.

#Comparamos los supuestos después de crear el objeto de regresión.

#1. Linealidad: La media de los errores es 0.

mean(linearMod$residuals)

#2. Normalidad: Errores distribuidos de forma normal.
library(MASS)

sresid  <- studres(linearMod)
shapiro.test(sresid) # < 0.05 No cumple normalidad


#3. Homocedasticidad: La varianza de los errores debe ser igual.

ncvTest(linearMod) # < 0.05. La varianza de los errores no es igual y puede haber un problema


#4. Independencia: Los errores deben ser distintos unos de otros. Es
# decir, las covarianzas deben ser 0.

durbinWatsonTest(linearMod) #> 0.05 Los errores son independientes.

##Hemos cumplido los 2 supuestos imprescindibles.


#Vemos la regresión

summary(linearMod)

#otra opción. paquete parameters

model_parameters(linearMod, bootstrap = TRUE, iterations = 200)

#intervalos de confianza
confint(linearMod)

#Anexo-----------------------------------------------------------------

#Para comparar modelos. El modelo con bajo AIC es preferido. (Para más de 1 variable)
linearMod <- lm(bmi ~ sbp, data=df) 
linearMod2 <- lm(bmi ~ dbp, data=df) 

AIC(linearMod)
AIC(linearMod2)

#-----------------------------------------------------------------


#Extraed una muestra aleatoria de 50 individuos a partir del archivo original y cread vuestro propio archivo de datos.

set.seed(12)
datos_n <- datos[sample(nrow(datos[3001:nrow(datos), ]), 500), ]
head(datos_n)

datos_n <- as.data.frame(datos_n)
datos_n <- datos_n[complete.cases(datos_n), ]


#Predicción

IMCPred <- predict(linearMod, datos_n)
actuals_preds <- data.frame(cbind(actuals=datos_n$bmi, predicteds=IMCPred))  
correlation_accuracy <- cor(actuals_preds)  
correlation_accuracy #Mala correlación
head(actuals_preds)

#Visualizaciones

# paq:  jtools

linearMod1 <- lm(bmi ~ sbp, data=df) 
linearMod2 <- lm(bmi ~ dbp, data=df) 
plot_summs(linearMod1, linearMod2, scale = TRUE) #Cuanto más se aleje del 0 mejor estimaría.

#paquete performance
m1 <- lm(bmi ~ sbp, data=df)
m2 <- lm(bmi ~ dbp, data=df) 
m3 <- lm(bmi ~ scl, data=df) 

compare_performance(m1, m2, m3, rank = TRUE)



#Función para visualizar regresiones bonitas

vwReg <- function(formula, data, title="", B=1000, shade=TRUE, shade.alpha=.1, spag=FALSE, spag.color="darkblue", mweight=TRUE, show.lm=FALSE, show.median = TRUE, median.col = "white", shape = 21, show.CI=FALSE, method=loess, bw=FALSE, slices=200, palette=colorRampPalette(c("#FFEDA0", "#DD0000"), bias=2)(20), ylim=NULL, quantize = "continuous",  add=FALSE, ...) {
  IV <- all.vars(formula)[2]
  DV <- all.vars(formula)[1]
  data <- na.omit(data[order(data[, IV]), c(IV, DV)])
  
  if (bw == TRUE) {
    palette <- colorRampPalette(c("#EEEEEE", "#999999", "#333333"), bias=2)(20)
  }
  
  print("Computing boostrapped smoothers ...")
  newx <- data.frame(seq(min(data[, IV]), max(data[, IV]), length=slices))
  colnames(newx) <- IV
  l0.boot <- matrix(NA, nrow=nrow(newx), ncol=B)
  
  l0 <- method(formula, data)
  for (i in 1:B) {
    data2 <- data[sample(nrow(data), replace=TRUE), ]
    data2 <- data2[order(data2[, IV]), ]
    if (class(l0)=="loess") {
      m1 <- method(formula, data2, control = loess.control(surface = "i", statistics="a", trace.hat="a"), ...)
    } else {
      m1 <- method(formula, data2, ...)
    }
    l0.boot[, i] <- predict(m1, newdata=newx)
  }
  
  # compute median and CI limits of bootstrap
  library(plyr)
  library(reshape2)
  CI.boot <- adply(l0.boot, 1, function(x) quantile(x, prob=c(.025, .5, .975, pnorm(c(-3, -2, -1, 0, 1, 2, 3))), na.rm=TRUE))[, -1]
  colnames(CI.boot)[1:10] <- c("LL", "M", "UL", paste0("SD", 1:7))
  CI.boot$x <- newx[, 1]
  CI.boot$width <- CI.boot$UL - CI.boot$LL
  
  # scale the CI width to the range 0 to 1 and flip it (bigger numbers = narrower CI)
  CI.boot$w2 <- (CI.boot$width - min(CI.boot$width))
  CI.boot$w3 <- 1-(CI.boot$w2/max(CI.boot$w2))
  
  
  # convert bootstrapped spaghettis to long format
  b2 <- melt(l0.boot)
  b2$x <- newx[,1]
  colnames(b2) <- c("index", "B", "value", "x")
  
  library(ggplot2)
  library(RColorBrewer)
  
  # Construct ggplot
  # All plot elements are constructed as a list, so they can be added to an existing ggplot
  
  # if add == FALSE: provide the basic ggplot object
  p0 <- ggplot(data, aes_string(x=IV, y=DV)) + theme_bw()
  
  # initialize elements with NULL (if they are defined, they are overwritten with something meaningful)
  gg.tiles <- gg.poly <- gg.spag <- gg.median <- gg.CI1 <- gg.CI2 <- gg.lm <- gg.points <- gg.title <- NULL
  
  if (shade == TRUE) {
    quantize <- match.arg(quantize, c("continuous", "SD"))
    if (quantize == "continuous") {
      print("Computing density estimates for each vertical cut ...")
      flush.console()
      
      if (is.null(ylim)) {
        min_value <- min(min(l0.boot, na.rm=TRUE), min(data[, DV], na.rm=TRUE))
        max_value <- max(max(l0.boot, na.rm=TRUE), max(data[, DV], na.rm=TRUE))
        ylim <- c(min_value, max_value)
      }
      
      # vertical cross-sectional density estimate
      d2 <- ddply(b2[, c("x", "value")], .(x), function(df) {
        res <- data.frame(density(df$value, na.rm=TRUE, n=slices, from=ylim[1], to=ylim[2])[c("x", "y")])
        #res <- data.frame(density(df$value, na.rm=TRUE, n=slices)[c("x", "y")])
        colnames(res) <- c("y", "dens")
        return(res)
      }, .progress="text")
      
      maxdens <- max(d2$dens)
      mindens <- min(d2$dens)
      d2$dens.scaled <- (d2$dens - mindens)/maxdens  
      
      ## Tile approach
      d2$alpha.factor <- d2$dens.scaled^shade.alpha
      gg.tiles <-  list(geom_tile(data=d2, aes(x=x, y=y, fill=dens.scaled, alpha=alpha.factor)), scale_fill_gradientn("dens.scaled", colours=palette), scale_alpha_continuous(range=c(0.001, 1)))
    }
    if (quantize == "SD") {
      ## Polygon approach
      
      SDs <- melt(CI.boot[, c("x", paste0("SD", 1:7))], id.vars="x")
      count <- 0
      d3 <- data.frame()
      col <- c(1,2,3,3,2,1)
      for (i in 1:6) {
        seg1 <- SDs[SDs$variable == paste0("SD", i), ]
        seg2 <- SDs[SDs$variable == paste0("SD", i+1), ]
        seg <- rbind(seg1, seg2[nrow(seg2):1, ])
        seg$group <- count
        seg$col <- col[i]
        count <- count + 1
        d3 <- rbind(d3, seg)
      }
      
      gg.poly <-  list(geom_polygon(data=d3, aes(x=x, y=value, color=NULL, fill=col, group=group)), scale_fill_gradientn("dens.scaled", colours=palette, values=seq(-1, 3, 1)))
    }
  }
  
  print("Build ggplot figure ...")
  flush.console()
  
  
  if (spag==TRUE) {
    gg.spag <-  geom_path(data=b2, aes(x=x, y=value, group=B), size=0.7, alpha=10/B, color=spag.color)
  }
  
  if (show.median == TRUE) {
    if (mweight == TRUE) {
      gg.median <-  geom_path(data=CI.boot, aes(x=x, y=M, alpha=w3^3), size=.6, linejoin="mitre", color=median.col)
    } else {
      gg.median <-  geom_path(data=CI.boot, aes(x=x, y=M), size = 0.6, linejoin="mitre", color=median.col)
    }
  }
  
  # Confidence limits
  if (show.CI == TRUE) {
    gg.CI1 <- geom_path(data=CI.boot, aes(x=x, y=UL), size=1, color="red")
    gg.CI2 <- geom_path(data=CI.boot, aes(x=x, y=LL), size=1, color="red")
  }
  
  # plain linear regression line
  if (show.lm==TRUE) {gg.lm <- geom_smooth(method="lm", color="darkgreen", se=FALSE)}
  
  gg.points <- geom_point(data=data, aes_string(x=IV, y=DV), size=1, shape=shape, fill="white", color="black")       
  
  if (title != "") {
    gg.title <- theme(title=title)
  }
  
  
  gg.elements <- list(gg.tiles, gg.poly, gg.spag, gg.median, gg.CI1, gg.CI2, gg.lm, gg.points, gg.title, theme(legend.position="none"))
  
  if (add == FALSE) {
    return(p0 + gg.elements)
  } else {
    return(gg.elements)
  }
}

vwReg(bmi ~ sbp,df)









#Regresión Lineal múltiple


library(faraway)
data(prostate, package="faraway")
datos <- prostate
str(datos)

datos_t <- datos[91:97,]
datos <- datos[1:90,]


model <- lm(lpsa ~., data=datos)
sumary(model)

rmse <- function(y_true, y_pred) {
  sqrt(mean((y_true - y_pred)^2, na.rm = TRUE))
}

rmse_modelo <- rmse(model$fitted.values, na.omit(datos)$lpsa)
rmse_modelo


#Podriamos eliminar predictores uno a uno empezando por los de mayor p-valor.
#o segun el creiterio de AKAIKE (AIC).

step(lm(lpsa~.,data=datos),direction="backward") #Si el AIC baja quitando una variable, la eliminará.

model <- lm(lpsa ~ lcavol + lweight + age + lbph + svi, data=datos)
sumary(model)

rmse_modelo <- rmse(model$fitted.values, na.omit(datos)$lpsa)
rmse_modelo

#Hemos eliminado 3 variables y hemos mantenido el RMSE BAJO.

vif(model) #No hay correlación entre predictores al ser menor de 5.



linearMod <- model



#Comparamos los supuestos después de crear el objeto de regresión.

#1. Linealidad: La media de los errores es 0.

mean(linearMod$residuals)

#2. Normalidad: Errores distribuidos de forma normal.
library(MASS)

sresid  <- studres(linearMod)
shapiro.test(sresid) # > 0.05 cumple la normalidad


#3. Homocedasticidad: La varianza de los errores debe ser igual.

ncvTest(linearMod) # > 0.05 no hay indicios de que la varianza de los residuos cambie.


#4. Independencia: Los errores deben ser distintos unos de otros. Es
# decir, las covarianzas deben ser 0.

durbinWatsonTest(linearMod) #< 0.05 Los errores no son independientes.

#Nos detendríamos aquí para el modelo de regresión lineal múltiple. Pero vamos a continuar.
#Por lo que sería ideal usar GLS (ignora la homocedasticidad e independencia)




#mejores presentaciones de resultados.
model_parameters(linearMod, bootstrap = TRUE, iterations = 500)

ols_step_both_p(linearMod) # Otra forma de eliminar las variables no significativas

summary(linearMod)


#Actualizamos el modelo
model <- lm(lpsa ~ lcavol + lweight + svi, data=datos)
sumary(model)

rmse_modelo <- rmse(model$fitted.values, na.omit(datos)$lpsa)
rmse_modelo


#intervalos de confianza
confint(model)

#ciertos indicadores
AIC(model)

#Prediccion
lpsaPred <- predict(model, datos_t)
actuals_preds <- data.frame(cbind(actuals=datos_t$lpsa, predicteds=lpsaPred))  
correlation_accuracy <- cor(actuals_preds)  
correlation_accuracy #Mala correlación
head(actuals_preds)



#Tablas apañadas
library(forestmodel)
forest_model(model)

apa.reg.table(model, filename = "tablaregresion.doc", table.number = NA,
              prop.var.conf.level = 0.95)





#representación 3D de la regesión con dos predictoras
z<-datos$lpsa
y<-datos$lcavol
x<-datos$lweight

scatter3D(x, y, z, theta = 15, phi = 20)
scatter3D(x, y, z, phi = 0, bty ="g")
scatter3D(x, y, z, pch = 18,  theta = 20, phi = 20,
          main = "XX", xlab = "lweight",
          ylab ="lcavol", zlab = "lpsa")

scatter3D(x, y, z, phi = 0, bty = "g",
          pch = 20, cex = 2, ticktype = "detailed",xlab = "lweight",
          ylab ="lcavol", zlab = "lpsa")

#es importante que los valores z sean de la dependiente. 
#creamos otro obj de regresión
objr<-lm(z ~ x+y)
objr
#preparamos el modelado 3d
grid.lines = 26
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred)
z.pred <- matrix(predict(objr, newdata = xy), 
                 nrow = grid.lines, ncol = grid.lines)

# Marcamos las líneas de iteracción para que busquen la recta de regresión
fitpoints <- predict(objr)
#ploteamos la gráfica en 3d con recta de regresión
scatter3D(x, y, z, pch = 18, cex = 2, 
          theta = 20, phi = 20, ticktype = "detailed",
          xlab = "lweight", ylab = "lcavol", zlab = "lpsa",  
          surf = list(x = x.pred, y = y.pred, z = z.pred,  
                      facets = NA, fit = fitpoints), main = "")
#la hacemos interactiva (coge el último gráfico que hayamos creado)
plotrgl()
#lo guardamos como gif en nuestro working directory
movie3d(spin3d(axis = c(0, 0, 1)), duration = 15,
        dir = getwd())




#MANOVA

set.seed(123)  # Para reproducibilidad

# Crear un dataframe con datos ficticios
data <- data.frame(
  Grupo = rep(c("Control", "Tratado"), each = 15),  # 15 pacientes por grupo
  PAS = c(rnorm(15, mean = 130, sd = 10), rnorm(15, mean = 120, sd = 10)), # Presión Sistólica
  PAD = c(rnorm(15, mean = 85, sd = 5), rnorm(15, mean = 78, sd = 5))      # Presión Diastólica
)


#Hacemos la normalidad multivariada en las variables dependientes
#(si no la cumple usamos PERMANOVA)
library(MVN)
# Evaluar normalidad multivariada en las variables dependientes (PAS y PAD)
mvn(data[, 2:3], mvnTest = "hz") #0.9032594 YES   "la cumple"


#Varianza
library(car)

# Prueba de Levene para PAS (Presión Sistólica)
leveneTest(PAS ~ Grupo, data = data)

# Prueba de Levene para PAD (Presión Diastólica)
leveneTest(PAD ~ Grupo, data = data)
#Se cumple la homogeneidad de varianzas.


#Covarianza
library(biotools)

# Prueba de Box para la homogeneidad de matrices de covarianza
boxM(data[,c(2,3)],grouping = data[,c(1)])
#Se cumple la homogeneidad de matrices de covarianza


#Manova
modelo_manova <- manova(cbind(PAS, PAD) ~ Grupo, data = data)

# Resumen del modelo MANOVA
summary(modelo_manova)

#Conclusión: Hay diferencias significativas entre los grupos (Control vs. 
#Tratado) con respecto a las variables dependientes combinadas (PAS y PAD).





