install.packages("e1071")
library(moments)         #para asimetria y curtosis
library(readxl)
library(ggplot2)

BD <- read_excel("Descargas/base de datos 1.xlsx")

#ESCOLARIDAD POR GÉNERO

# Promedio de escolaridad por género
prom_escolaridad <-round(tapply(BD$`Escolaridad (años)`, BD$Genero, mean), 0)
print(prom_escolaridad)
# Desviación estándar por género
desviacion_est <-round(tapply(BD$`Escolaridad (años)`, BD$Genero, sd),2)
print(desviacion_est)
# Cuartiles por género
tapply(BD$`Escolaridad (años)`, BD$Genero, quantile, probs = c(0.25, 0.5, 0.75))
# Coeficiente de variación por género
round(tapply(BD$`Escolaridad (años)`, BD$Genero, function(x) sd(x) / mean(x) * 100),2)


##########################################################
#HOMBRES
# Filtrar solo hombres
BD_hombres <- subset(BD, Genero == "Hombre")
################################################################
#promedio de escolaridad por estado civíl 
promedio_escolaridad_hombres <- aggregate(BD_hombres$`Escolaridad (años)`, 
                                          by = list(BD_hombres$`Estado Civil`), 
                                          FUN = mean)
# Renombrar columnas
colnames(promedio_escolaridad_hombres) <- c("Estado Civil", "Promedio de Escolaridad")
# Redondear a 2 decimales
promedio_escolaridad_hombres$`Promedio de Escolaridad` <- round(promedio_escolaridad_hombres$`Promedio de Escolaridad`, 0)

# Calcular promedio y desviación estándar de escolaridad para hombres
promedio_hombres <- round(mean(BD_hombres$`Escolaridad (años)`), 0)
desviacion_hombres <- round(sd(BD_hombres$`Escolaridad (años)`), 2)
##########################################################################
# Calcular el promedio de hijos por estado civil en hombres
promedio_hijos_hombres <- aggregate(BD_hombres$Hijos, by = list(BD_hombres$`Estado Civil`), FUN = mean)
# Renombrar las columnas
colnames(promedio_hijos_hombres) <- c("Estado Civil", "Promedio de Hijos")
# Redondear el promedio a 2 decimales
promedio_hijos_hombres$`Promedio de Hijos` <- round(promedio_hijos_hombres$`Promedio de Hijos`, 2)
# Mostrar la tabla
print(promedio_hijos_hombres)
########################################################################

# Cuartiles 
cuartiles_hombres <-tapply(BD_hombres$`Escolaridad (años)`, BD_hombres$Genero, quantile)
# Calcular mínimo y máximo de escolaridad por estado civil
minimo_hombres <- tapply(BD_hombres$`Escolaridad (años)`, BD_hombres$`Estado Civil`, min)
maximo_hombres <- tapply(BD_hombres$`Escolaridad (años)`, BD_hombres$`Estado Civil`, max)
# Crear la matriz max_min_hombres
max_min_hombres <- matrix(c(minimo_hombres, maximo_hombres), 
                             nrow = 2, 
                             byrow = TRUE,
                             dimnames = list(
                               c("Mínimo Escolaridad", "Máximo Escolaridad"),
                               names(minimo_hombres)
                             ))

asimetria_H <- round(skewness(BD_hombres$`Escolaridad (años)`),2)
curtosis_H <- round(kurtosis(BD_hombres$`Escolaridad (años)`),2)
########################################################################3
# Imprimir resultados de hombres
print(max_min_hombres)
cat("La desviación de los datos de escolaridad es:\n", desviacion_hombres, "\n")
cat("El promedio de años cursados es:\n", promedio_hombres, "\n")
print(cuartiles_hombres)
cat("Asimetría en los datos de escolaridad:", asimetria_H, "\n")
cat("Curtosis en los datos de escolaridad:", curtosis_H, "\n")
#promedio de hijos por estado civil
tapply(BD_hombres$Hijos, BD_hombres$`Estado Civil`, sum)
print(promedio_escolaridad_hombres) #promedio de escolaridad por estado civil




#######################################################################
#MUJERES
# Filtrar solo mujeres
BD_mujeres <- subset(BD, Genero == "Mujer")

#promedio de escolaridad por estado civíl 
promedio_escolaridad_mujeres <- aggregate(BD_mujeres$`Escolaridad (años)`, 
                                          by = list(BD_mujeres$`Estado Civil`),                                           FUN = mean)
# Renombrar columnas
colnames(promedio_escolaridad_mujeres) <- c("Estado Civil", "Promedio de Escolaridad")
# Redondear a 2 decimales
promedio_escolaridad_mujeres$`Promedio de Escolaridad` <- round(promedio_escolaridad_mujeres$`Promedio de Escolaridad`, 0)
#######################################################################
# Calcular promedio y desviación estándar de escolaridad para mujeres
promedio_mujeres <- round(mean(BD_mujeres$`Escolaridad (años)`), 0)
desviacion_mujeres <- round(sd(BD_mujeres$`Escolaridad (años)`), 2)
# Cuartiles 
cuartiles_mujeres <-tapply(BD_mujeres$`Escolaridad (años)`, BD_mujeres$Genero, quantile)
# Calcular mínimo y máximo de escolaridad por estado civil
minimo_mujeres <- tapply(BD_mujeres$`Escolaridad (años)`, BD_mujeres$`Estado Civil`, min)
maximo_mujeres <- tapply(BD_mujeres$`Escolaridad (años)`, BD_mujeres$`Estado Civil`, max)
# Crear la matriz max_min_mujeres
max_min_mujeres <- matrix(c(minimo_mujeres, maximo_mujeres), 
                          nrow = 2, 
                          byrow = TRUE,
                          dimnames = list(
                            c("Mínimo Escolaridad", "Máximo Escolaridad"),
                            names(minimo_mujeres)
                          ))

asimetria_M <- round(skewness(BD_mujeres$`Escolaridad (años)`),2)
curtosis_M <- round(kurtosis(BD_mujeres$`Escolaridad (años)`),2)


##################################################333
# Imprimir resultados de mujeres
print(max_min_mujeres)
cat("La desviación de los datos de escolaridad es:\n", desviacion_mujeres, "\n")
cat("El promedio de años cursados es:\n", promedio_mujeres, "\n")
print(cuartiles_mujeres)
cat("Asimetría en los datos de escolaridad:", asimetria_M, "\n")
cat("Curtosis en los datos de escolaridad:", curtosis_M, "\n")
print(promedio_escolaridad_mujeres) #promedio de escolaridad por estado civil
################################################################################
# Calcular el promedio de hijos por estado civil en mujeres
promedio_hijos_mujeres <- aggregate(BD_mujeres$Hijos, by = list(BD_mujeres$`Estado Civil`), FUN = mean)
# Renombrar las columnas
colnames(promedio_hijos_mujeres) <- c("Estado Civil", "Promedio de Hijos")
# Redondear el promedio a 2 decimales
promedio_hijos_mujeres$`Promedio de Hijos` <- round(promedio_hijos_mujeres$`Promedio de Hijos`, 0)
# Mostrar la tabla
print(promedio_hijos_mujeres)

###############################################################################

#GRÁFICAS MUJERES
# Graficar el histograma con todos los valores de escolaridad de mujeres
hist(BD_mujeres$`Escolaridad (años)`, 
     col = "pink", border = "black",
     main = "Distribución de Escolaridad en Mujeres",
     xlab = "Años de Escolaridad", 
     ylab = "Frecuencia",
     breaks = 10)  # Ajusta el número de barras

boxplot(cuartiles_mujeres, col = "pink", main = "Cuartiles de Escolaridad en Mujeres",
        ylab = "Años de Escolaridad", xlab = "Mujeres")
#################################################################3
# Calcular el promedio de escolaridad por estado civil
prom_escolaridad_estado1 <- aggregate(BD_mujeres$`Escolaridad (años)`, 
                                     by = list(BD_mujeres$`Estado Civil`), 
                                     FUN = mean)
# Convertir el resultado en un dataframe
prom_escolaridad_estado1 <- as.data.frame(prom_escolaridad_estado1)
# Renombrar columnas 
colnames(prom_escolaridad_estado1) <- c("Estado Civil", "Promedio Escolaridad")
# Graficar
ggplot(prom_escolaridad_estado1, aes(x = `Estado Civil`, y = `Promedio Escolaridad`, fill = `Estado Civil`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(`Promedio Escolaridad`, 1)),  # Mostrar el número en la barra
            vjust = -0.5,  # Posición sobre la barra
            size = 5) +    # Tamaño del texto
  labs(title = "Promedio de Escolaridad por Estado Civil", 
       x = "Estado Civil", 
       y = "Promedio de Escolaridad (años)") +
  theme_minimal()
##################################################################
#gráfico de barras para el promedio de número de hijos por estado civil

ggplot(promedio_hijos_mujeres, aes(x = `Estado Civil`, y = `Promedio de Hijos`, fill = `Estado Civil`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = `Promedio de Hijos`), vjust = -0.5, size = 5) +  
  labs(title = "Promedio de Hijos por Estado Civil en mujeres",
       x = "Estado Civil",
       y = "Promedio de Hijos") +
  theme_minimal() +
  theme(legend.position = "none")  # Ocultar la leyenda, ya que los colores representan los estados civiles


############################################################################
#GRÁFICAS HOMBRES
# Hstograma con todos los valores de escolaridad de hombres
hist(BD_hombres$`Escolaridad (años)`, 
     col = "blue", border = "black",
     main = "Distribución de Escolaridad en Hombres",
     xlab = "Años de Escolaridad", 
     ylab = "Frecuencia",
     breaks = 10)  # núm de barras

#boxplot cuartiles
boxplot(cuartiles_hombres, col = "blue",main = "Cuartiles de Escolaridad en Hombres",
        ylab = "Años de Escolaridad", xlab = "Hombres")

###############################################################################
# Calcular el promedio de escolaridad por estado civil
prom_escolaridad_estado <- aggregate(BD_hombres$`Escolaridad (años)`, 
                                     by = list(BD_hombres$`Estado Civil`), 
                                     FUN = mean)
# Convertir el resultado en un dataframe
prom_escolaridad_estado <- as.data.frame(prom_escolaridad_estado)
# Renombrar columnas 
colnames(prom_escolaridad_estado) <- c("Estado Civil", "Promedio Escolaridad")
# Graficar
ggplot(prom_escolaridad_estado, aes(x = `Estado Civil`, y = `Promedio Escolaridad`, fill = `Estado Civil`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(`Promedio Escolaridad`, 1)),  # Mostrar el número en la barra
            vjust = -0.5,  # Posición sobre la barra
            size = 5) +    # Tamaño del texto
  labs(title = "Promedio de Escolaridad por Estado Civil", 
       x = "Estado Civil", 
       y = "Promedio de Escolaridad (años)") +
  theme_minimal()

#######################################################################
#gráfico de barras para el promedio de número de hijos por estado civil

ggplot(promedio_hijos_hombres, aes(x = `Estado Civil`, y = `Promedio de Hijos`, fill = `Estado Civil`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = `Promedio de Hijos`), vjust = -0.5, size = 5) +  # Mostrar valores en las barras
  labs(title = "Promedio de Hijos por Estado Civil en Hombres",
       x = "Estado Civil",
       y = "Promedio de Hijos") +
  theme_minimal() +
  theme(legend.position = "none")  # Ocultar la leyenda, ya que los colores representan los estados civiles


##################################################################
#CRUZAR GŔAFICAS

#HISTOGRAMA ESCOLARIDAD POR GÉNERO
# Definir los mismos breaks para ambos histogramas
breaks_comunes <- seq(min(BD$`Escolaridad (años)`, na.rm = TRUE), 
                      max(BD$`Escolaridad (años)`, na.rm = TRUE), 
                      length.out = 11)  # Ajusta según sea necesario
# Graficar el histograma de mujeres primero
hist(BD_mujeres$`Escolaridad (años)`, 
     col = rgb(1, 0, 0, 0.5),  # Color rojo con transparencia
     border = "black",
     main = "Distribución de Escolaridad por Género",
     xlab = "Años de Escolaridad", 
     ylab = "Frecuencia",
     breaks = breaks_comunes, 
     freq = TRUE)
# Graficar el histograma de hombres superpuesto
hist(BD_hombres$`Escolaridad (años)`, 
     col = rgb(0, 0, 1, 0.5),  # Color azul con transparencia
     border = "black",
     breaks = breaks_comunes, 
     freq = TRUE,
     add = TRUE)  # Superpone los datos
# Agregar leyenda
legend("topright", legend = c("Mujeres", "Hombres"), 
       fill = c(rgb(1, 0, 0, 0.5), rgb(0, 0, 1, 0.5)), border = "black")
#Boxplot cuantiles por género
ggplot(BD, aes(x = Genero, y = `Escolaridad (años)`, fill = Genero)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Mujer" = "pink", "Hombre" = "blue")) +
  labs(
    title = "Boxplot de Escolaridad por Género",
    x = "Género",
    y = "Escolaridad (años)"
  ) +
  theme_minimal()
###############################################################################3
#PIE
#Cantidad de hombres y mujeres
# Calcular la cantidad
genero_frecuencia <- as.data.frame(table(BD$Genero))
# Renombrar columnas
colnames(genero_frecuencia) <- c("Género", "Frecuencia")
# porcentajes
genero_frecuencia$Porcentaje <- round(genero_frecuencia$Frecuencia / sum(genero_frecuencia$Frecuencia) * 100, 1)
# Crear gráfico de pastel
ggplot(genero_frecuencia, aes(x = "", y = Frecuencia, fill = Género)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +  # Convierte a gráfico de pastel
  geom_text(aes(label = paste0(Género, "\n", Frecuencia, " (", Porcentaje, "%)")), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 5) 
  labs(title = "Distribución de Género") 
  theme_void()
##################################################################################
#promedio de hijos por estado civil  
  # Unir ambas bases de datos en una sola
  promedio_hijos_hombres$Genero <- "Hombres"
  promedio_hijos_mujeres$Genero <- "Mujeres"
  
  # Combinar ambas bases de datos
  promedio_hijos_total <- rbind(promedio_hijos_hombres, promedio_hijos_mujeres)
  
  # Crear la gráfica de barras comparativa
  ggplot(promedio_hijos_total, aes(x = `Estado Civil`, y = `Promedio de Hijos`, fill = Genero)) +
    geom_bar(stat = "identity", position = "dodge") +  # "dodge" para que las barras queden lado a lado
    geom_text(aes(label = `Promedio de Hijos`), vjust = -0.5, size = 5, position = position_dodge(0.9)) +  
    labs(title = "Promedio de Hijos por Estado Civil en Hombres y Mujeres",
         x = "Estado Civil",
         y = "Promedio de Hijos") +
    scale_fill_manual(values = c("Hombres" = "blue", "Mujeres" = "pink")) +  # Colores personalizados
    theme_minimal()
  
  