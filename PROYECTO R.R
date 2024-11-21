##Pregunta 1
##Leemos los datos del documentos y le ponemos nombre 
library(readr)
datos <- read.table("datos-trabajoR.txt", header = TRUE) #Se observan 4 variables, 55 observaciones y 6 tratmientos
head(datos)      # Para poder ver las 6 primeras filas de la tabla
summary(datos)   # Para obtener el resumen estadístico de cada variable
dim(datos)       # Dimensiones de la tabla
str(datos)       # Caracteristicas de las columnas

##Pregunta 2, para crea un boxplot usamos la funcion boxplot() indicando donde se encuentran las variables que queremos contrastar, donde se ubican los datos,
## y usamos pch y col para el formato y el color.
boxplot(Wildtype ~ Tratamiento, data=datos, pch=15,col="coral1" )
boxplot(Sequia ~ Tratamiento, data=datos, pch=15, col="cadetblue2")
boxplot(ExcesoRiego ~ Tratamiento, data=datos, pch=15, col="seagreen3")

##Pregunta 3 y Pregunta 4; para crear el scatterplot usamos la funcion plot(), indicando donde se encuentran las variables que queremos observar,
##usamos main para indicar el titulo, xlab e ylab se usan para indicar el eje y y el eje x,y usamos pch y col para el formato y el color
## Para añadir la leyenda usamos la funcion legend(), indicando donde queremos que se ubique, las variables y usamos pch y col para el formato y el color
plot(datos$Sequia, datos$Wildtype, main="SEQUIA VS WILDTYPE", xlab="Sequia", ylab="Wildtype", pch=16, col=datos$Tratamiento)
legend("bottomright", legend = c("Tratamiento 1","Tratamiento 2","Tratamiento 3", "Tratamiento 4", "Tratamiento 5","Tratamiento 6"), pch=16, col= c(1,2,3,4,5,6))

plot(datos$ExcesoRiego, datos$Wildtype, main= " EXCESO DE RIEGO VS WILDTYPE", xlab="Exceso de Riego", ylab="Wildtype", pch=16, col=datos$Tratamiento)
legend("bottomright", legend = c("Tratamiento 1","Tratamiento 2","Tratamiento 3", "Tratamiento 4", "Tratamiento 5","Tratamiento 6"), pch=16, col= c(1,2,3,4,5,6))

#Pregunta 5; para crear un histograma usamos la funcion hist(), indicando donde se encuentran las variables que queremos observar,
##usamos main para indicar el titulo, xlab se usa para indicar el eje x,y usamos pch y col para el formato y el color

hist(datos$Tratamiento, main="TRATAMIENTO",xlab="Tratamientos",col="coral1", border="black")
hist(datos$Wildtype, main="WILDTYPE", xlab="Wildtype", col="cadetblue2",border="black")
hist(datos$Sequia, main="SEQUIA", xlab="Sequia", col="seagreen3",border="black")
hist(datos$ExcesoRiego, main="EXCESO DE RIEGO", xlab="Riego", col="magenta3",border="black")

#Pregunta 6; usamos la funcion factor() y se guardo en una variable, y usamos head para comprobar que se guardo
tratamiento_factor <- factor(datos$Tratamiento)
head(tratamiento_factor)

#Pregunta 7; para calcular la media y la desviacion estandar se debe usar la funcion tapply() y introducir mean o sd al final para indicar si queremos la media o la desviacion estandar
media_tratamientoswildtype<-tapply(datos$Wildtype,datos$Tratamiento,mean)
media_tratamientoswildtype
media_tratamientosequia<-tapply(datos$Sequia,datos$Tratamiento,mean)
media_tratamientosequia
media_tratamientoriego<-tapply(datos$ExcesoRiego,datos$Tratamiento,mean)
media_tratamientoriego

desviación_tratamientoswildtype<-tapply(datos$Wildtype,datos$Tratamiento,sd)
desviación_tratamientoswildtype
desviación_tratamientosequia<-tapply(datos$Sequia,datos$Tratamiento,sd)
desviación_tratamientosequia
desviación_tratamientoriego<-tapply(datos$ExcesoRiego,datos$Tratamiento,sd)
desviación_tratamientoriego


#Pregunta 8; Para saber cuantos elementos tiene cada tratamiento usamos la funcion table(), la primera fila se trata del nombre asignado a cada tratamiento, y la segunda los elementos que tiene cada tratamiento
Conteo_tratamientos <- table(datos$Tratamiento)
Conteo_tratamientos

#Pregunta 9; extraemos los datos para el tratamiento 1 y 4 con la funcion subset()y guardamos en una variable
levels(tratamiento_factor)
tratamiento_1 <- subset(datos,Tratamiento==levels(tratamiento_factor)[1])
head(tratamiento_1)
tratamiento_4<-subset(datos,Tratamiento== levels(tratamiento_factor)[4])
head(tratamiento_4)

#Pregunta 10; antes de poder analizar los datos seguimos estos pasos: 

# Filtrar los datos para Tratamiento 1 y Tratamiento 5
tratamiento_1 <- subset(datos, Tratamiento == levels(tratamiento_factor)[1])
tratamiento_5 <- subset(datos, Tratamiento == levels(tratamiento_factor)[5])

# Pruebas de normalidad para Tratamiento 1: Wildtype y Sequía
shapiro.test(tratamiento_1$Wildtype)
shapiro.test(tratamiento_1$Sequia)
shapiro.test(tratamiento_1$ExcesoRiego)

# Pruebas de normalidad para Tratamiento 5: Wildtype y ExcesoRiego
shapiro.test(tratamiento_5$Wildtype)
shapiro.test(tratamiento_5$ExcesoRiego)
shapiro.test(tratamiento_5$Sequia)

#p>0.05: Los datos no rechazan la hipótesis nula (distribución normal).
#p≤0.05: Los datos rechazan la hipótesis nula (no distribución normal).
# Todas las distribuciones son normales , menos la del Wildtype y la de la Sequia del Tratamiento 5, ya que se encuentra por debajo de un valor de 0.05


# Comparación del Tratamiento 1
# Comparar Wildtype con Sequía
# Si los datos son normales y las varianzas son iguales, usa t-test de Student
t.test(tratamiento_1$Wildtype, tratamiento_1$Sequia, var.equal = TRUE)

#Comparar Wildtype con ExcesoRiego
# Si los datos son normales y las varianzas son iguales, usa t-test de Student
t.test(tratamiento_1$Wildtype, tratamiento_1$ExcesoRiego, var.equal = TRUE)

# Comparar Sequía con ExcesoRiego
# Si los datos son normales y las varianzas son iguales, usa t-test de Student
t.test(tratamiento_1$Sequia, tratamiento_1$ExcesoRiego, var.equal = TRUE)

#Para el Tratamiento 1, encontramos diferencias estadísticamente significativas:
#Wildtype vs Sequía: La media de Wildtype (4.00) es significativamente mayor que la de Sequía (0.51).
#Wildtype vs ExcesoRiego: La media de Wildtype (4.00) es significativamente menor que la de ExcesoRiego (5.97)
#En Tratamiento 1, t-tests de Student mostraron diferencias significativas en las comparaciones entre Wildtype vs Sequia, Wildtype vs ExcesoRiego, y Sequia vs ExcesoRiego, dado que los valores p fueron muy pequeños (todos < 0.05).


#Comparación del Tratamiento 5, en este caso usamos Wilcox.test ya que no tienen una distribucion normal 
#Comparar Wildtype vs Sequia:
wilcox.test(tratamiento_5$Wildtype, tratamiento_5$Sequia)

#Comparar Wildtype vs ExcesoRiego:
wilcox.test(tratamiento_5$Wildtype, tratamiento_5$ExcesoRiego)

#Comparar Sequia vs ExcesoRiego:
wilcox.test(tratamiento_5$Sequia, tratamiento_5$ExcesoRiego)

#En Tratamiento 5, pruebas de Wilcoxon también indicaron diferencias significativas en las comparaciones entre Wildtype vs Sequia, Wildtype vs ExcesoRiego, y Sequia vs ExcesoRiego, con valores p igualmente muy pequeños.

#Preguta 11; el test de anova se usan cuando queremos comparar las medias de tres o más variables
#Primero debemos separar cada variable del tratamiento q 

Wildtype_1<-tratamiento_1$Wildtype
Sequia_1 <- tratamiento_1$Sequia
ExcesoRiego_1<-tratamiento_1$ExcesoRiego

#Para que sea más sencillo formateamos nuestros datos en una tabla donde una columna son las variables (W,Sy RE) y la otra los datos de la mismas.
datos_anova<- data.frame("Variables"=c("W","W","W","W","W","W","W","W","W","W","S","S","S","S","S","S","S","S","S","S","ER","ER","ER","ER","ER","ER","ER","ER","ER","ER"),"Valores"=c(Wildtype_1,Sequia_1,ExcesoRiego_1))
anova<- aov(Valores ~ Variables,data=datos_anova)
summary(anova)

#FIN :)
