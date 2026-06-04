# Práctico 6: Análisis multivariado (parte I)

En este realizaremos diversos análisis multivariados en R. Primero realizaremos un análisis multivariado de varianza (MANOVA) y luego un análisis de componentes principales (PCA):

---
## 1. Análisis de varianza multivariado (MANOVA)

Descargar los datos contenidos en el archivo Excel [dataR2](https://github.com/BioCastaneda/Biostat/blob/main/dataR2.xlsx)

Este set de datos consta de 116 observaciones, de las cuales 64 pacientes tienen cáncer de mama y 52 forman parte del grupo de control. El conjunto de datos consta de 10 variables: Edad (años), IMC (kg/m²), Glucosa (mg/dL), Insulina (µU/mL), HOMA, Leptina (ng/mL), Adiponectina (µg/mL), Resistina (ng/mL), MCP-1 (pg/dL), y Clasificación (1 = controles sanos, 2 = pacientes con cáncer). Ver [paper](https://github.com/BioCastaneda/Biostat/blob/main/s12885-017-3877-1.pdf)

```
## Cargar los siguientes paquetes
library(ggpubr)
library(rstatix)
library(readxl)

## cargar los datos
data1 <- read_xlsx("dataR2.xlsx")
head(data1)
str(data1)
data1$Classification <- as.factor(data1$Classification)
levels(data1$Classification)
```

Estimar las correlaciones entre todas las variables.
```
data2 <- data1[,-10]
cor.mat <- data2 %>% cor_mat()
cor.mat
#
# Veamos qué correlaciones son significativas.
cor.mat %>% cor_get_pval()
cor.mat %>% cor_mark_significant()
#
# Grafiquemos la matriz de correlación
cor.mat %>% pull_lower_triangle() %>% cor_plot()
```

Ahora evaluaremos los supuestos del MANOVA
```
## Primero de forma normalidad univariada
shapiro.test(data1$Age)
shapiro.test(data1$BMI)
shapiro.test(data1$Glucose)
shapiro.test(data1$Insulin)
shapiro.test(data1$HOMA)
shapiro.test(data1$Leptin)
shapiro.test(data1$Adiponectin)
shapiro.test(data1$Resistin)
shapiro.test(data1$MCP.1)

## Luego normalidad multivariada
library(MVN)
mvn(data1[,c(1:9)],mvn_test="hz")
#
## Ahora la homogeneidad de varianzas
library(car)
leveneTest(data1$Age ~ data1$Classification)
leveneTest(data1$BMI ~ data1$Classification)
leveneTest(data1$Glucose ~ data1$Classification)
leveneTest(data1$Insulin ~ data1$Classification)
leveneTest(data1$HOMA ~ data1$Classification)
leveneTest(data1$Leptin ~ data1$Classification)
leveneTest(data1$Adiponectin ~ data1$Classification)
leveneTest(data1$Resistin ~ data1$Classification)
leveneTest(data1$MCP.1 ~ data1$Classification)
#
## Ahora evaluaremos la presencia de outliers
mahalanobis_distance(data = data1[, c(1:9)])$is.outlier
```

Ahora realizaremos el MANOVA
```
data1$newVar <- cbind(data1$Age,data1$BMI,data1$Glucose,data1$Insulin,data1$HOMA,
                data1$Leptin,data1$Adiponectin,data1$Resistin,data1$MCP.1)
m0 <- manova(newVar ~ Classification, data=data1)
anova(m0, test="Wilks")
#
## Cálcular el tamaño del efecto
library(effectsize)
eta_squared(m0)
#
## Revisar los resultados del ANOVA para cada variable
summary.aov(m0)
```

Gráfiquemos una de las variables: Insulina
```
plot1 <- ggboxplot(data1, x="Classification", y="Insulina", col="black", ylab="Insulina (µU/mL)", xlab="Condición", add="jitter") +
            scale_x_discrete(labels = c("Control", "Cáncer"))
plot1
```

---
## 2. Análisis de componentes principales

Realicemos el PCA
```
## Primero cargamos las librerías
library(ggpubr)
library(car)
library(readxl)
library(factoextra)
library(FactoMineR)

## Correr el PCA
ds.pca <- PCA(data2, graph=T)

## Calcular los eigenvalues para cada componente principal
ds.pca$eig

# Calcular la contribución de las variables originales a cada uno de los componentes principales
ds.pca$var$coord

# Calcular las coordenadasde cada muestra respecto a los componentes principales
ds.pca$ind$coord
```

Grafiquemos la varianza explicada por los componentes principales
```
fviz_eig(ds.pca, addlabels = TRUE, ylim = c(0, 80))
```

Grafiquemos los componentes principales 1 y2 
```
levels(data1$Classification) <- c("Control", "Tratamiento")
plot.pca <- fviz_pca_biplot(ds.pca, 
                            # Individuals
                            geom.ind = "point",
                            fill.ind = data1$Stress, col.ind = "black",
                            pointshape = 21, pointsize = 2,
                            palette = c("red","blue"),
                            # Variables
                            alpha.var=1, col.var = "black",
                            gradient.cols = "black",
                            legend.title = list(fill = "Stress"))

plot.pca
```

Análicemos cómo varía PC1 entre ambos tratamientos
```
## Vamos a crear una matriz con los datos de coordendas 
pca.coord <- as.matrix(ds.pca$ind$coord)

### Agregar la variable PC1 al dataset "data"
data1$PC1 <- pca.coord[,1]   

## Prubea de normalidad
shapiro.test(data1$PC1)  
ggqqplot(data1$PC1)

## Prueba de homocedasticidad
leveneTest(data1$PC1 ~ data1$Classification)  

## ANOVA usando el PC1
test3 <- aov(PC1 ~ Classification, data=data1)
anova(test3)    
kruskal.test(PC1 ~ Classification, data=data1)

# Gráfico
ggboxplot(data1, x="Classification", y="PC1", col="black", ylab="PC1", xlab="Condición", add="jitter")
```
