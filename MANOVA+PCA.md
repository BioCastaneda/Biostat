# Práctico 6: Análisis multivariado (parte I)

En este realizaremos diversos análisis multivariados en R. Primero realizaremos un análisis multivariado de varianza (MANOVA) y luego un análisis de componentes principales (PCA):

---

## Contenido

1. [Análisis de varianza múltivariado](https://github.com/lecastaneda/Bioestadistica/edit/main/Pr%C3%A1ctico5.md#1-an%C3%A1lisis-de-varianza-multivariado-manova)
2. [Análisis de componentes principales](https://github.com/lecastaneda/Bioestadistica/blob/main/Pr%C3%A1ctico5.md#2-an%C3%A1lisis-de-componentes-principales)

---
## 1. Análisis de varianza multivariado (MANOVA)

Descargar los datos contenidos en el archivo Excel [dataR2](https://github.com/lecastaneda/Bioestadistica/blob/main/dataR2.xlsx)

Este set de datos consta de 116 observaciones, de las cuales 64 pacientes tienen cáncer de mama y 52 forman parte del grupo de control. El conjunto de datos consta de 10 variables: Edad (años), IMC (kg/m²), Glucosa (mg/dL), Insulina (µU/mL), HOMA, Leptina (ng/mL), Adiponectina (µg/mL), Resistina (ng/mL), MCP-1 (pg/dL), y Clasificación (1 = controles sanos, 2 = pacientes (con cáncer))

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
cor.mat <- data1 %>% cor_mat()
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
leveneTest(data1[,1]) ~data1[,10]))
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
mahalanobis_distance(data = data1[, c(,1:9)])$is.outlier
```

Ahora realizaremos el MANOVA
```
m0 <- manova(cbind(data1[,1:9) ~ data1[,9])
anova(m0, test="Wilks")
#
## Cálcular el tamaño del efecto
library(effectsize)
eta_squared(m0)
#
## Revisar los resultados del ANOVA para cada variable
summary.aov(m0)
# ó
m1 <- aov(data1$pH ~data1$site); anova(m1)
m2 <- aov(data1$C ~data1$site); anova(m2)
m3 <- aov(data1$N ~data1$site); anova(m3)
m4 <- aov(data1$P ~data1$site); anova(m4)
m5 <- aov(data1$K ~data1$site); anova(m5)
#
## Realizar las pruebas a posteriori
post.m1 <- TukeyHSD(m1); plot(post.m1)
post.m2 <- TukeyHSD(m1); plot(post.m2)
post.m3 <- TukeyHSD(m1); plot(post.m3)
post.m5 <- TukeyHSD(m1); plot(post.m5)
```

Gráfiquemos una de las variables: pH
```
plot1 <- ggboxplot(data1, x="site", y="pH", col="black", ylab="pH", xlab="Sitios", add="jitter")
plot1
#

## Establecer posiciones de las líneas
## Pero primero ordenaremos los sitios de mayor a menor para
## favorecer la estética del gráfico
data1$site <- factor(data1$site, levels=c("Neltume","Huillilemu","Catanli","LasPalmas","Pelchuquin"))
#
## Volvemos a gráficas
plot1 <- ggboxplot(data1, x="site", y="pH", col="black", ylab="pH", xlab="Sitios", add="jitter")
plot1
#
## Realizamos la prueba a posteriori
test1 <- data1 %>% t_test(pH~site) %>% add_significance() %>% adjust_pvalue(method="fdr")
test1
## Agragamos las posiciones en el y donde queremos que vayan las líneas de significancia
test1a <- test1 %>% add_xy_position(x="site",dodge=0.8) %>% 
  mutate(y.position=c(7.1,6.95,6.8,6.65,6.5,6.35,6.2,6.05,5.9,5.6))
test1a
#
## Gráfico final
plot1 + stat_pvalue_manual(test1a,label="p.adj.signif",tip.length = 0.01)
# 
## Este gráfico tiene todas las significancias entre pares de sitios,
## pero se ve muy saturado, así que dejaremos solo las comparaciones significativas
## Para esto, reemplazamos las líneas que no queremos cono NAs
test1b <- test1 %>% add_xy_position(x="site",dodge=0.8) %>% 
  mutate(y.position=c(NA,NA,6.8,6.65,NA,NA,NA,NA,NA,NA))
test1b
#
## Gráfico final v2
plot1+stat_pvalue_manual(test1b,label="p.adj.signif",tip.length = 0.01)

```

---
## 2. Análisis de componentes principales

Descargar los datos contenidos en el archivo de texto [Phylum](https://github.com/BioCastaneda/Inverskin/blob/main/archivos/DSdata_phylum.xlsx)

Este set de datos contiene las abundancias (relativas y absolutas) de bacterias asociadas al intestito de Drosophila subobscura a nivel taxonómico de phylum.

Analizamos las correlaciones entre variables
```
library(ggpubr)
library(car)
library(readxl)
library(factoextra)
library(FactoMineR)

data1 <- read_xlsx("DSdata_phylum.xlsx")
head(data1)
dim(data1)
str(data1)
#
## Seleccionamos las columnas con las abundancias absolutas
data2 <- data1[,c(9:13)]
cor.mat <- data2 %>% cor_mat()
cor.mat %>% pull_lower_triangle() %>% cor_plot()
```

Realicemos el PCA
```
ds.pca <- PCA(data2, graph=F)

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
# Vamos a crear una matriz con los datos de coordendas 
pca.coord <- as.matrix(ds.pca$ind$coord)


data1$PC1 <- pca.coord[,1]   # Agregar la variable PC1 al dataset "data"

shapiro.test(data1$PC1)  # Prubea de normalidad
ggqqplot(data1$PC1)
leveneTest(data1$PC1 ~ data1$site)  # Prueba de homocedasticidad

## ANOVA usando el PC1
test3 <- aov(PC1 ~ site, data=data1)
anova(test3)    
kruskal.test(PC1 ~ Stress, data=data1)

# Gráfico
ggboxplot(data1, x="Stress", y="PC1", col="black", ylab="PC1", xlab="Sitios", add="jitter")

```
