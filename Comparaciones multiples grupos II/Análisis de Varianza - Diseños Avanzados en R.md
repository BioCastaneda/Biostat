# Tutorial Práctico: Análisis de Varianza - Diseños Avanzados en R

En la clase teórica de la "Parte 2" del Análisis de Varianza, discutimos cómo la investigación biológica moderna rara vez se limita a un solo factor en condiciones perfectamente controladas. Abordamos el Diseño de Bloques Aleatorios, los Diseños Jerárquicos (Anidados) y los Diseños Factoriales.

En esta sesión de laboratorio computacional, llevaremos esa teoría a la práctica. Utilizaremos el lenguaje **R** para analizar datos simulados que replican escenarios biológicos reales. Exploraremos los datos, ajustaremos los modelos y, lo más importante, **calcularemos manualmente la significancia (el valor *p*) usando las funciones de probabilidad de R** para desmitificar lo que hace el software bajo el capó.

---

## Preparación del Entorno

Primero, cargaremos las librerías necesarias para la manipulación de datos y visualización.

```r
# Instalación previa requerida: install.packages(c("dplyr", "ggplot2", "knitr"))
library(dplyr)
library(ggplot2)
library(knitr)
```

A continuación, leeremos el código en R que usaremos más tarde para graficar la distribución de _F_ bajo distintos escenarios. Para mantener las buenas prácticas de programación (principio DRY: *Don't Repeat Yourself*), crearemos primero una función graficadora personalizada con `ggplot2` y luego la aplicaremos a cada uno de nuestros tres diseños.

```r
# Leemos el código de una función para graficar la distribución de F bajo distintas hipótesis
source("plot_F_power.r")
```

---

## 1. Diseño de Bloques Aleatorios

**Escenario:** Estamos evaluando el aumento de peso en ratones sometidos a 3 dietas diferentes (Factor principal). Para reducir el error genético y materno, utilizamos "Camadas" (Litters) como nuestro factor de bloqueo. Tenemos 4 camadas, y de cada camada asignamos aleatoriamente un ratón a cada dieta.

### 1.1 Generación de Datos y Análisis Exploratorio

```r
# Generamos los datos
dietas <- rep(c("Dieta_A", "Dieta_B", "Dieta_C"), times = 4)
camadas <- rep(paste0("Camada_", 1:4), each = 3)

# Simulamos el aumento de peso con un efecto real de la dieta y de la camada
aumento_peso <- c(
  12, 15, 18,  # Camada 1
  10, 14, 16,  # Camada 2
  14, 17, 21,  # Camada 3
  11, 13, 17   # Camada 4
)

datos_bloques <- data.frame(Dieta = dietas, Camada = camadas, Peso = aumento_peso)

# Tabla Resumen
resumen_bloques <- datos_bloques %>%
  group_by(Dieta) %>%
  summarise(Media_Peso = mean(Peso), Desviacion = sd(Peso))

kable(resumen_bloques, caption = "Estadísticas Descriptivas por Dieta")
```

```r
# Visualización
ggplot(datos_bloques, aes(x = Dieta, y = Peso, fill = Dieta)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, size = 3, aes(shape = Camada)) +
  theme_minimal() +
  labs(title = "Aumento de Peso por Dieta (controlando por Camada)",
       y = "Aumento de Peso (g)")
```

### 1.2 Ajuste del Modelo y Cálculo de Probabilidad

Ajustamos el modelo incluyendo la Dieta y el factor de Bloque (Camada).

```r
# Ajuste del ANOVA
modelo_bloques <- aov(Peso ~ Dieta + Camada, data = datos_bloques)
resumen_anova_bloq <- summary(modelo_bloques)
print(resumen_anova_bloq)

# Otra forma de obtener los mismo en R
anova(lm(Peso ~ Dieta + Camada, data = datos_bloques))
```

La salida del software nos da un valor *F* para la `Dieta`. ¿De dónde sale el valor *p* (significancia)? Se calcula evaluando dónde cae nuestro estadístico *F* calculado dentro de la distribución teórica de Fisher, dados los grados de libertad del numerador (Dieta) y del denominador (Residuos).

```r
# Extrayendo valores de la tabla ANOVA
F_calc_dieta <- resumen_anova_bloq[[1]][["F value"]][1] # Estadístico F
gl_dieta <- resumen_anova_bloq[[1]][["Df"]][1]          # k - 1 (3 dietas - 1 = 2)
gl_error <- resumen_anova_bloq[[1]][["Df"]][3]          # (k-1)*(b-1) = 2 * 3 = 6

# Cálculo manual de la probabilidad de obtener un F tan o más extremo (cola derecha)
# Usamos la función pf() (Probability Function of F distribution)
p_valor_manual <- pf(q = F_calc_dieta, df1 = gl_dieta, df2 = gl_error, lower.tail = FALSE)

cat("Estadístico F calculado:", round(F_calc_dieta, 2), "\n")
cat("Valor p calculado con pf():", signif(p_valor_manual, 4), "\n")
```

*Interpretación:* Al extraer la varianza introducida por la genética de las camadas, logramos reducir la Media Cuadrática del Error (el denominador de F), aumentando la potencia de nuestra dócima para encontrar diferencias reales entre las dietas.

### 1.3 Visualización de la significancia y poder

Generemos un gráfico para visualizar las distribuciones de _F_ si cada una de las dos hipótesis (nula y alternativa) fueran ciertas.  Visualizar las distribuciones estadísticas es útil para comprender conceptos complejos como la **Potencia Estadística ($1 - \beta$)** y el **Error Tipo II ($\beta$)**.

Para graficar la hipótesis alternativa ($H_A$), debemos introducir un concepto avanzado: el **Parámetro de No Centralidad (NCP, por sus siglas en inglés)**.

- Bajo $H_0$ (no hay efecto real), el estadístico $F$ sigue una **Distribución $F$ Central** ($NCP = 0$).

- Bajo $H_A$ (existe un efecto real), la curva se desplaza hacia la derecha. Ese desplazamiento está dictado por el tamaño del efecto biológico, el cual modelamos usando una **Distribución $F$ No Central** ($NCP > 0$).

```r
# Ejecutar comando para Diseño de Bloques
grafico_bloques <- plot_F_power(df1 = 2, df2 = 6, ncp = 12, 
                                test_name = "Bloques (Efecto de la Dieta)", 
                                max_x = 25, Fobs=F_calc_dieta)
print(grafico_bloques)
```

---

## 2. Diseño Estratificado (Anidado o Jerárquico)

**Escenario:** Queremos medir la concentración de un metal pesado en dos Lagos (Factor A). Dentro de cada lago, tomamos muestras en 3 Sitios distintos (Factor B, anidado en A). En cada sitio, tomamos 3 réplicas analíticas. Estos datos fueron presentados en clase.

### 2.1 Generación de Datos y Análisis Exploratorio

```r
# Generamos los datos
lagos <- rep(c("Lago_Norte", "Lago_Sur"), each = 3, times=3)
sitios <- rep(paste0("Sitio_", 1:6), times = 3)

# Simulamos concentraciones. El Lago Sur tiene mayor concentración general.
concentracion <- scan(text="
18 19 18  21 19 19
16 20 18  20 20 23
16 19 20  18 21 21
"   
)

datos_anidados <- data.frame(Lago = lagos, Sitio = sitios, Conc = concentracion)

# Visualización
ggplot(datos_anidados, aes(x = Sitio, y = Conc, fill = Lago)) +
  geom_boxplot() +
  theme_bw() +
  labs(title = "Concentración de Metal por Sitio anidado en Lago",
       y = "Concentración (ppm)")
```

### 2.2 Ajuste del Modelo y Cálculo de Probabilidad (¡Atención al Denominador!)

Como discutimos en clase, en un diseño anidado, el efecto principal (Lago) **no** se prueba contra el error residual de las réplicas, sino contra la variabilidad de los subgrupos (Sitios dentro de Lagos).

```r
# Ajustamos el modelo indicando que Sitio está anidado dentro de Lago (%in%)
modelo_anidado <- aov(Conc ~ Lago + Sitio %in% Lago, data = datos_anidados)
resumen_anid <- summary(modelo_anidado)
print(resumen_anid)
```

Compare la tabla ANOVA con lo mostrado en clase. ¿Con iguales? ¿Hay algún problema con este análisis?

Usemos el ahora el operador `/` para especificar la jerarquía de efectos en el modelo, como se muestra en esta referencia [5.2.3: Nested Model in R - Statistics LibreTexts](https://stats.libretexts.org/Bookshelves/Advanced_Statistics/Analysis_of_Variance_and_Design_of_Experiments/05%3A_Multi-Factor_ANOVA/5.02%3A_Nested_Treatment_Design/5.2.03%3A_Nested_Model_in_R).

```r
modelo_anidado_2 <- aov(Conc ~ Lago + Lago/Sitio, data = datos_anidados)
resumen_anid_2 <- summary(modelo_anidado_2)
print(resumen_anid_2)
```

¿Se parece más este resultado a lo mostrado en clase?

Intentemos una vez más, pero ahora especificando que Sito no es un efecto fijo, sino un aleatorio y que será el error para la prueba de *F* usando la función `Error()`

```r
# Sintaxis correcta para ANOVA jerárquico según Schefler
modelo_anidado_3 <- aov(Conc ~ Lago + Error(Sitio), data = datos_anidados)
resumen_anid_3 <- summary(modelo_anidado_3)
print(resumen_anid_3)
```

Note que solo cuando especificamos que Sitio es el error residual, obtuvimos el mismo (o similar) resultado a lo descrito por Schefler (1981). Esto es porque por defecto, R asume que todos los factores en un modelo lineal tienen efectos fijos (excepto los efectos residuales) y por lo tanto, siempre evalúa la significancia contra el cuadrado medio de los residuos.

Intentemos ahora usar un método distinto para generar un análisis de varianza. Esta ajustaremos el modelo con la función `lmer` del paquete `lme4`

```r
library(lme4)
library(lmerTest) # Fundamental para obtener valores p
modelo_anidado_lmer <- lmer(Conc ~ Lago + (1 | Sitio), data = datos_anidados)
anova(modelo_anidado_lmer)
```

Para profundizar más sobre el uso de `lmer` puede revisar [esta nota](Nota_sobre_lmer.md) (opcional).

**Cálculo manual crítico:** Por defecto, la función `summary` de R calcula el *F* de los Lagos dividiendo su Media Cuadrática (MC) por la MC del *Residuo*. **¡Esto es incorrecto teóricamente para inferir sobre los lagos!** Debemos dividir la MC de los Lagos por la MC de los Sitios(Lagos).

```r
# Extrayendo Medias Cuadráticas (Mean Squares)
MC_Lago <- resumen_anid[[1]][["Mean Sq"]][1]
MC_Sitio_en_Lago <- resumen_anid[[1]][["Mean Sq"]][2]

# Calculamos el verdadero F para el Factor Principal (Lago)
F_verdadero_lago <- MC_Lago / MC_Sitio_en_Lago

# Extraemos grados de libertad
gl_lago <- resumen_anid[[1]][["Df"]][1]       # a - 1 = 2 - 1 = 1
gl_sitios <- resumen_anid[[1]][["Df"]][2]     # a(b - 1) = 2(3 - 1) = 4

# Calculamos el valor p correcto con pf()
p_valor_lago <- pf(q = F_verdadero_lago, df1 = gl_lago, df2 = gl_sitios, lower.tail = FALSE)

cat("MC Lago:", round(MC_Lago, 2), "| MC Sitio(Lago):", round(MC_Sitio_en_Lago, 2), "\n")
cat("F empírico correcto para Lago:", round(F_verdadero_lago, 2), "\n")
cat("Valor p verdadero calculado con pf():", signif(p_valor_lago, 4), "\n")
```

### 2.3 Visualización de la significancia y poder

Recordemos que el factor principal (Lago) se evaluaba contra la variabilidad de los Sitios anidados. Por tanto, los grados de libertad eran gl_1 = 1 (2 lagos - 1) y el denominador correcto era gl_2 = 4 (los sitios). Al ser tan pocos grados de libertad, la curva F central está muy sesgada. Asumiremos un tamaño del efecto biológico de la contaminación equivalente al estimado (NCP = 4.985). Ver nota abajo para saber cómo estimamos el NCP.

```r
# Ejecutar comando para Diseño Anidado
grafico_anidado <- plot_F_power(df1 = 1, df2 = 4, ncp = 4.985, 
                                test_name = "Jerárquico (Efecto del Lago)", 
                                max_x = 40, Fobs=F_verdadero_lago) # Extendemos el eje X
print(grafico_anidado)
```

 ---

## 3. Diseño Factorial (ANOVA de Dos Vías con Interacción)

**Escenario:** Evaluamos la reducción de la presión arterial (variable dependiente). Tenemos dos factores cruzados: Tratamiento (Fármaco vs. Placebo) y Sexo (Hombre vs. Mujer). Tenemos $n=5$ pacientes por cada combinación.

Queremos saber si el efecto del Fármaco es independiente del Sexo, o si existe una **interacción**.

### 3.1 Generación de Datos y Análisis Exploratorio

```r
tratamiento <- rep(c("Placebo", "Fármaco"), each = 10)
sexo <- rep(rep(c("Hombre", "Mujer"), each = 5), times = 2)

# Simulamos datos donde hay una fuerte interacción: 
# El fármaco funciona muy bien en mujeres, pero poco en hombres.
red_placebo_h <- rnorm(5, 5, 2)
red_placebo_m <- rnorm(5, 6, 2)
red_farmaco_h <- rnorm(5, 8, 2)  # Efecto leve
red_farmaco_m <- rnorm(5, 20, 2) # Efecto masivo

reduccion <- c(red_placebo_h, red_placebo_m, red_farmaco_h, red_farmaco_m)
datos_fact <- data.frame(Tratamiento = tratamiento, Sexo = sexo, Reduccion = reduccion)

# Tabla de Medias Cruzadas
tabla_medias <- datos_fact %>%
  group_by(Tratamiento, Sexo) %>%
  summarise(Media_Reduccion = round(mean(Reduccion), 1), .groups = 'drop')

kable(tabla_medias, caption = "Media de Reducción de Presión Arterial (mmHg)")
```

```r
# Gráfico de Interacción (Crucial para diseños factoriales)
ggplot(tabla_medias, aes(x = Tratamiento, y = Media_Reduccion, group = Sexo, color = Sexo)) +
  geom_line(size = 1.5) +
  geom_point(size = 4) +
  theme_classic() +
  labs(title = "Gráfico de Interacción: Tratamiento x Sexo",
       y = "Reducción de Presión (Media)")
```

*Si las líneas no son paralelas en el gráfico, es el primer indicio visual de que existe una interacción.*

### 3.2 Ajuste del Modelo y Cálculo de Probabilidad

Ajustamos el modelo incluyendo ambos factores principales y su término de interacción (`Tratamiento * Sexo`).

```r
# Ajuste de ANOVA Factorial (El asterisco incluye efectos principales e interacción)
modelo_factorial <- aov(Reduccion ~ Tratamiento * Sexo, data = datos_fact)
res_fact <- summary(modelo_factorial)
print(res_fact)
```

**Cálculo de la significancia de la Interacción:**

En la tabla de salida de R, buscamos la fila correspondiente a `Tratamiento:Sexo`.

```r
# Extracción de valores para la Interacción
F_interaccion <- res_fact[[1]][["F value"]][3]
gl_interaccion <- res_fact[[1]][["Df"]][3]      # (a-1)*(b-1) = 1 * 1 = 1
gl_residuos_fact <- res_fact[[1]][["Df"]][4]    # N - ab = 20 - 4 = 16

# Cálculo manual de la probabilidad 
p_valor_interaccion <- pf(q = F_interaccion, df1 = gl_interaccion, df2 = gl_residuos_fact, lower.tail = FALSE)

cat("Estadístico F para Interacción Tratamiento:Sexo:", round(F_interaccion, 2), "\n")
cat("Valor p calculado mediante pf():", signif(p_valor_interaccion, 4), "\n")
```

Si el valor *p* de la interacción es significativo (usualmente $< 0.05$), como profesores y estadísticos concluimos que: **El efecto del Fármaco depende del Sexo del paciente**. Los efectos principales individuales pasan a un segundo plano, pues la realidad del fenómeno biológico recae en la sinergia de los factores.

### 3.3 Visualización de la significancia y poder

Para la interacción de nuestro modelo médico 2x2, calculamos $gl_1 = 1$ y un error residual total de $gl_2 = 16$. Dado que simulamos una interacción masiva (el fármaco funcionaba excelente en mujeres pero no en hombres), usaremos un $NCP$ muy alto ($NCP = 25$).

```r
# Ejecutar comando para Diseño Factorial
grafico_factorial <- plot_F_power(df1 = 1, df2 = 16, ncp = 25, 
                                  test_name = "Factorial (Interacción Fármaco x Sexo)", 
                                  max_x = 45, Fobs=F_interaccion)
print(grafico_factorial)
```

## 4 Nota sobre el cálculo del Parámetro de no Centralidad

El **Parámetro de No Centralidad (NCP)**, denotado usualmente con la letra griega $\lambda$ (lambda), es el valor que define exactamente "cuánto se desplaza" la distribución $F$ hacia la derecha cuando la Hipótesis Nula ($H_0$) es falsa.

Existen dos formas de entender el NCP:

1. **NCP Empírico (Estimado):** Se calcula con los datos obtenidos en la muestra. Su fórmula directa es $\lambda_{emp} = F_{calculado} \times gl_{efecto}$. También equivale a dividir la Suma de Cuadrados del Efecto entre la Media Cuadrática del Error ($\frac{SC_{efecto}}{MC_{error}}$).

2. **NCP Teórico (Poblacional):** Es el valor esperado *a priori* basado en los parámetros reales de la población ($\mu$ y $\sigma^2$) que nosotros mismos definimos al simular los datos.

A continuación, desarrollaremos el **NCP Teórico** para los  casos simulados y el NCP Empírico para datos experimentales (en este caso, tomados del libro Schefler 1981).

---

### 4.1. Diseño de Bloques Aleatorios (Efecto Dieta)

En este caso, no usamos la función `rnorm()` para generar poblaciones aleatorias, sino que **introdujimos los datos manualmente** mediante un vector exacto. Por lo tanto, calcularemos el parámetro basado en los valores fijos que introdujimos.

**Datos introducidos:**

- **Dieta A:** 12, 10, 14, 11 $\rightarrow \bar{X}_A = 11.75$

- **Dieta B:** 15, 14, 17, 13 $\rightarrow \bar{X}_B = 14.75$

- **Dieta C:** 18, 16, 21, 17 $\rightarrow \bar{X}_C = 18.00$

- **Media Global ($\bar{X}_{...}$):** $14.833$

- **$n$ (sujetos por dieta):** 4

La fórmula del NCP para un factor de efectos fijos es:

$$\lambda = \frac{n \sum (\mu_i - \mu_{global})^2}{\sigma^2_{error}}$$

**Paso 1: Variabilidad del Efecto (Numerador)**

Calculamos la suma de las diferencias al cuadrado de las dietas respecto a la media global:

$\sum (\mu_i - \mu_{global})^2 = (11.75 - 14.833)^2 + (14.75 - 14.833)^2 + (18.00 - 14.833)^2$

$\sum = (-3.083)^2 + (-0.083)^2 + (3.167)^2 = 9.505 + 0.007 + 10.030 = 19.542$

Multiplicamos por $n=4$:

Numerador $= 4 \times 19.542 = 78.168$ *(Esto equivale exactamente a la Suma de Cuadrados de la Dieta).*

**Paso 2: Varianza del Error (Denominador)**

Dado que introdujimos los datos fijos, la varianza residual empírica (después de extraer el efecto del bloque de las camadas) en este set específico de datos resulta ser $MC_{error} \approx 1.889$.

**Cálculo Final del NCP:**

$\lambda = \frac{78.168}{1.889} \approx 41.38$

*(Nota pedagógica: En el script de la gráfica anterior usé `ncp = 12` solo para que las curvas se superpusieran de manera estética en el gráfico y fueran fáciles de visualizar. Sin embargo, los datos exactos que tipeamos escondían un efecto masivo real de $\lambda \approx 41.38$).*

---

### 4.2. Diseño Estratificado (Efecto del Lago)

El parámetro de no centralidad estimado de nuestra muestra se calcula exactamente igual que el estadístico $F_{calculado}$ multiplicado por los grados de libertad del numerador ($gl_{Lagos} = 2 - 1 = 1$). Dado que los grados de libertad del lago son 1, el NCP equivale numéricamente a $F$.

$\lambda = \frac{SC_{Lagos}}{MC_{Sitios(Lagos)}}$

$\lambda = \frac{18.00}{3.611} = \mathbf{4.985}$

**Conclusión:** En este experimento clásico extraído del manual, obtenemos un $\lambda_{empírico} \approx 4.985$. Si evaluáramos el estadístico $F$ ($4.985$) contra la tabla crítica de F para $\alpha = 0.05$ con $gl(1, 4)$, encontraríamos que el $F_{crítico}$ es 7.71.Como nuestro $\lambda$ (y F calculado) de 4.985 es menor que 7.71, fracasamos en rechazar la Hipótesis Nula. No podemos concluir estadísticamente que exista una diferencia real entre el Lago Norte y el Lago Sur. El NCP de 4.985 nos indica que el desplazamiento de la curva alternativa hacia la derecha es pequeño, lo que significa que este experimento tenía una baja Potencia Estadística. Esto se debe principalmente a que tenemos muy pocos sitios anidados ($gl=4$), lo que castiga severamente nuestro denominador, ilustrando por qué diseñar correctamente las réplicas en los distintos estratos de un diseño anidado es vital en la biología.

---

### 4.3. Diseño Factorial (Interacción Fármaco x Sexo)

Este es el cálculo matemáticamente más riguroso, porque el NCP no evalúa las medias de los grupos principales, sino el término de **Interacción ($\alpha\beta_{ij}$)**.

Recordemos las medias poblacionales $\mu$ que usamos en los `rnorm()`:

- Placebo Hombre (PH) = 5

- Placebo Mujer (PM) = 6

- Fármaco Hombre (FH) = 8

- Fármaco Mujer (FM) = 20

**Paso 1: Cálculo de los efectos de interacción ($\alpha\beta_{ij}$)**

El efecto de interacción de una celda se calcula aislándolo de los efectos principales:

$\alpha\beta_{ij} = \mu_{celda} - \mu_{tratamiento} - \mu_{sexo} + \mu_{global}$

Primero obtenemos los promedios marginales:

- Media Placebo = $5.5$

- Media Fármaco = $14.0$

- Media Hombre = $6.5$

- Media Mujer = $13.0$

- **Media Global ($\mu_{..}$)** = $9.75$

Ahora, calculamos las interacciones puras de cada celda:

- $\alpha\beta_{PH} = 5 - 5.5 - 6.5 + 9.75 = \mathbf{2.75}$

- $\alpha\beta_{PM} = 6 - 5.5 - 13.0 + 9.75 = \mathbf{-2.75}$

- $\alpha\beta_{FH} = 8 - 14.0 - 6.5 + 9.75 = \mathbf{-2.75}$

- $\alpha\beta_{FM} = 20 - 14.0 - 13.0 + 9.75 = \mathbf{2.75}$

**Paso 2: Cálculo del NCP de la Interacción**

Todos los grupos se simularon con $n=5$ y una $\sigma_{poblacional} = 2$ (por tanto, $\sigma^2_{error} = 4$).

$\lambda = \frac{n \sum (\alpha\beta_{ij})^2}{\sigma^2_{error}}$

$\lambda = \frac{5 \times [ (2.75)^2 + (-2.75)^2 + (-2.75)^2 + (2.75)^2 ]}{4}$

$\lambda = \frac{5 \times [ 7.5625 \times 4 ]}{4}$

$\lambda = \frac{5 \times 30.25}{4} = \frac{151.25}{4} = \mathbf{37.81}$

### 4.4 Resumen Pedagógico

Como ven, el valor exacto de la "falsedad" de la Hipótesis Nula ($\lambda$) no es mágico; es una consecuencia matemática directa de:

1. El **tamaño biológico del efecto** (diferencias de medias poblacionales o magnitud de la interacción).

2. El **tamaño de la muestra ($n$)** (a mayor $n$, mayor $\lambda$).

3. La **varianza del error ($\sigma^2$)** (a menor ruido, mayor $\lambda$).

Es exactamente este $\lambda$ el que nos exigen programas como [_G\*Power_](https://www.psychologie.hhu.de/arbeitsgruppen/allgemeine-psychologie-und-arbeitspsychologie/gpower) o [R](https://www.r-project.org) cuando intentamos calcular prospectivamente cuántos animales de laboratorio o pacientes necesitaremos para alcanzar un $80\%$ de Potencia Estadística en un proyecto o experimento.
