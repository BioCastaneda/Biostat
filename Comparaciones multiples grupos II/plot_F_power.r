# Cargar librería para visualización
library(ggplot2)

# ==============================================================================
# FUNCIÓN MAESTRA PARA GRAFICAR DISTRIBUCIONES F SUPERPUESTAS
# ==============================================================================
plot_F_power <- function(df1, df2, ncp, test_name, max_x = 20, Fobs=NULL) {
  
  max_x <- max(c(max_x,Fobs))
  # 1. Crear secuencia empezando un poco después de 0 para evitar el valor 'Infinito'
  f_vals <- seq(0.1, max_x, length.out = 1000)
  
  densidad_H0 <- df(x = f_vals, df1 = df1, df2 = df2)
  densidad_HA <- df(x = f_vals, df1 = df1, df2 = df2, ncp = ncp)
  f_critico <- qf(p = 0.95, df1 = df1, df2 = df2)
  
  # 2. Estructurar los datos
  datos_plot <- data.frame(
    F_value = rep(f_vals, 2),
    Density = c(densidad_H0, densidad_HA),
    Hypothesis = factor(rep(c("H0 (Efecto Nulo)", "HA (Efecto Real)"), each = length(f_vals)))
  )
  
  datos_plot <- datos_plot[!datos_plot$Density %in% Inf,]
    
  # 3. Límite del Eje Y inteligente: 
  # Buscamos el pico máximo de la campana alternativa, ignorando la asíntota cercana a 0
  pico_HA <- max(densidad_HA[f_vals > 1]) 
  limite_y_visual <- pico_HA * 1.5
  
  # 4. Generar el gráfico
  p <- ggplot(datos_plot, aes(x = F_value, y = Density, fill = Hypothesis, color = Hypothesis)) +
    geom_area(alpha = 0.5, position = "identity") +
    
    # Línea del F crítico
    geom_vline(xintercept = f_critico, linetype = "dashed", color = "#C0392B", linewidth = 1) +
    
    # Etiqueta de F Crítico anclada de forma segura
    annotate("text", x = f_critico + 0.5, y = Inf, vjust = 2, 
             label = paste("F Crítico =", round(f_critico, 2)), 
             color = "#C0392B", hjust = 0, fontface = "bold") +

    # Línea del F observado
    geom_vline(xintercept = Fobs, linetype = "dashed", color = "blue", size = 1) +
    annotate("text", x = Fobs + 0.5, y = Inf, vjust = 2,
            label = paste("F observado =", round(Fobs, 2)), color = "blue", hjust = 1, fontface = "bold") +         
    
    # Cortar el eje Y de forma sensata (evita que la curva nula aplaste todo el gráfico)
    coord_cartesian(ylim = c(0, limite_y_visual)) +
    
    scale_fill_manual(values = c("#005088", "#11caa0")) +
    scale_color_manual(values = c("#005088", "#11caa0")) +
    theme_minimal(base_size = 14) +
    labs(title = paste("Distribuciones F: H0 vs HA -", test_name),
         subtitle = paste("gl1 =", df1, ", gl2 =", df2, "| NCP =", ncp),
         x = "Valor del Estadístico F",
         y = "Densidad de Probabilidad") +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold"))
  
  return(p)
}