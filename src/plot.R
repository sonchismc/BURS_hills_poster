# Cargar librerías
library(ggplot2)
library(dplyr)

# Cargar funciones externas
source('src/utils.R')

# Directorio de entrada y salida
input_folder <- "top_hills"
output_folder <- "hills_plot"

# Crear la carpeta de salida si no existe
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# Obtener lista de archivos GPX en el folder de entrada
files <- list.files(input_folder, pattern = "\\.gpx$", full.names = TRUE)

# Iterar sobre cada archivo GPX
for (file in files) {
  # Extraer el nombre del archivo sin extensión
  plot_name <- tools::file_path_sans_ext(basename(file))
  
  # Extraer datos del archivo GPX
  route_df <- extract_gpx3d(file)
  
  # Preparar los datos
  route_df <- route_df %>%
    mutate(distance = as.numeric(distance)) %>% 
    mutate(cumulative_distance = cumsum(distance))
  
  n_lines <- 5
  line_positions <- seq(min(route_df$cumulative_distance), 
                        max(route_df$cumulative_distance), 
                        length.out = n_lines)
  
  # Filtrar los puntos más cercanos a las posiciones calculadas
  stem_points <- route_df %>%
    filter(cumulative_distance %in% sapply(line_positions, function(x) {
      route_df$cumulative_distance[which.min(abs(route_df$cumulative_distance - x))]
    }))
  
  # Crear la gráfica con líneas stem
  p <- ggplot() +
    # Líneas "stem" en posiciones específicas
    geom_segment(data = stem_points, 
                 aes(x = cumulative_distance, xend = cumulative_distance, 
                     y = 0, yend = ele), 
                 color = "darkgray", size = 0.2) +
    
    # Línea principal del perfil de elevación
    geom_line(data = route_df, 
              aes(x = cumulative_distance, y = ele), 
              color = "black", size = 1) +
    
    # Agregar título
    labs(title = paste(plot_name, "Elevation Profile"),
         x = "Cumulative Distance (m)",
         y = "Elevation (m)") +
    
    # Configuración del tema sin título, etiquetas y grid horizontal
    theme_minimal(base_size = 15) +
    theme(
      panel.grid.major.x = element_blank(),  # Quitar grid vertical
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10)
    ) +
    scale_x_continuous(expand = expansion(mult = c(0.01, 0.01))) +
    scale_y_continuous(expand = expansion(mult = c(0.01, 0.05)))
  
  # Guardar la gráfica en la carpeta hills_plot
  output_path <- file.path(output_folder, paste0(plot_name, "_plot.png"))
  ggsave(output_path, plot = p, width = 10, height = 6, dpi = 300)
  
  # Mensaje de confirmación
  cat("Gráfica guardada en:", output_path, "\n")
}
