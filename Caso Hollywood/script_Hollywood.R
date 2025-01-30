#Primero importar de la hoja de excel Exhibit 1 y llamarla data_hollywood

names(data_hollywood)

# Convertir unas variables de texto a categóricas

data_hollywood$MPAA <- as.factor(data_hollywood$MPAA)
data_hollywood$Genre <- as.factor(data_hollywood$Genre)


summary(data_hollywood)

# Cargamos la biblioteca tidyverse
library(tidyverse)

# Seleccionamos las variables de interés y las convertimos en filas
data_long <- data_hollywood %>%
  select(`Opening Gross`, `Total U.S. Gross`, `Total Non-U.S. Gross`, `Opening Theatres`) %>%
  gather(variable, value)

# Calculamos los valores mínimo, medio y máximo
summary_table <- data_long %>%
  group_by(variable) %>%
  summarise(
    min_value = min(value, na.rm = TRUE),
    mean_value = mean(value, na.rm = TRUE),
    max_value = max(value, na.rm = TRUE)
  )

# Imprimimos la tabla resumen
print(summary_table)

# Cargar paquete necesario
library(gt)

# Crear tabla con gt
gt_table <- summary_table %>%
  gt() %>%
  tab_header(title = "Estadísticos Descriptivos") %>%
  cols_label(
    variable = "Variable",
    min_value = "Mínimo",
    max_value = "Máximo",
    mean_value = "Media"
  )

# Mostrar tabla
gt_table

# Exportar la table
gtsave(gt_table, file = "mi_tabla.html")

# Contamos cuántas películas son comedias
comedy_movies <- data_hollywood %>%
  filter(Genre == "Comedy") %>%
  nrow()

# Contamos cuántas películas son de categoría R
r_movies <- data_hollywood %>%
  filter(MPAA == "R") %>%
  nrow()

print(paste("Número de películas que son comedias: ", comedy_movies))
print(paste("Número de películas que son de categoría R: ", r_movies))

# View the frequency table
freq_table <- table(data_hollywood$MPAA)

## Punto2 

# Calculamos el ROI
data_hollywood <- data_hollywood %>%
  mutate(ROI_US = (`Total U.S. Gross` - Budget) / Budget)

# Estimar un intervalo de confianza del 95% para la media del ROI 

ic_ROI  <- data_hollywood %>%
  summarise(lower = t.test(ROI_US)$conf.int[1], upper = t.test(ROI_US)$conf.int[2])
print(ic_ROI)

# A modo de Ejemplo:
# Calculamos el intervalo de confianza del 90%
ic_ROI_90 <- data_hollywood %>%
  summarise(lower = t.test(ROI_US, conf.level = 0.90)$conf.int[1], 
            upper = t.test(ROI_US, conf.level = 0.90)$conf.int[2])
print(ic_ROI_90)

# Calculamos el intervalo de confianza del 99%
ic_ROI_99 <- data_hollywood %>%
  summarise(lower = t.test(ROI_US, conf.level = 0.99)$conf.int[1], 
            upper = t.test(ROI_US, conf.level = 0.99)$conf.int[2])
print(ic_ROI_99)


# Prueba para responder a la cita de London

# Realizamos la prueba t de una muestra
result <- t.test(data_hollywood$ROI_US, mu = 0.12)

# Imprimimos el valor p
print(paste("El valor p es ", result$p.value))

library(ggplot2)

ggplot(data = data_hollywood, aes(x = Budget, y = `Total U.S. Gross`,  color="#55C667FF")) +
  geom_point(size = 4) + theme_classic() +  theme(legend.position = "none")