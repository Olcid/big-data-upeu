# EDA básico - Tráfico urbano simulado
# 6405 observaciones, 8 variables
library(tidyverse)
source("datos.R")   # regenera 'datos' para que el script sea reproducible

# 1. Estructura y resumen
glimpse(datos)
summary(datos)

# 2. Valores faltantes por columna
colSums(is.na(datos))

# 3. Estadísticos principales
datos %>%
  summarise(
    vehiculos_prom = mean(vehiculos),
    vehiculos_sd   = sd(vehiculos),
    vehiculos_min  = min(vehiculos),
    vehiculos_max  = max(vehiculos),
    velocidad_prom = mean(velocidad_kmh),
    velocidad_min  = min(velocidad_kmh),
    velocidad_max  = max(velocidad_kmh)
  )

# 4. Frecuencia de variables categóricas
table(datos$punto)
table(datos$congestion)

# 5. Tráfico por punto de la ciudad
datos %>%
  group_by(punto) %>%
  summarise(
    vehiculos_prom = mean(vehiculos),
    velocidad_prom = mean(velocidad_kmh)
  )

# 6. Tráfico por hora del día
datos %>%
  group_by(hora_entera = floor(hora)) %>%
  summarise(vehiculos_prom = mean(vehiculos)) %>%
  print(n = 24)

# 7. Relación entre volumen y velocidad
cor(datos$vehiculos, datos$velocidad_kmh)

# 8. Distribución de vehículos
ggplot(datos, aes(vehiculos)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(title = "Distribución de vehículos", x = "Vehículos", y = "Frecuencia") +
  theme_minimal()