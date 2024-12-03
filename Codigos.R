# Cargar librerías y datos
library(pacman)
pacman::p_load(sf, spdep, sp, R2WinBUGS, ggplot2, patchwork, Matrix, INLA)

# Cargar datos
load("./material/Aragon.Rdata")
aragon.sf <- st_read("./material/aragon.shp")

# Preprocesar datos
aragon.sf <- aragon.sf[order(aragon.sf$CODMUNI), ] # Ordenar por código municipal
aragon.sf <- cbind(aragon.sf, Aragon.df[, 2:3])   # Fusionar datos
rm(Aragon.df) # Eliminar objeto original
aragon.spdf <- as(aragon.sf, "Spatial") # Convertir a SpatialPolygonsDataFrame
vecinos <- nb2WB(poly2nb(aragon.spdf))  # Calcular vecinos para análisis espacial

# Crear matriz de adyacencia dispersa
row_indices <- rep(1:length(vecinos$num), times = vecinos$num)  # Filas
col_indices <- vecinos$adj  # Columnas
weights <- vecinos$weights  # Pesos
adj.matrix <- sparseMatrix(i = row_indices, j = col_indices, x = weights)

# Verificar matriz de adyacencia
print(adj.matrix)

# --- Método 1: Análisis con WinBUGS ---
# Cargar resultados precomputados
load("./material/resmod1")

# Añadir resultados al sf para visualización
aragon.sf$RME <- res.mod1$mean$R
aragon.sf$p.RME <- res.mod1$mean$p.R

# Graficar resultados de WinBUGS
rme.plot <- ggplot(aragon.sf) +
  geom_sf(aes(fill = RME)) +
  scale_fill_viridis_c(option = "plasma") +
  ggtitle("WinBUGS: RME") +
  theme_classic()

p.rme.plot <- ggplot(aragon.sf) +
  geom_sf(aes(fill = p.RME)) +
  scale_fill_viridis_c(option = "viridis") +
  ggtitle("WinBUGS: P(RME > 1)") +
  labs(fill = "P(RME > 1)") +
  theme_classic()

# --- Método 2: Análisis con INLA ---
# Preparar datos para INLA
aragon.df <- as.data.frame(aragon.spdf)
aragon.df$O <- aragon.spdf$O  # Observados
aragon.df$E <- aragon.spdf$E  # Esperados
aragon.df$sp <- 1:nrow(aragon.df)  # Índice para modelo BYM

# Definir modelo INLA
formula <- O ~ f(sp, model = "bym", graph = adj.matrix) + offset(log(E))

# Ajustar modelo
res.inla <- inla(
  formula,
  family = "poisson",
  data = aragon.df,
  control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE, waic = TRUE)
)




# Extraer efectos espaciales (estructurados) del modelo BYM
sp_effects <- res.inla$summary.random$sp

# Verificar dimensiones y estructura
str(sp_effects)

# Filtrar valores relevantes para las regiones (estructurados)
# Los primeros 729 valores corresponden a los efectos estructurados
structured_effects <- sp_effects$mean[1:nrow(aragon.sf)]

# Asignar valores al objeto aragon.sf
aragon.sf$RME_inla <- structured_effects



# Extraer resultados de INLA
aragon.sf$p.RME_inla <- res.inla$summary.fitted.values$mean

# Graficar resultados de INLA
rme_inla.plot <- ggplot(aragon.sf) +
  geom_sf(aes(fill = RME_inla)) +
  scale_fill_viridis_c(option = "plasma") +
  ggtitle("INLA: RME") +
  theme_classic()

p.rme_inla.plot <- ggplot(aragon.sf) +
  geom_sf(aes(fill = p.RME_inla)) +
  scale_fill_viridis_c(option = "viridis") +
  ggtitle("INLA: P(RME > 1)") +
  labs(fill = "P(RME > 1)") +
  theme_classic()

# --- Comparar métodos ---
comparison_plots <- wrap_plots(
  list(rme.plot, p.rme.plot, rme_inla.plot, p.rme_inla.plot),
  ncol = 2
)
print(comparison_plots)

# --- Evaluar replicabilidad ---
# Comparar métricas DIC y WAIC
metrics <- data.frame(
  Method = c("WinBUGS", "INLA"),
  DIC = c(res.mod1$DIC, res.inla$dic$dic),
  WAIC = c(NA, res.inla$waic$waic)  # WAIC no está disponible para WinBUGS
)
print(metrics)

# Guardar gráficos y métricas para replicabilidad
ggsave("comparison_plots.png", plot = comparison_plots, width = 12, height = 8)
write.csv(metrics, "metrics_comparison.csv", row.names = FALSE)




