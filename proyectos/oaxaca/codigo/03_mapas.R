#=========================================================================
# 03_mapas.R
# Mapas de las regiones de Oaxaca.
#
# Paso A: cruza los municipios del SIAP con los polígonos oficiales del INEGI
#         y los agrupa en las siete regiones del análisis.
# Paso B: dibuja tres mapas (referencia, divergencia superficie-valor y
#         dependencia de la lluvia por municipio).
#
# Las regiones son los Distritos de Desarrollo Rural (DDR) del SIAP, con el
# nombre de la región a la que corresponden (ver 01_limpieza.R).
#
# Geografía: INEGI, Marco Geoestadístico 2020, en la versión GeoJSON de
# github.com/PhantomInsights/mexico-geojson. El archivo original pesa 13 MB y
# se descarga una vez; lo que usa el sitio son versiones simplificadas y ligeras.
#=========================================================================

library(sf)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(stringi)
library(ggplot2)
library(scales)

source("codigo/00_estilo.R")

url_geo     <- "https://raw.githubusercontent.com/PhantomInsights/mexico-geojson/main/2020/states/Oaxaca.json"
archivo_geo <- "datos/crudos/Oaxaca_municipios_inegi.json"
crs_mx      <- 6372      # proyección oficial de México (LCC, ITRF2008), en metros

dir.create("datos/crudos", recursive = TRUE, showWarnings = FALSE)
dir.create("figuras", showWarnings = FALSE)

# A. GEOGRAFÍA: municipios -> regiones -------------------------------------

if (!file.exists(archivo_geo)) download.file(url_geo, archivo_geo, mode = "wb", quiet = TRUE)

# Normaliza nombres: sin acentos, minúsculas, sin puntuación
clave_nombre <- function(x) {
  x %>% stri_trans_general("Latin-ASCII") %>% str_to_lower() %>%
    str_replace_all("[^a-z0-9 ]", " ") %>% str_squish()
}

geo <- st_read(archivo_geo, quiet = TRUE) %>%
  st_transform(crs_mx) %>%
  st_make_valid() %>%
  transmute(cvegeo = CVEGEO, municipio_inegi = NOM_MUN, clave = clave_nombre(NOM_MUN))

mun_siap <- read_csv("datos/oaxaca_municipio_modalidad.csv", show_col_types = FALSE)

siap <- mun_siap %>%
  distinct(municipio, region) %>%
  mutate(clave = clave_nombre(municipio))

# Cinco municipios se escriben distinto en el SIAP y en el INEGI
arregla_clave <- c(
  "h v tezoatlan de segura y luna c de la i de o" = "heroica villa tezoatlan de segura y luna cuna de la independencia de oaxaca",
  "heroica ciudad de juchitan de zaragoza"        = "juchitan de zaragoza",
  "heroica villa de san blas atempa"              = "san blas atempa",
  "heroico san martin de los cansecos"            = "san martin de los cansecos",
  "villa hidalgo yalalag"                         = "villa hidalgo"
)
siap <- siap %>%
  mutate(clave = coalesce(unname(arregla_clave[clave]), clave))

# Dos nombres existen dos veces en Oaxaca (en distritos distintos). El SIAP los
# separa por región; se verificó con la ubicación de cada polígono.
homonimos <- tribble(
  ~clave,               ~region,            ~cvegeo_h,
  "san juan mixtepec",  "Mixteca",          "20208",   # cerca de Juxtlahuaca
  "san juan mixtepec",  "Valles Centrales", "20209",   # distrito de Miahuatlán
  "san pedro mixtepec", "Costa",            "20318",   # distrito de Juquila
  "san pedro mixtepec", "Valles Centrales", "20319"    # distrito de Miahuatlán
)

geo_unicos <- geo %>% st_drop_geometry() %>% filter(!clave %in% homonimos$clave) %>%
  select(cvegeo, clave)

cruce <- siap %>%
  left_join(geo_unicos, by = "clave") %>%
  left_join(homonimos, by = c("clave", "region")) %>%
  mutate(cvegeo = coalesce(cvegeo, cvegeo_h)) %>%
  select(municipio, region, cvegeo)

stopifnot(!anyNA(cruce$cvegeo), !anyDuplicated(cruce$cvegeo))
write_csv(cruce, "datos/oaxaca_municipios_cvegeo.csv")

geo <- geo %>%
  left_join(cruce %>% select(cvegeo, region), by = "cvegeo") %>%
  mutate(sin_dato_siap = is.na(region))

# Los municipios que el SIAP no reporta (sin superficie sembrada) toman la región
# de la mayoría de sus vecinos, para no dejar huecos en el mapa regional.
vecinos <- st_touches(geo)
for (i in which(geo$sin_dato_siap)) {
  r <- geo$region[vecinos[[i]]]
  r <- r[!is.na(r)]
  if (length(r) > 0) geo$region[i] <- names(sort(table(r), decreasing = TRUE))[1]
}
stopifnot(!anyNA(geo$region))

# El archivo del INEGI deja miles de micro-huecos (menores a un metro cuadrado)
# entre municipios vecinos. Al disolver, se dibujarían como puntos dentro de las
# regiones. Esta función los elimina y conserva los huecos reales (enclaves de
# otra región, de más de 1 km2).
quita_huecos <- function(x, umbral_m2 = 1e6) {
  crs <- st_crs(x)
  st_geometry(x) <- st_sfc(lapply(st_geometry(x), function(g) {
    partes <- if (inherits(g, "MULTIPOLYGON")) unclass(g) else list(unclass(g))
    partes <- lapply(partes, function(anillos) {
      grande <- vapply(anillos[-1], function(r) {
        as.numeric(st_area(st_sfc(st_polygon(list(r)), crs = crs))) >= umbral_m2
      }, logical(1))
      anillos[c(TRUE, grande)]
    })
    st_multipolygon(partes)
  }), crs = crs)
  x
}

# Regiones: se disuelven los polígonos originales, se limpian y se simplifican
regiones <- geo %>%
  group_by(region) %>%
  summarise(n_municipios = n(), .groups = "drop") %>%
  quita_huecos() %>%
  st_simplify(preserveTopology = TRUE, dTolerance = 400)

municipios <- geo %>%
  st_simplify(preserveTopology = TRUE, dTolerance = 250)

guarda_geo <- function(x, archivo) {
  st_write(st_transform(x, 4326), archivo, delete_dsn = TRUE, quiet = TRUE,
           layer_options = "COORDINATE_PRECISION=4")
}
guarda_geo(regiones,   "datos/oaxaca_regiones.geojson")
guarda_geo(municipios, "datos/oaxaca_municipios.geojson")

# B. MAPAS --------------------------------------------------------------------

fuente_mapa <- paste0("Fuente: SIAP, Anuario Estadístico de la Producción Agrícola; INEGI, Marco Geoestadístico 2020. ",
                      "Regiones = Distritos de Desarrollo Rural.")

tema_mapa <- function() {
  tema_agro() +
    theme(
      axis.text = element_blank(), axis.title = element_blank(),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.spacing.x = unit(0.5, "lines"),
      legend.position = "bottom", legend.justification = "center"
    )
}

# Punto de etiqueta dentro de cada región
puntos <- st_point_on_surface(regiones)
etiquetas <- bind_cols(st_drop_geometry(regiones), as_tibble(st_coordinates(puntos)))

# B1. Mapa de referencia ---------------------------------------------------------

etiq_ref <- etiquetas %>%
  mutate(texto = paste0(region, "\n(", n_municipios, " municipios)"))

mapa1 <- ggplot() +
  geom_sf(data = municipios, fill = "#efe9dc", colour = "#d9d2c1", linewidth = 0.12) +
  geom_sf(data = regiones, fill = NA, colour = col$tinta, linewidth = 0.55) +
  geom_label(data = etiq_ref, aes(X, Y, label = texto), size = 3.1, lineheight = 0.95,
             fill = alpha(col$papel, 0.82), colour = col$tinta, label.size = 0,
             label.padding = unit(0.18, "lines"), fontface = "bold") +
  labs(
    title = "Las siete regiones del análisis",
    subtitle = "Contorno grueso: región. Contorno fino: municipio.",
    caption = fuente_mapa
  ) +
  tema_mapa()

guarda(mapa1, "07_mapa_regiones.png", 9, 6.6)

# B2. Divergencia entre superficie y valor ------------------------------------------

region_anio <- read_csv("datos/oaxaca_region_anio.csv", show_col_types = FALSE)

cambio <- region_anio %>%
  filter(region != "Oaxaca (total)", anio %in% c(2004, 2024)) %>%
  group_by(region) %>%
  summarise(
    `Superficie sembrada`    = (sup_sembrada_ha[anio == 2024] / sup_sembrada_ha[anio == 2004] - 1) * 100,
    `Valor de la producción` = (valor_real_miles_pesos_2024[anio == 2024] /
                                  valor_real_miles_pesos_2024[anio == 2004] - 1) * 100,
    .groups = "drop"
  ) %>%
  pivot_longer(-region, names_to = "medida", values_to = "cambio") %>%
  mutate(medida = factor(medida, levels = c("Superficie sembrada", "Valor de la producción")))

mapa_cambio <- regiones %>% inner_join(cambio, by = "region")
etiq_cambio <- etiquetas %>% select(region, X, Y) %>% inner_join(cambio, by = "region")

mapa2 <- ggplot(mapa_cambio) +
  geom_sf(aes(fill = cambio), colour = col$papel, linewidth = 0.6) +
  geom_label(data = etiq_cambio, aes(X, Y, label = pct_signo(cambio)), size = 3,
             fill = alpha(col$papel, 0.85), colour = col$tinta, label.size = 0,
             label.padding = unit(0.15, "lines"), fontface = "bold") +
  facet_wrap(~medida) +
  scale_fill_gradient2(
    low = col$superficie, mid = "#f0efec", high = col$valor, midpoint = 0,
    limits = c(-60, 60), oob = squish, space = "Lab",
    breaks = c(-60, -30, 0, 30, 60), labels = function(x) ifelse(x == 0, "0%", pct_signo(x)),
    guide = guide_colourbar(barwidth = 14, barheight = 0.55, title = NULL)
  ) +
  labs(
    title = "Donde el valor sube mientras la superficie baja: Valles Centrales y Mixteca",
    subtitle = "Cambio entre 2004 y 2024 por región. Valor en pesos de 2024. Misma escala en ambos mapas.",
    caption = paste0(fuente_mapa, " Deflactor: INPC, Banco Mundial.")
  ) +
  tema_mapa()

guarda(mapa2, "08_mapa_divergencia.png", 11, 6.2)

# B3. Dependencia de la lluvia, por municipio (2024) -----------------------------------

temporal_mun <- mun_siap %>%
  filter(anio == 2024) %>%
  left_join(cruce %>% select(municipio, region, cvegeo), by = c("municipio", "region")) %>%
  group_by(cvegeo) %>%
  summarise(
    sup_total    = sum(sup_sembrada_ha, na.rm = TRUE),
    sup_temporal = sum(sup_sembrada_ha[modalidad == "temporal"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(pct_temporal = ifelse(sup_total > 0, sup_temporal / sup_total * 100, NA_real_))

mapa_mun <- municipios %>% left_join(temporal_mun, by = "cvegeo")

mapa3 <- ggplot() +
  geom_sf(data = mapa_mun, aes(fill = pct_temporal), colour = col$papel, linewidth = 0.08) +
  geom_sf(data = regiones, fill = NA, colour = col$tinta, linewidth = 0.4) +
  scale_fill_gradient(
    low = "#f6e7c1", high = "#6b4300", limits = c(0, 100), na.value = "#d9d5cb",
    breaks = c(0, 25, 50, 75, 100), labels = function(x) paste0(x, "%"),
    guide = guide_colourbar(barwidth = 14, barheight = 0.55, title = NULL)
  ) +
  labs(
    title = "En 9 de cada 10 municipios, 75% o más del campo es de temporal",
    subtitle = "Porcentaje de la superficie sembrada bajo temporal, 2024. Gris: sin superficie reportada.",
    caption = fuente_mapa
  ) +
  tema_mapa()

guarda(mapa3, "09_mapa_temporal_municipal.png", 9, 6.6)

message("Listo: 3 mapas en figuras/ y geografía ligera en datos/")
