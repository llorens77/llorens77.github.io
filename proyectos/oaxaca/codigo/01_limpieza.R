#=========================================================================
# 01_limpieza.R
# Anuario Estadístico de la Producción Agrícola (SIAP) — Oaxaca, 2004-2024
#
# Qué hace: lee los Excel descargados de nube.agricultura.gob.mx/cierre_agricola,
# los limpia y escribe cuatro tablas CSV en la carpeta datos/.
# Los CSV son públicos y ligeros: el resto del proyecto (gráficas, mapa,
# página de Quarto) lee solo esos CSV, no los Excel originales.
#
# Cómo correrlo: abre RStudio en la carpeta del proyecto y ejecuta el script.
#=========================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# 1. PARÁMETROS ----------------------------------------------------------

# Carpeta con "Agricultura Temporal", "Agricultura Riego" y "Cultivos.xlsx"
dir_crudos <- "~/Desktop/NORIA/Oaxaca"

# Deflactor: índice de precios al consumidor de México (2010 = 100),
# promedio anual. Fuente: Banco Mundial, indicador FP.CPI.TOTL.
# Sirve para expresar el valor de la producción en pesos de 2024 y que
# la comparación entre años no mezcle producción con inflación.
deflactor <- tibble::tribble(
  ~anio, ~inpc,
  2004,  77.4154528387153,
  2009,  96.0091610619154,
  2014, 116.247985177591,
  2019, 141.542522969974,
  2024, 184.432651983998
) %>%
  mutate(factor_2024 = inpc[anio == 2024] / inpc)   # multiplicar pesos de cada año por este factor

# El SIAP reporta por Distrito de Desarrollo Rural. Dos llevan el nombre de
# su ciudad y aquí se nombran por la región a la que corresponden.
nombre_region <- function(distrito) {
  case_when(
    distrito == "Huajuapan de León" ~ "Mixteca",
    distrito == "Tuxtepec"          ~ "Papaloapan",
    TRUE                            ~ distrito
  )
}

# 2. LECTURA DE LOS CIERRES AGRÍCOLAS ------------------------------------
# El año y la modalidad se leen del encabezado de cada archivo
# (celdas "Año: 2004" y "Modalidad: Temporal"), así no dependen del
# número del nombre del archivo.

lee_cierre <- function(archivo) {
  crudo <- read_excel(archivo, col_names = FALSE, col_types = "text",
                      .name_repair = "minimal")
  names(crudo) <- paste0("c", seq_len(ncol(crudo)))

  tibble(
    anio      = as.integer(str_extract(crudo$c1[1], "\\d{4}")),
    modalidad = str_to_lower(str_trim(str_remove(crudo$c1[4], "^Modalidad:"))),
    entidad   = crudo$c2,
    distrito  = crudo$c3,
    municipio = crudo$c4,
    sup_sembrada_ha   = suppressWarnings(as.numeric(crudo$c5)),
    sup_cosechada_ha  = suppressWarnings(as.numeric(crudo$c6)),
    valor_miles_pesos = suppressWarnings(as.numeric(crudo$c7))
  ) %>%
    filter(entidad == "Oaxaca", !is.na(distrito))     # descarta encabezados y fila "Total"
}

archivos <- list.files(
  file.path(dir_crudos, c("Agricultura Temporal", "Agricultura Riego")),
  pattern = "^Cierre.*\\.xlsx$", full.names = TRUE
)
stopifnot(length(archivos) == 10)

municipio_modalidad <- lapply(archivos, lee_cierre) %>%
  bind_rows() %>%
  mutate(region = nombre_region(distrito)) %>%
  left_join(select(deflactor, anio, factor_2024), by = "anio") %>%
  mutate(valor_real_miles_pesos_2024 = valor_miles_pesos * factor_2024) %>%
  select(anio, modalidad, region, distrito, municipio,
         sup_sembrada_ha, sup_cosechada_ha,
         valor_miles_pesos, valor_real_miles_pesos_2024) %>%
  arrange(anio, modalidad, region, municipio)

# Comprobación: cada año debe tener ambas modalidades
stopifnot(all(table(municipio_modalidad$anio, municipio_modalidad$modalidad) > 0))

# 3. TABLAS RESUMEN -------------------------------------------------------

resume <- function(df, ...) {
  df %>%
    group_by(...) %>%
    summarise(
      sup_sembrada_ha               = sum(sup_sembrada_ha, na.rm = TRUE),
      valor_miles_pesos             = sum(valor_miles_pesos, na.rm = TRUE),
      valor_real_miles_pesos_2024   = sum(valor_real_miles_pesos_2024, na.rm = TRUE),
      .groups = "drop"
    )
}

# Región y año (ambas modalidades), más el total de Oaxaca
region_anio <- resume(municipio_modalidad, region, anio)
region_anio <- bind_rows(
  region_anio,
  resume(municipio_modalidad, anio) %>% mutate(region = "Oaxaca (total)")
) %>% arrange(region, anio)

# Región, año y modalidad, más el total de Oaxaca
region_modalidad <- resume(municipio_modalidad, region, anio, modalidad)
region_modalidad <- bind_rows(
  region_modalidad,
  resume(municipio_modalidad, anio, modalidad) %>% mutate(region = "Oaxaca (total)")
) %>% arrange(region, anio, modalidad)

# 4. CULTIVOS --------------------------------------------------------------
# Se agrupan los cultivos en seis grupos (flores y lo no listado van a "Otros"). El agrupamiento es una decisión
# analítica: está en este vector para poder editarlo fácilmente.

grupo_cultivo <- c(
  "Maíz grano"                  = "Maíz",
  "Frijol"                      = "Frijol",
  "Sorgo grano"                 = "Otros granos y forrajes",
  "Trigo grano"                 = "Otros granos y forrajes",
  "Cebada grano"                = "Otros granos y forrajes",
  "Alpiste grano"               = "Otros granos y forrajes",
  "Arroz palay"                 = "Otros granos y forrajes",
  "Amaranto"                    = "Otros granos y forrajes",
  "Garbanzo grano"              = "Otros granos y forrajes",
  "Haba grano"                  = "Otros granos y forrajes",
  "Arvejón"                     = "Otros granos y forrajes",
  "Chícharo"                    = "Otros granos y forrajes",
  "Avena forrajera en verde"    = "Otros granos y forrajes",
  "Maíz forrajero en verde"     = "Otros granos y forrajes",
  "Sorgo forrajero en verde"    = "Otros granos y forrajes",
  "Cebada forrajera en verde"   = "Otros granos y forrajes",
  "Trigo forrajero verde"       = "Otros granos y forrajes",
  "Ebo (janamargo o veza)"      = "Otros granos y forrajes",
  "Ajonjolí"                    = "Ajonjolí, cacahuate y jamaica",
  "Cacahuate"                   = "Ajonjolí, cacahuate y jamaica",
  "Jamaica"                     = "Ajonjolí, cacahuate y jamaica",
  "Chile verde"                 = "Hortalizas y frutas",
  "Tomate rojo (jitomate)"      = "Hortalizas y frutas",
  "Tomate verde"                = "Hortalizas y frutas",
  "Calabacita"                  = "Hortalizas y frutas",
  "Sandía"                      = "Hortalizas y frutas",
  "Melón"                       = "Hortalizas y frutas",
  "Pepino"                      = "Hortalizas y frutas",
  "Cebolla"                     = "Hortalizas y frutas",
  "Ajo"                         = "Hortalizas y frutas",
  "Papa"                        = "Hortalizas y frutas",
  "Col (repollo)"               = "Hortalizas y frutas",
  "Cilantro"                    = "Hortalizas y frutas",
  "Fresa"                       = "Hortalizas y frutas",
  "Ejote"                       = "Hortalizas y frutas",
  "Camote"                      = "Hortalizas y frutas",
  "Jengibre"                    = "Hortalizas y frutas",
  "Zanahoria"                   = "Hortalizas y frutas"
)

cultivos <- read_excel(file.path(dir_crudos, "Cultivos.xlsx")) %>%
  transmute(
    region = str_to_title(str_trim(`Región`)),         # "Sierra juárez" -> "Sierra Juárez"
    anio   = as.integer(`Año`),
    cultivo = str_trim(Cultivo),
    sup_sembrada_ha   = as.numeric(`Superficie (ha)`),
    valor_miles_pesos = as.numeric(`Valor Producción (mdp)`)   # en realidad miles de pesos
  ) %>%
  mutate(grupo = coalesce(unname(grupo_cultivo[cultivo]), "Otros")) %>%
  left_join(select(deflactor, anio, factor_2024), by = "anio") %>%
  mutate(valor_real_miles_pesos_2024 = valor_miles_pesos * factor_2024) %>%
  select(-factor_2024) %>%
  arrange(region, anio, cultivo)

# Comprobación: los cultivos deben sumar lo mismo que los cierres por región
chequeo <- cultivos %>%
  group_by(region, anio) %>%
  summarise(ha_cultivos = sum(sup_sembrada_ha), .groups = "drop") %>%
  inner_join(filter(region_anio, region != "Oaxaca (total)") %>%
               select(region, anio, ha_cierres = sup_sembrada_ha),
             by = c("region", "anio"))
stopifnot(all(abs(chequeo$ha_cultivos - chequeo$ha_cierres) < 5))

# 5. ESCRITURA --------------------------------------------------------------

dir.create("datos", showWarnings = FALSE)
write_csv(municipio_modalidad, "datos/oaxaca_municipio_modalidad.csv")
write_csv(region_anio,         "datos/oaxaca_region_anio.csv")
write_csv(region_modalidad,    "datos/oaxaca_region_modalidad.csv")
write_csv(cultivos,            "datos/oaxaca_cultivos.csv")
write_csv(deflactor,           "datos/deflactor_inpc.csv")

message("Listo: ", nrow(municipio_modalidad), " filas municipales, ",
        n_distinct(municipio_modalidad$region), " regiones, años ",
        paste(sort(unique(municipio_modalidad$anio)), collapse = ", "))
