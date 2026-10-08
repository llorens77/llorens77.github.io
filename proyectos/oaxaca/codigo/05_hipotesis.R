#=========================================================================
# 05_hipotesis.R
# Figuras que ponen a prueba lo que sí se puede decir con los datos:
# (13) ¿a dónde va la superficie que pierde el maíz?
# (14) ¿qué tan concentrada está la pérdida de superficie entre municipios?
# Lee datos/oaxaca_cultivos.csv y datos/oaxaca_municipio_modalidad.csv.
#=========================================================================

library(readr); library(dplyr); library(tidyr); library(ggplot2)
library(scales); library(forcats)

cultivos <- read_csv("datos/oaxaca_cultivos.csv", show_col_types = FALSE)
mun      <- read_csv("datos/oaxaca_municipio_modalidad.csv", show_col_types = FALSE)

dir.create("figuras", showWarnings = FALSE)
source("codigo/00_estilo.R")

# FIGURA 13 — Balance de hectáreas por grupo de cultivo -----------------------

balance <- cultivos %>%
  filter(anio %in% c(2004, 2024)) %>%
  group_by(grupo, anio) %>%
  summarise(ha = sum(sup_sembrada_ha), .groups = "drop") %>%
  pivot_wider(names_from = anio, values_from = ha, names_prefix = "a") %>%
  mutate(cambio = a2024 - a2004,
         grupo = fct_reorder(grupo, cambio))

total04 <- sum(balance$a2004); total_cambio <- sum(balance$cambio)

fig13 <- ggplot(balance, aes(cambio, grupo, fill = grupo)) +
  geom_col(width = 0.65) +
  geom_vline(xintercept = 0, colour = col$tinta_suave, linewidth = 0.4) +
  geom_text(aes(label = sprintf("%+s", comma(round(cambio, -1))),
                hjust = ifelse(cambio >= 0, -0.15, 1.15)),
            size = 3.3, colour = col$tinta) +
  scale_fill_manual(values = col_cultivo, guide = "none") +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0.2, 0.12))) +
  labs(title = "La superficie que pierde el maíz no la gana ningún otro cultivo registrado",
       subtitle = sprintf("Cambio en hectáreas sembradas entre 2004 y 2024. Total: %s ha (%s).",
                          comma(round(total_cambio, -2)), percent(total_cambio / total04, accuracy = 1)),
       x = NULL, y = NULL, caption = fuente) +
  tema_agro() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = col$rejilla, linewidth = 0.3))

guarda(fig13, "13_balance_hectareas.png", 10, 4.6)

# FIGURA 14 — Concentración de la pérdida entre municipios ------------------

mcambio <- mun %>%
  filter(anio %in% c(2004, 2024)) %>%
  group_by(municipio, region, anio) %>%
  summarise(ha = sum(sup_sembrada_ha, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = anio, values_from = ha, names_prefix = "a") %>%
  mutate(cambio = a2024 - a2004)

n_total   <- nrow(mcambio)
pierden   <- mcambio %>% filter(cambio < 0) %>% arrange(cambio) %>%
  mutate(rango = row_number() / n(), acum = cumsum(-cambio) / sum(-cambio))
n_pierden <- nrow(pierden)
p10 <- pierden %>% filter(rango >= 0.10) %>% slice(1)

fig14 <- ggplot(pierden, aes(rango, acum)) +
  geom_abline(slope = 1, intercept = 0, colour = col$tinta_suave,
              linetype = "22", linewidth = 0.4) +
  geom_line(colour = col$superficie, linewidth = 1.2) +
  geom_point(data = p10, size = 3.4, colour = col$superficie) +
  annotate("text", x = p10$rango + 0.03, y = p10$acum - 0.08, hjust = 0, size = 3.5,
           colour = col$tinta, fontface = "bold", lineheight = 1.1,
           label = sprintf("El 10%% de los municipios que pierden\nconcentra %s de la pérdida", percent(p10$acum, accuracy = 1))) +
  annotate("text", x = 0.55, y = 0.4, hjust = 0, size = 3.1, colour = col$tinta_suave,
           label = "Línea punteada: pérdida repartida\npor igual entre municipios") +
  scale_x_continuous(labels = label_percent(), limits = c(0, 1), expand = c(0.01, 0)) +
  scale_y_continuous(labels = label_percent(), limits = c(0, 1), expand = c(0.01, 0)) +
  labs(title = "Pocos municipios concentran la mayor parte de la superficie perdida",
       subtitle = sprintf("Pierden superficie sembrada entre 2004 y 2024 %s de %s municipios (%s ha en total).",
                          n_pierden, n_total, comma(round(sum(-pierden$cambio), -2))),
       x = "Proporción de los municipios que pierden (de mayor a menor pérdida)",
       y = "Proporción acumulada de la pérdida",
       caption = paste0(fuente, "\nNo descuenta los 216 municipios que ganan superficie.")) +
  tema_agro() +
  theme(panel.grid.major.x = element_line(colour = col$rejilla, linewidth = 0.3))

guarda(fig14, "14_concentracion_perdida_municipios.png", 9, 5.8)

message("Listo: figuras 13 y 14 en figuras/")
