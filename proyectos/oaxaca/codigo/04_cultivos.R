#=========================================================================
# 04_cultivos.R
# Qué se siembra y cómo cambia: concentración por región, valor por
# hectárea de cada cultivo y qué cultivos explican el cambio en el valor.
# Lee datos/oaxaca_cultivos.csv; guarda PNG en figuras/.
# Valores en pesos de 2024.
#=========================================================================

library(readr); library(dplyr); library(tidyr); library(stringr)
library(ggplot2); library(scales); library(forcats)

cultivos <- read_csv("datos/oaxaca_cultivos.csv", show_col_types = FALSE) %>%
  mutate(cultivo = case_when(
    cultivo == "Maíz grano"             ~ "Maíz",
    cultivo == "Tomate rojo (jitomate)" ~ "Tomate rojo",
    cultivo == "Sorgo grano"            ~ "Sorgo",
    cultivo == "Trigo grano"            ~ "Trigo",
    cultivo == "Garbanzo grano"         ~ "Garbanzo",
    TRUE ~ cultivo))

dir.create("figuras", showWarnings = FALSE)
source("codigo/00_estilo.R")

# FIGURA 10 — Concentración de la superficie por región ----------------------
# Índice de concentración (Herfindahl): 1 = un solo cultivo; baja si la
# superficie se reparte entre más cultivos.

hhi <- cultivos %>%
  group_by(region, anio) %>%
  mutate(p = sup_sembrada_ha / sum(sup_sembrada_ha)) %>%
  summarise(hhi = sum(p^2), .groups = "drop") %>%
  filter(anio %in% c(2004, 2024)) %>%
  pivot_wider(names_from = anio, values_from = hhi, names_prefix = "a") %>%
  mutate(cambio = a2024 - a2004,
         sentido = ifelse(cambio > 0, "Más concentrada", "Más diversa"),
         region = fct_reorder(region, a2024))

fig10 <- ggplot(hhi, aes(y = region)) +
  geom_segment(aes(x = a2004, xend = a2024, yend = region, colour = sentido),
               linewidth = 2.2, lineend = "round") +
  geom_point(aes(x = a2004), shape = 21, size = 3.6, fill = col$papel,
             colour = col$tinta_suave, stroke = 1.2) +
  geom_point(aes(x = a2024, colour = sentido), size = 3.6) +
  geom_text(aes(x = a2004, label = "2004"), data = filter(hhi, region == levels(region)[1]),
            vjust = 2.1, size = 3, colour = col$tinta_suave) +
  geom_text(aes(x = a2024, label = "2024"), data = filter(hhi, region == levels(region)[1]),
            vjust = 2.1, size = 3, colour = col$tinta_suave, fontface = "bold") +
  scale_colour_manual(values = c("Más concentrada" = col$superficie, "Más diversa" = col$valor),
                      guide = guide_legend(override.aes = list(linewidth = 2.2))) +
  scale_x_continuous(limits = c(0.45, 1), breaks = seq(0.5, 1, 0.1)) +
  labs(title = "Valles Centrales se concentra en pocos cultivos; el Istmo se diversifica",
       subtitle = "Índice de concentración de la superficie sembrada (1 = un solo cultivo), 2004 y 2024.",
       x = NULL, y = NULL, caption = fuente) +
  tema_agro() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = col$rejilla, linewidth = 0.3))

guarda(fig10, "10_concentracion_cultivos.png", 10, 5)

# FIGURA 11 — Valor por hectárea de cada cultivo (2024) ----------------------

# (valor por hectárea = valor real de 2024 / superficie sembrada 2024)
vph <- cultivos %>%
  filter(anio == 2024) %>%
  group_by(cultivo, grupo) %>%
  summarise(ha = sum(sup_sembrada_ha), v = sum(valor_real_miles_pesos_2024), .groups = "drop") %>%
  mutate(vph = v / ha) %>%
  slice_max(ha, n = 14) %>%
  mutate(cultivo = fct_reorder(cultivo, vph))

fig11 <- ggplot(vph, aes(vph, cultivo, colour = grupo)) +
  geom_segment(aes(x = 1, xend = vph, yend = cultivo), linewidth = 1, alpha = 0.55) +
  geom_point(size = 3.6) +
  geom_text(aes(label = comma(vph, accuracy = 1)), hjust = -1.1, size = 3,
            colour = col$tinta, show.legend = FALSE) +
  scale_x_log10(breaks = c(1, 10, 100, 1000), labels = comma,
                expand = expansion(mult = c(0.02, 0.15))) +
  scale_colour_manual(values = col_cultivo, breaks = names(col_cultivo)) +
  guides(colour = guide_legend(nrow = 2)) +
  labs(title = "Una hectárea de jitomate genera unas 190 veces el valor de una de maíz",
       subtitle = "Valor de la producción por hectárea sembrada, 2024, miles de pesos de 2024 (escala logarítmica). Los 14 cultivos con más superficie.",
       x = NULL, y = NULL, caption = fuente) +
  tema_agro() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = col$rejilla, linewidth = 0.3),
        legend.box = "vertical")

guarda(fig11, "11_valor_por_hectarea_cultivos.png", 10, 6.6)

# FIGURA 12 — Qué cultivos explican el cambio en el valor ---------------------

cambio <- function(df, etiqueta, n = 5) {
  df %>%
    filter(anio %in% c(2004, 2024)) %>%
    group_by(cultivo, anio) %>%
    summarise(v = sum(valor_real_miles_pesos_2024), .groups = "drop") %>%
    pivot_wider(names_from = anio, values_from = v, names_prefix = "a", values_fill = 0) %>%
    mutate(d = (a2024 - a2004) / 1000, panel = etiqueta) %>%     # millones de pesos
    filter(rank(-d) <= n | rank(d) <= n)
}

dc <- bind_rows(cambio(cultivos, "Oaxaca"),
                cambio(filter(cultivos, region == "Valles Centrales"), "Valles Centrales")) %>%
  mutate(panel = factor(panel, levels = c("Oaxaca", "Valles Centrales")),
         clave = paste(cultivo, panel, sep = "___"),
         clave = fct_reorder(clave, d),
         signo = ifelse(d > 0, "Gana valor", "Pierde valor"))

fig12 <- ggplot(dc, aes(d, clave, fill = signo)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 0, colour = col$tinta_suave, linewidth = 0.4) +
  geom_text(aes(label = sprintf("%+s", comma(d, accuracy = 1)),
                hjust = ifelse(d > 0, -0.15, 1.15)), size = 3, colour = col$tinta) +
  facet_wrap(~panel, scales = "free") +
  scale_y_discrete(labels = function(x) sub("___.*", "", x)) +
  scale_x_continuous(expand = expansion(mult = c(0.18, 0.18)), labels = comma) +
  scale_fill_manual(values = c("Gana valor" = col$valor, "Pierde valor" = col$superficie)) +
  labs(title = "El jitomate explica casi todo el aumento de valor en Valles Centrales",
       subtitle = "Cambio en el valor de la producción entre 2004 y 2024, millones de pesos de 2024. Cinco cultivos que más suben y cinco que más bajan.",
       x = NULL, y = NULL, caption = fuente) +
  tema_agro() +
  theme(panel.grid.major.y = element_blank(), legend.position = "none")

guarda(fig12, "12_cultivos_que_explican_el_cambio.png", 11, 5.6)

message("Listo: 3 figuras (10-12) en figuras/")
