#=========================================================================
# 02_graficas.R
# Gráficas del proyecto "Superficie y valor de la producción agrícola en
# Oaxaca, 2004-2024". Lee los CSV que escribe 01_limpieza.R y guarda los
# PNG en la carpeta figuras/.
#
# Pregunta de fondo: ¿qué se siembra, cómo cambia, y dónde la superficie
# sembrada y el valor de lo que se produce se mueven en direcciones
# distintas? Todas las comparaciones de valor usan pesos de 2024.
#=========================================================================

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggrepel)
library(scales)
library(forcats)

# 1. DATOS -----------------------------------------------------------------

region_anio      <- read_csv("datos/oaxaca_region_anio.csv",      show_col_types = FALSE)
region_modalidad <- read_csv("datos/oaxaca_region_modalidad.csv", show_col_types = FALSE)
cultivos         <- read_csv("datos/oaxaca_cultivos.csv",         show_col_types = FALSE)

dir.create("figuras", showWarnings = FALSE)

# 2. ESTILO ----------------------------------------------------------------
source("codigo/00_estilo.R")

anios <- c(2004, 2009, 2014, 2019, 2024)

# 3. FIGURA 1 — Superficie y valor: índice 2004 = 100 ----------------------

indice <- region_anio %>%
  group_by(region) %>%
  arrange(anio) %>%
  mutate(
    Superficie = sup_sembrada_ha / first(sup_sembrada_ha) * 100,
    Valor      = valor_real_miles_pesos_2024 / first(valor_real_miles_pesos_2024) * 100
  ) %>%
  ungroup()

# Orden de paneles: Oaxaca primero; luego regiones por cambio del valor real
orden_regiones <- indice %>%
  filter(anio == 2024, region != "Oaxaca (total)") %>%
  arrange(desc(Valor)) %>%
  pull(region)
orden_regiones <- c("Oaxaca (total)", orden_regiones)

indice <- indice %>% mutate(region = factor(region, levels = orden_regiones))

indice_largo <- indice %>%
  select(region, anio, Superficie, Valor) %>%
  pivot_longer(c(Superficie, Valor), names_to = "serie", values_to = "indice") %>%
  mutate(serie = factor(serie, levels = c("Superficie", "Valor"),
                        labels = c("Superficie sembrada", "Valor de la producción")))

etiquetas_fin <- indice_largo %>%
  filter(anio == 2024) %>%
  mutate(etiqueta = pct_signo(indice - 100))

fig1 <- ggplot(indice_largo, aes(anio, indice)) +
  geom_hline(yintercept = 100, colour = col$tinta_suave, linewidth = 0.3, linetype = "dashed") +
  geom_ribbon(data = indice, aes(x = anio, ymin = pmin(Superficie, Valor),
                                 ymax = pmax(Superficie, Valor)),
              inherit.aes = FALSE, fill = col$tinta_suave, alpha = 0.12) +
  geom_line(aes(colour = serie), linewidth = 0.9) +
  geom_point(aes(colour = serie), size = 2.1, shape = 21, fill = col$papel, stroke = 1.1) +
  geom_text_repel(data = etiquetas_fin, aes(label = etiqueta), colour = col$tinta,
                  size = 3.1, hjust = 0, direction = "y", nudge_x = 1.2,
                  segment.colour = NA, box.padding = 0.1, min.segment.length = Inf,
                  xlim = c(2024.2, 2031)) +
  facet_wrap(~region, ncol = 4) +
  scale_colour_manual(values = c("Superficie sembrada" = col$superficie,
                                 "Valor de la producción" = col$valor)) +
  scale_x_continuous(breaks = c(2004, 2014, 2024), limits = c(2004, 2031),
                     labels = c("2004", "2014", "2024")) +
  scale_y_continuous(limits = c(55, 165), breaks = c(60, 80, 100, 120, 140, 160)) +
  labs(
    title = "Menos superficie sembrada, ¿más valor? Solo en dos regiones",
    subtitle = "Índice 2004 = 100. El sombreado marca la distancia entre la superficie y el valor de la producción.",
    x = NULL, y = "Índice (2004 = 100)",
    caption = fuente
  ) +
  tema_agro()

guarda(fig1, "01_superficie_vs_valor_indice.png", 11, 6.4)

# 4. FIGURA 2 — Cambio 2004-2024: superficie vs valor ----------------------

cambio <- indice %>%
  filter(anio == 2024) %>%
  transmute(region = as.character(region),
            Superficie = Superficie - 100,
            Valor = Valor - 100,
            brecha = Valor - Superficie) %>%
  mutate(region = fct_reorder(region, brecha))

cambio_largo <- cambio %>%
  pivot_longer(c(Superficie, Valor), names_to = "serie", values_to = "cambio") %>%
  group_by(region) %>%
  mutate(lado = ifelse(cambio == min(cambio), "izq", "der")) %>%   # etiqueta hacia afuera: sin encimarse
  ungroup() %>%
  mutate(serie = factor(serie, levels = c("Superficie", "Valor"),
                        labels = c("Superficie sembrada", "Valor de la producción")))

fig2 <- ggplot(cambio, aes(y = region)) +
  geom_vline(xintercept = 0, colour = col$tinta_suave, linewidth = 0.4) +
  geom_segment(aes(x = Superficie, xend = Valor, yend = region),
               colour = col$tinta_suave, alpha = 0.45, linewidth = 1.6, lineend = "round") +
  geom_point(data = cambio_largo, aes(x = cambio, colour = serie), size = 4.2) +
  geom_text(data = cambio_largo, aes(x = cambio, label = pct_signo(cambio),
                                     hjust = ifelse(lado == "der", -0.4, 1.4)),
            colour = col$tinta, size = 3.2) +
  scale_colour_manual(values = c("Superficie sembrada" = col$superficie,
                                 "Valor de la producción" = col$valor)) +
  scale_x_continuous(limits = c(-45, 85), breaks = seq(-40, 80, 20),
                     labels = function(x) ifelse(x == 0, "0%", pct_signo(x))) +
  labs(
    title = "Valles Centrales y la Mixteca concentran la brecha",
    subtitle = "Cambio entre 2004 y 2024. La brecha es la distancia entre el punto azul y el rojo.",
    x = NULL, y = NULL, caption = fuente
  ) +
  tema_agro() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = col$rejilla, linewidth = 0.3))

guarda(fig2, "02_brecha_superficie_valor.png", 9, 5.2)

# 5. FIGURAS 3 y 4 — Temporal y riego --------------------------------------

modalidad <- region_modalidad %>%
  mutate(
    region = factor(region, levels = orden_regiones),
    modalidad = factor(modalidad, levels = c("temporal", "riego"),
                       labels = c("Temporal", "Riego"))
  )

# Fig. 3: superficie
fig3 <- ggplot(modalidad, aes(factor(anio), sup_sembrada_ha / 1000, fill = modalidad)) +
  geom_col(width = 0.72, colour = col$papel, linewidth = 0.6) +
  facet_wrap(~region, ncol = 4, scales = "free_y") +
  scale_fill_manual(values = c("Temporal" = col$temporal, "Riego" = col$riego)) +
  scale_x_discrete(breaks = c("2004", "2014", "2024")) +
  scale_y_continuous(labels = label_number(big.mark = ","), expand = expansion(mult = c(0, 0.14))) +
  labs(
    title = "Más de nueve de cada diez hectáreas dependen de la lluvia",
    subtitle = "Superficie sembrada (miles de hectáreas). Cada panel tiene su propia escala.",
    x = NULL, y = NULL, caption = fuente
  ) +
  tema_agro()

guarda(fig3, "03_modalidad_superficie.png", 11, 6.4)

# Fig. 4: valor
fig4 <- ggplot(modalidad, aes(factor(anio), valor_real_miles_pesos_2024 / 1000, fill = modalidad)) +
  geom_col(width = 0.72, colour = col$papel, linewidth = 0.6) +
  facet_wrap(~region, ncol = 4, scales = "free_y") +
  scale_fill_manual(values = c("Temporal" = col$temporal, "Riego" = col$riego)) +
  scale_x_discrete(breaks = c("2004", "2014", "2024")) +
  scale_y_continuous(labels = label_number(big.mark = ","), expand = expansion(mult = c(0, 0.14))) +
  labs(
    title = "En Oaxaca, el valor crece por el riego; el del temporal se mantiene",
    subtitle = "Valor de la producción (millones de pesos de 2024). Cada panel tiene su propia escala.",
    x = NULL, y = NULL, caption = fuente
  ) +
  tema_agro()

guarda(fig4, "04_modalidad_valor.png", 11, 6.4)

# 6. FIGURA 5 — Qué se siembra: composición de la superficie ---------------

niveles_grupo <- names(col_cultivo)

comp <- cultivos %>%
  mutate(grupo = factor(grupo, levels = rev(niveles_grupo))) %>%
  group_by(region, anio, grupo) %>%
  summarise(sup = sum(sup_sembrada_ha), .groups = "drop") %>%
  group_by(region, anio) %>%
  mutate(prop = sup / sum(sup)) %>%
  ungroup() %>%
  mutate(region = factor(region, levels = orden_regiones[-1]))

maiz_etiq <- comp %>%
  filter(grupo == "Maíz", anio %in% c(2004, 2024)) %>%
  mutate(etiqueta = percent(prop, accuracy = 1))

fig5 <- ggplot(comp, aes(factor(anio), prop, fill = grupo)) +
  geom_col(width = 0.78, colour = col$papel, linewidth = 0.6) +
  geom_text(data = maiz_etiq, aes(x = factor(anio), y = 0.5 * prop, label = etiqueta),
            colour = col$tinta, fontface = "bold", size = 3, inherit.aes = FALSE) +
  facet_wrap(~region, ncol = 4) +
  scale_fill_manual(values = col_cultivo, breaks = niveles_grupo) +
  scale_x_discrete(breaks = c("2004", "2014", "2024")) +
  scale_y_continuous(labels = label_percent(), expand = expansion(mult = c(0, 0.04))) +
  guides(fill = guide_legend(nrow = 2, reverse = FALSE)) +
  labs(
    title = "El maíz ocupa entre 71% y 97% de la superficie de cada región",
    subtitle = "Composición de la superficie sembrada por grupo de cultivo. Etiqueta: participación del maíz.",
    x = NULL, y = NULL, caption = fuente
  ) +
  tema_agro()

guarda(fig5, "05_composicion_cultivos.png", 11, 6.4)

# 7. FIGURA 6 — Qué se siembra vs qué vale (2024) --------------------------

peso <- cultivos %>%
  filter(anio == 2024) %>%
  mutate(grupo = factor(grupo, levels = rev(niveles_grupo))) %>%
  group_by(region, grupo) %>%
  summarise(Superficie = sum(sup_sembrada_ha),
            Valor = sum(valor_real_miles_pesos_2024), .groups = "drop") %>%
  pivot_longer(c(Superficie, Valor), names_to = "medida", values_to = "x") %>%
  group_by(region, medida) %>%
  mutate(prop = x / sum(x)) %>%
  ungroup() %>%
  mutate(region = factor(region, levels = orden_regiones[-1]),
         medida = factor(medida, levels = c("Valor", "Superficie")))

fig6 <- ggplot(peso, aes(prop, medida, fill = grupo)) +
  geom_col(width = 0.7, colour = col$papel, linewidth = 0.6) +
  geom_text(data = mutate(peso, etiqueta = ifelse(prop >= 0.12, percent(prop, accuracy = 1), "")),
            aes(label = etiqueta, group = grupo,
                colour = grupo %in% c("Maíz", "Ajonjolí, cacahuate y jamaica")),
            position = position_stack(vjust = 0.5), size = 3, fontface = "bold") +
  scale_colour_manual(values = c(`TRUE` = col$tinta, `FALSE` = "white"), guide = "none") +
  facet_wrap(~region, ncol = 2) +
  scale_fill_manual(values = col_cultivo, breaks = niveles_grupo) +
  scale_x_continuous(labels = label_percent(), expand = c(0, 0)) +
  guides(fill = guide_legend(nrow = 2)) +
  labs(
    title = "El maíz ocupa la mayor parte de la tierra, pero genera menos valor",
    subtitle = "2024. Participación de cada grupo de cultivo en la superficie sembrada y en el valor de la producción.",
    x = NULL, y = NULL, caption = fuente
  ) +
  tema_agro() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank())

guarda(fig6, "06_superficie_vs_valor_cultivos_2024.png", 9, 8)

message("Listo: 6 figuras en figuras/")
