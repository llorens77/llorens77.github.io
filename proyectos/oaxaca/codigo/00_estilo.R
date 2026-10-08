#=========================================================================
# 00_estilo.R
# Colores, tema y utilidades compartidas por las gráficas y los mapas.
# Los demás scripts lo cargan con source("codigo/00_estilo.R").
#=========================================================================

library(ggplot2)
library(stringr)

# ESTILO ---------------------------------------------------------------------
# Colores validados para daltonismo y contraste sobre el fondo crema.

col <- list(
  papel      = "#fbf8f1",
  tinta      = "#2b2a27",
  tinta_suave = "#5b574f",
  rejilla    = "#e6e1d6",
  superficie = "#9c3d2e",   # tierra
  valor      = "#2a78d6",   # valor de la producción
  temporal   = "#c98500",   # temporal (lluvia)
  riego      = "#2a78d6"    # riego (agua)
)

col_cultivo <- c(
  "Maíz"                          = "#c98500",
  "Frijol"                        = "#9c3d2e",
  "Otros granos y forrajes"       = "#2a78d6",
  "Hortalizas y frutas"           = "#4f8a1f",
  "Ajonjolí, cacahuate y jamaica" = "#e0709a",
  "Otros"                         = "#a29e94"
)

tema_agro <- function(base_size = 11) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      plot.background   = element_rect(fill = col$papel, colour = NA),
      panel.background  = element_rect(fill = col$papel, colour = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_line(colour = col$rejilla, linewidth = 0.3),
      axis.text    = element_text(colour = col$tinta_suave, size = rel(0.85)),
      axis.title   = element_text(colour = col$tinta_suave, size = rel(0.9)),
      strip.text   = element_text(colour = col$tinta, face = "bold", hjust = 0, size = rel(1),
                                  margin = margin(b = 6)),
      panel.spacing.y = unit(1.3, "lines"),
      panel.spacing.x = unit(1.1, "lines"),
      plot.title    = element_text(colour = col$tinta, face = "bold", size = rel(1.35),
                                   hjust = 0, margin = margin(b = 4)),
      plot.subtitle = element_text(colour = col$tinta_suave, size = rel(0.95),
                                   hjust = 0, margin = margin(b = 10),
                                   lineheight = 1.15),
      plot.caption  = element_text(colour = col$tinta_suave, size = rel(0.75),
                                   hjust = 0, margin = margin(t = 10)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      legend.position = "top",
      legend.justification = "left",
      legend.text  = element_text(colour = col$tinta),
      legend.title = element_blank(),
      plot.margin  = margin(14, 16, 10, 14)
    )
}

fuente <- "Fuente: SIAP, Anuario Estadístico de la Producción Agrícola (año agrícola, riego + temporal). Valor en pesos de 2024 (deflactor: INPC, Banco Mundial)."

pct_signo <- function(x) {
  s <- sprintf("%+.0f%%", x)
  str_replace(s, "^-", "−")
}

guarda <- function(p, nombre, ancho, alto) {
  ggsave(file.path("figuras", nombre), p, width = ancho, height = alto,
         dpi = 200, bg = col$papel, device = ragg::agg_png)
}
