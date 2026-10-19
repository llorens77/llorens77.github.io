# %% [markdown]
# # Exploración: acuíferos sobreexplotados (CONAGUA)


# %% 1. Librerías
from pathlib import Path

import geopandas as gpd
import pandas as pd

# Mostrar todas las columnas al imprimir tablas
pd.set_option("display.max_columns", None)

# %% 2. Encontrar y leer el shapefile
# Busca el .shp dentro de la carpeta de datos crudos, sin importar su nombre exacto.
carpeta = Path("proyectos/acuiferos-coahuila/datos/crudos")
archivo_shp = next(carpeta.rglob("*.shp"))
print(archivo_shp)

acuiferos = gpd.read_file(archivo_shp)
# Si los acentos salen raros (Ã³ en vez de ó), usa:
# acuiferos = gpd.read_file(archivo_shp, encoding="latin1")

# %% 3. Primer vistazo: tamaño, columnas y tipos
print(acuiferos.shape)      # (filas, columnas): cada fila es un acuífero
print(acuiferos.dtypes)     # tipo de dato de cada columna
acuiferos.head()

# %% 4. Sistema de coordenadas
# Debe decir algo como EPSG:4326 (WGS84: latitud y longitud en grados).
acuiferos.crs

# %% 5. Mapa rápido de todo el país
acuiferos.plot(figsize=(10, 7), edgecolor="white", linewidth=0.3)

# %% 6. Buscar los acuíferos de Coahuila y La Laguna
# Todavía no sabemos cómo se llaman las columnas, así que buscamos el texto en todas.
texto = acuiferos.drop(columns="geometry").astype(str)
coincide = texto.apply(
    lambda col: col.str.contains("COAH|Coah|LAGUN|Lagun", regex=True)
).any(axis=1)

coahuila = acuiferos[coincide]
print(len(coahuila), "acuíferos encontrados")
coahuila.drop(columns="geometry")

# %% 7. Mapa de la selección
ax = acuiferos.plot(color="lightgrey", figsize=(10, 7))
coahuila.plot(ax=ax, color="firebrick", edgecolor="white", linewidth=0.5)
ax.set_xlim(coahuila.total_bounds[[0, 2]] + [-2, 2])
ax.set_ylim(coahuila.total_bounds[[1, 3]] + [-2, 2])
ax.set_axis_off()

# %% 8. Mapa interactivo (requiere: uv add folium mapclassify)
coahuila.explore(tooltip=True)
