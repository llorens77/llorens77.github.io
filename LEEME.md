# Tema del portafolio

Qué cambia: logo en la navegación y en la portada, tipografía (Hanken Grotesk + DM Mono, alojadas en el sitio),
rejilla y tarjetas planas, portada con la tira de imágenes con "cotas".

## Cómo instalarlo

1. Haz una copia de respaldo de `_quarto.yml`, `styles.css` e `index.qmd` de tu carpeta `portafolio`.
2. Copia estos archivos a la raíz de `portafolio` y reemplaza los que ya existen:
   `_quarto.yml`, `styles.css`, `index.qmd`, `logo.svg`, `logo.png` y la carpeta `fonts/`.
3. No toca `about.qmd`, `proyectos.qmd` ni tus proyectos.
4. Abre `index.qmd` en RStudio y dale Render, o corre `quarto preview` en la Terminal.
5. Para publicar: `git add .`, `git commit -m "Nuevo tema"`, `git push` y `quarto publish gh-pages`.

## Dónde se cambia cada cosa

- Colores y espaciados: bloque `:root` al inicio de `styles.css`.
- Texto de la portada: `index.qmd`.
- Textos del menú y del pie: `_quarto.yml`.
