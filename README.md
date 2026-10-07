# bccr

Cliente **no oficial** en R para la API publica de indicadores economicos
(SDDE) del Banco Central de Costa Rica. Descarga las series en formato
ordenado y las guarda en parquet.

> Este paquete no esta afiliado ni respaldado por el BCCR. Los datos son
> propiedad del BCCR y estan sujetos a sus condiciones de uso.

## Instalacion

```r
# install.packages("pak")
pak::pak("fabianbt6/bccr-api")
```

## Configuracion (una vez por maquina)

1. Solicita el API key en el sitio del BCCR.
2. Abre `.Renviron` con `usethis::edit_r_environ()` y agregue:

   ```
   BCCR_API_KEY=<tu key>
   ```

3. Reiniciar R.

El `.Renviron` vive en su carpeta de usuario, fuera de cualquier repo, así
que la key nunca se sube a git.

## Uso

```r
library(bccr)

# Indicadores por defecto
datos <- descargar_bccr()

# Indicadores y fechas a eleccion
tc <- descargar_bccr(ids = c("317", "3541"), fecha_inicio = "2020-01-01")

# Guardar y leer en parquet
guardar_bccr(datos, "datos/datos_bccr.parquet")
datos <- leer_bccr("datos/datos_bccr.parquet")
```

El resultado tiene las columnas `id`, `Serie`, `Fecha` y `Valor`.

## Notas

Partes del codigo se desarrollaron con asistencia de Claude (Anthropic).

## Licencia

MIT (c) Fabián Brenes
