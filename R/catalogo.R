#' Catalogo de indicadores disponibles
#'
#' Descarga de la API del BCCR el archivo Excel con todos los indicadores
#' economicos disponibles (codigo, nombre, cuadro, etc.) y lo devuelve como
#' tabla.
#'
#' @param ruta_excel Ruta opcional donde guardar el Excel original. Si es
#'   `NULL` (por defecto), se usa un archivo temporal.
#' @inheritParams get_serie_bccr
#'
#' @return Un tibble con el contenido del catalogo, tal como lo publica el
#'   BCCR.
#' @export
#' @examples
#' \dontrun{
#' catalogo <- catalogo_bccr()
#' dplyr::filter(catalogo, grepl("IMAE", catalogo[[2]]))
#' }
catalogo_bccr <- function(ruta_excel = NULL,
                          token = Sys.getenv("BCCR_API_KEY")) {
  token <- validar_token(token)
  if (is.null(ruta_excel)) ruta_excel <- tempfile(fileext = ".xlsx")

  bccr_request("/indicadoresEconomicos/descargar", token) |>
    httr2::req_perform(path = ruta_excel)

  leer_catalogo(ruta_excel)
}

# El Excel trae un titulo y posiblemente otras filas antes de la tabla.
# Se ubica la primera fila cuyo primer campo es un codigo numerico; la
# fila anterior es el encabezado.
leer_catalogo <- function(ruta) {
  crudo <- readxl::read_excel(ruta, col_names = FALSE, col_types = "text",
                              .name_repair = "minimal")
  col1 <- trimws(crudo[[1]])
  primera <- which(grepl("^[0-9]+$", col1))[1]
  if (is.na(primera) || primera < 2) {
    stop("No se reconocio la estructura del catalogo del BCCR.", call. = FALSE)
  }

  encabezado <- limpiar_nombres(unlist(crudo[primera - 1, ], use.names = FALSE))
  datos <- crudo[primera:nrow(crudo), , drop = FALSE]
  names(datos) <- encabezado

  # Quitar columnas sin nombre y vacias, y filas sin codigo
  vacias <- grepl("^col_[0-9]+$", encabezado) & vapply(datos, function(x) all(is.na(x)), logical(1))
  datos <- datos[, !vacias, drop = FALSE]
  datos <- datos[grepl("^[0-9]+$", trimws(datos[[1]])), , drop = FALSE]
  tibble::as_tibble(datos)
}

# "Codigo del indicador" -> "codigo_del_indicador" (sin tildes ni simbolos)
limpiar_nombres <- function(x) {
  x[is.na(x)] <- ""
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  x <- tolower(gsub("[^A-Za-z0-9]+", "_", trimws(x)))
  x <- gsub("^_|_$", "", x)
  x[x == ""] <- paste0("col_", which(x == ""))
  make.unique(x, sep = "_")
}

#' Metadatos de indicadores
#'
#' Consulta nombre, periodicidad, unidad de medida y fechas del primer y
#' ultimo dato de uno o varios indicadores.
#'
#' @param ids Vector de codigos de indicadores.
#' @inheritParams get_serie_bccr
#'
#' @return Un tibble con columnas `id`, `nombre`, `periodicidad`,
#'   `unidad`, `primer_dato`, `ultimo_dato` y `ultima_publicacion`.
#' @export
#' @examples
#' \dontrun{
#' info_bccr(indicadores_default())
#' }
info_bccr <- function(ids, token = Sys.getenv("BCCR_API_KEY")) {
  token <- validar_token(token)
  ids <- as.character(ids)

  una <- function(id) {
    datos <- bccr_request(paste0("/indicadoresEconomicos/", id, "/metadata"), token) |>
      bccr_json()
    m <- datos[[1]]
    tibble::tibble(
      id = id,
      nombre = valor_o_na(m$nombre),
      periodicidad = valor_o_na(m$periodicidad),
      unidad = valor_o_na(m$unidadDeMedida),
      primer_dato = como_fecha(valor_o_na(m$primerDato)),
      ultimo_dato = como_fecha(valor_o_na(m$ultimoDatoSerie)),
      ultima_publicacion = como_fecha(valor_o_na(m$ultimaPublicacion))
    )
  }

  segura <- purrr::possibly(una, otherwise = NULL, quiet = FALSE)
  res <- purrr::map(ids, segura)
  fallo <- vapply(res, is.null, logical(1))
  if (any(fallo)) {
    warning("Sin metadatos para: ", paste(ids[fallo], collapse = ", "), call. = FALSE)
  }
  dplyr::bind_rows(res[!fallo])
}
