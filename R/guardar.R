#' Guardar y leer datos del BCCR en formato parquet
#'
#' @param datos Tibble devuelto por [descargar_bccr()].
#' @param ruta Ruta del archivo `.parquet`. Las carpetas que falten se crean.
#'
#' @return `guardar_bccr()` devuelve la ruta (invisible); `leer_bccr()` un
#'   tibble.
#' @export
#' @examples
#' \dontrun{
#' datos <- descargar_bccr()
#' guardar_bccr(datos, "datos/datos_bccr.parquet")
#' datos <- leer_bccr("datos/datos_bccr.parquet")
#' }
guardar_bccr <- function(datos, ruta) {
  dir.create(dirname(ruta), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(datos, ruta)
  message("Datos guardados en: ", ruta)
  invisible(ruta)
}

#' @rdname guardar_bccr
#' @export
leer_bccr <- function(ruta) {
  tibble::as_tibble(arrow::read_parquet(ruta))
}
