#' Descargar un indicador del BCCR
#'
#' @param id Codigo del indicador (texto o numero).
#' @param fecha_inicio,fecha_fin Fechas del rango a descargar (`Date` o texto
#'   `"AAAA-MM-DD"`).
#' @param token API key del BCCR. Por defecto se lee de la variable de entorno
#'   `BCCR_API_KEY`.
#'
#' @return Un tibble con columnas `Serie`, `Fecha` y `Valor`.
#' @export
#' @examples
#' \dontrun{
#' tc <- get_serie_bccr("317")
#' }
get_serie_bccr <- function(id,
                           fecha_inicio = "1987-01-31",
                           fecha_fin = Sys.Date(),
                           token = Sys.getenv("BCCR_API_KEY")) {
  token <- validar_token(token)

  datos <- bccr_request(
    paste0("/indicadoresEconomicos/", id, "/series"),
    token,
    fechaInicio = formatear_fecha(fecha_inicio),
    fechaFin = formatear_fecha(fecha_fin)
  ) |>
    bccr_json()

  parsear_series(datos)
}

#' Descargar varios indicadores del BCCR
#'
#' Descarga cada indicador con [get_serie_bccr()]. Si alguno falla (codigo
#' inexistente, problema de red puntual), avisa cual fue y sigue con los
#' demas.
#'
#' @param ids Vector de codigos de indicadores. Por defecto
#'   [indicadores_default()].
#' @inheritParams get_serie_bccr
#'
#' @return Un tibble con columnas `id`, `Serie`, `Fecha` y `Valor`.
#' @export
#' @examples
#' \dontrun{
#' datos <- descargar_bccr()
#' datos <- descargar_bccr(ids = c("317", "3541"), fecha_inicio = "2020-01-01")
#' }
descargar_bccr <- function(ids = indicadores_default(),
                           fecha_inicio = "1987-01-31",
                           fecha_fin = Sys.Date(),
                           token = Sys.getenv("BCCR_API_KEY")) {
  token <- validar_token(token)
  ids <- as.character(ids)

  segura <- purrr::possibly(get_serie_bccr, otherwise = NULL, quiet = FALSE)
  res <- purrr::map(ids, function(id) segura(id, fecha_inicio, fecha_fin, token))
  names(res) <- ids

  fallo <- vapply(res, is.null, logical(1))
  if (any(fallo)) {
    warning(
      "No se pudieron descargar estos indicadores (revisa el codigo o tu ",
      "conexion): ", paste(ids[fallo], collapse = ", "),
      call. = FALSE
    )
  }
  if (all(fallo)) {
    stop("No se pudo descargar ningun indicador.", call. = FALSE)
  }

  dplyr::bind_rows(res[!fallo], .id = "id")
}

# datos = list(list(codigoIndicador, nombreIndicador,
#                   series = list(list(fecha, valorDatoPorPeriodo), ...)))
parsear_series <- function(datos) {
  vacio <- tibble::tibble(
    Serie = character(), Fecha = as.Date(character()), Valor = numeric()
  )
  if (length(datos) == 0) return(vacio)

  d <- datos[[1]]
  s <- d$series
  if (length(s) == 0) return(vacio)

  out <- tibble::tibble(
    Serie = valor_o_na(d$nombreIndicador),
    Fecha = como_fecha(vapply(s, function(x) as.character(valor_o_na(x$fecha)), character(1))),
    Valor = vapply(s, function(x) as.numeric(valor_o_na(x$valorDatoPorPeriodo, NA_real_)), numeric(1))
  )
  out[!is.na(out$Valor), ]
}
