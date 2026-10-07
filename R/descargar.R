bccr_base_url <- "https://apim.bccr.fi.cr/SDDE"
bccr_endpoint <- "/api/Bccr.GE.SDDE.Publico.Indicadores.API/indicadoresEconomicos/"

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

  json <- httr2::request(paste0(bccr_base_url, bccr_endpoint, id, "/series")) |>
    httr2::req_url_query(
      fechaInicio = formatear_fecha(fecha_inicio),
      fechaFin = formatear_fecha(fecha_fin),
      idioma = "es"
    ) |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_headers(Accept = "application/json") |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  parsear_respuesta(json)
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

# --- Funciones internas -------------------------------------------------

validar_token <- function(token) {
  if (is.null(token) || identical(token, "")) {
    stop(
      "Falta la API key del BCCR (variable de entorno BCCR_API_KEY).\n",
      "1. Corre usethis::edit_r_environ()\n",
      "2. Agrega la linea BCCR_API_KEY=<tu key> y guarda el archivo\n",
      "3. Reinicia R",
      call. = FALSE
    )
  }
  token
}

formatear_fecha <- function(x) {
  format(as.Date(x), "%Y/%m/%d")
}

# La API devuelve una lista que, al aplanarla, tiene 4 campos de metadatos
# (el 4to es el nombre de la serie) seguidos de pares fecha / valor.
parsear_respuesta <- function(json) {
  v <- unlist(json, use.names = FALSE)
  serie <- v[4]
  v <- v[-(1:4)]
  n <- length(v) %/% 2

  if (n == 0) {
    return(tibble::tibble(
      Serie = character(), Fecha = as.Date(character()), Valor = numeric()
    ))
  }

  out <- tibble::tibble(
    Serie = serie,
    Fecha = lubridate::ymd(v[seq(1, by = 2, length.out = n)]),
    Valor = suppressWarnings(as.numeric(v[seq(2, by = 2, length.out = n)]))
  )
  out[!is.na(out$Valor), ]
}
