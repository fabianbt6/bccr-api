# Funciones internas para hablar con la API SDDE del BCCR.
# Documentacion oficial: "Estandar_API_SDDE.pdf" en el sitio del BCCR.

bccr_base_url <- "https://apim.bccr.fi.cr/SDDE/api/Bccr.GE.SDDE.Publico.Indicadores.API"

# Arma una solicitud autenticada con timeouts y reintentos.
bccr_request <- function(ruta, token, ...) {
  httr2::request(paste0(bccr_base_url, ruta)) |>
    httr2::req_url_query(..., idioma = "es") |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_options(connecttimeout = 30) |>
    httr2::req_timeout(120) |>
    httr2::req_retry(max_tries = 3, retry_on_failure = TRUE)
}

# Ejecuta la solicitud y devuelve el campo `datos` del JSON
# ({estado, mensaje, datos}).
bccr_json <- function(req) {
  json <- req |>
    httr2::req_headers(Accept = "application/json") |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  if (isFALSE(json$estado)) {
    stop("La API del BCCR respondio: ", json$mensaje, call. = FALSE)
  }
  json$datos
}

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

# NULL -> NA, para campos que la API a veces omite.
valor_o_na <- function(x, tipo = NA_character_) {
  if (is.null(x)) tipo else x
}

# Fechas que pueden venir como "2024-12-01" o "2024-12-01T00:00:00".
como_fecha <- function(x) {
  as.Date(substr(x, 1, 10))
}
