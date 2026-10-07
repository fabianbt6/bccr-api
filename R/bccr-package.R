#' bccr: cliente no oficial para la API de indicadores economicos del BCCR
#'
#' Funciones principales:
#' * [descargar_bccr()]: descarga varios indicadores en una sola tabla.
#' * [get_serie_bccr()]: descarga un indicador.
#' * [guardar_bccr()] / [leer_bccr()]: guardan y leen los datos en parquet.
#'
#' La API key se lee de la variable de entorno `BCCR_API_KEY`. Configurala
#' una vez por maquina con `usethis::edit_r_environ()`, agregando la linea
#' `BCCR_API_KEY=<tu key>`, y reinicia R.
#'
#' @keywords internal
"_PACKAGE"
