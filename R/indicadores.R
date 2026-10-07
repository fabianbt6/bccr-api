#' Indicadores por defecto
#'
#' Lista de codigos de indicadores del BCCR que se descargan si no se
#' indica otra cosa en [descargar_bccr()].
#'
#' @return Vector de caracteres con codigos de indicadores.
#' @export
#' @examples
#' indicadores_default()
indicadores_default <- function() {
  c(
    "317",   # Tipo de cambio de compra
    "87031",
    "87703",
    "87961",
    "3541",  # TPM diaria
    "423",   # TBP diaria
    "89638", # Inflacion
    "23630", # Desempleo
    "38208", # Exportaciones FOB regimen definitivo
    "94967", # IMAE construccion
    "97473", # PIB construccion (variacion interanual)
    "23999", # Cuenta corriente, balanza de pagos
    "96993", # PIB nominal en USD
    "25243", # Cuenta corriente
    "3044",  # Reservas internacionales netas
    "94931", # IMAE serie original
    "94990"  # IMAE regimen especial
  )
}
