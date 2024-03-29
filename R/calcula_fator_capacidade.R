#' Calcula o fator de capacidade.
#'
#' Esta função calcula o 'fator de capacidade' a partir da taxa de inflação
#' anual fornecida à função.
#'
#'
#'
#'@param inflacao Taxa anual de inflação de longo prazo no formato percentual. Valor numérco.
#'
#' @return Valor do fator de capacidade correspondente à taxa de inflação informada.
#'
#' @export
#'
#' @examples
#' calcula_fator_capacidade(4.5)
calcula_fator_capacidade <- function(inflacao){
  
  inflacao <- inflacao / 100
    ifelse(inflacao == 0, 1, ((1-((1 + inflacao)^(1/12))^-12) / (1-((1 + inflacao)^(1/12))^-1)) / 12)
  
} 

