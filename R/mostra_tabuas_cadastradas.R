#' Mostra nomes das tábuas cadastradas no pacote
#' 
#' Esta função mostra os nomes das tábuas biométricas
#' atualmente cadastradas no pacote. Esses nomes devem
#' ser fornecidos às funções.
#'
#' @return Um `data frame` contendo os nomes das tábuas já 
#'  cadastradas no pacote e informações adicionais sobre as mesmas.
#' @export
#'
#' @examples
#' mostra_tabuas_cadastradas()
mostra_tabuas_cadastradas <- function(){
  names(tabuas[,-1])
} 
