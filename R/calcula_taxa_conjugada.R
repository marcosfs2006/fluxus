#' Calcula taxa conjugada
#'
#' @param juros Taxa de juros. Valor numérico em formato percentual.
#' @param incremento Taxa de crescimento dos salários ou benefícios. Valor numérico em formato percentual.
#'
#' @return Valor da taxa conjugada.
#' @export
#'
#' @examples
#' calcula_taxa_conjugada(juros=4.5, incremento=2.5)
#' 
calcula_taxa_conjugada <- function(juros, incremento){

  juros <- juros / 100
  incremento <- incremento / 100
  
  ((1 + juros) / (1 + incremento)) - 1
  
}
