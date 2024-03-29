#' Cria tábua de comutação
#' 
#' A função cria um objeto da classe `actuarialtable` do pacote
#' `lifecontingencies`
#'
#' @param tabua Um objeto da classe `lifetable` do pacote `lifecontingencies` 
#'  criado com a função `cria_tabua()` 
#' @param juros Taxa de juros. Valor numérico em formato percentual.
#'
#' @return Um objeto da classe `actuarialtable` do pacote `lifecontingencies`
#' 
#' @export
#'
#' 
cria_comutacao <- function(tabua, juros){
  
  methods::new("actuarialtable",
      x = tabua@x,
      lx = tabua@lx,
      interest = juros/100,
      name = tabua@name)
}

