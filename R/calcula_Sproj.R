#' Calcula o salário projetado
#' 
#' Função para realizar a projeção dos salários de contribuição
#'
#' Os valores de `pxT` passados à função são obtidos com a função
#' [calcula_multidecremento()]
#' 
#' Esta função é usada internamente pela função `vasf()`
#'
#' @param pxT Probabilidades de sobrevivência multidecremental. Vetor numérico.
#' @param Sx Salário de contribuição do servidor na data focal da avaliação atuarial.
#' @param cs Taxa real de crescimento salarial. Vetor numérico.
#' @param t Tempo de diferimento da projeção. 
#'
#' @return Salários projetados para o período t 
#' 
#' 
calcula_Sproj <- function(pxT, Sx, cs, t){
 
  pxT * 13 * Sx * (1 + cs)^t

}

