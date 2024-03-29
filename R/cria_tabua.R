#' Cria tábua biométrica
#'
#' Os possíveis nomes de tábuas a serem fornecidos ao argumento `qx`
#' pode ser consultado com a função [mostra_tabuas_cadastradas()]
#'
#' @param qx String contendo o nome da tábua a ser utilizada
#' obtidas com a função [calcula_multidecremento()] ou um vetor
#' numérico contendo probabilidades multidecrementais, caso
#' em que o argumento `md` (multidecremento) deve ser `TRUE`
#'     
#' @param md Indica se o argumento `qx` está recebendo um
#'  vetor de probabilidades multidecrementalis ou não.
#'  Se `FALSE` (opção _default_) o argumento recebe seus
#'  valores de uma tábua biométrica já cadastrada no pacote. Se `TRUE`
#'  o argumento é um vetor numérico de probabilidades
#'   multidecrementais obtidas com a função [calcula_multidecremento()].
#'
#' @return Um objeto da classe `lifetable` do pacote `lifecontingencies`
#' 
#' @export
#'
#' @examples
#' tabuaAT49Masculina <- cria_tabua("AT49M")
#' summary(tabuaAT49Masculina)
cria_tabua <- function(qx, md = FALSE){

  if(!md){ 
  
  if(!is.element(qx, names(tabuas))) stop("Tabua nao cadastrada.") 
  
  lifecontingencies::probs2lifetable(probs = stats::na.omit(tabuas[[qx]]),
                                     type = "qx",
                                     radix = 100000,
                                     name = qx) 
  } else { # se for falso, cria uma tabua a partir dos qxT fornecidos calculados de forma multidecremental.
    
  lifecontingencies::probs2lifetable(probs = stats::na.omit(qx),
                                     type = "qx",
                                     radix = 100000,
                                     name = "Tabua de Multiplo Decremento")  
    
  }
  
}







