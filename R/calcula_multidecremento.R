#' Calcula probabilidades multidecrementais
#'
#' Esta função calcula vetores de probabilidade considerando
#' dois ou mais decrementos. 
#'
#'@details
#' Devem ser informados, no mínimo, os decrementos de morte e invalidez. 
#' Se forem declarados três decrementos estes serão considerados morte, invalidez e rotatividade.
#'
#'
#' @param qx Vetor númerico contendo as probabilidades de morte. Argumento obrigatório.
#' @param ix Vetor numérico contendo as probabilidades de entrada em invalidez. Argumento obrigatório.
#' @param tx Vetor numérico contendo as probabilidades de saída do plano de benefícios (turnover/rotatividade).
#' @param rx Vetor numérico contendo as probabilidades de aposentadoria antecipada (retirement).
#' @param retorno String informando se devem ser retornadas as probabilidades multidecrementais de morte (`qxT`)
#' ou de sobrevivência (`pxT`)
#'
#' @return Um vetor númerico contendo as probabilidades multidecrementais.
#' @export
#'
#'                         
calcula_multidecremento <- function(qx=NULL, ix=NULL, tx=NULL, rx=NULL, retorno=c("qxT", "pxT")) { 

  teste_qx <- is.null(qx)
  teste_ix <- is.null(ix)
  teste_tx <- is.null(tx)
  teste_rx <- is.null(rx)
  
  if(teste_qx) stop("Decremento mortalidade e obrigatorio.")
  if(teste_ix) stop("O decremento de invalidez tambem.")
  
  if(all(c(teste_tx, teste_rx))) {  # se ambos são nulos é bi 
  
  # bi decremental
  qx2 <-  qx * (1 - 1/2 * ix)
  ix2 <-  ix * (1 - 1/2 * qx)
  
 
  switch(match.arg(retorno),
         "qxT" = qx2 + ix2,
         "pxT" = 1 - (qx2 + ix2))
  
  
  } else if(all(c(!teste_tx, !teste_rx))) {  # se os dois últimos são não nulos então é penta
  
  # penta decremental
  qx4 <-  qx * (1 - 1/2 * (ix + tx + rx) + 1/3 * (ix * tx + ix * rx + tx * rx) - 1/4 * (ix * tx * rx))
  ix4 <-  ix * (1 - 1/2 * (qx + tx + rx) + 1/3 * (qx * tx + qx * rx + tx * rx) - 1/4 * (qx * tx * rx))
  tx4 <-  tx * (1 - 1/2 * (qx + ix + rx) + 1/3 * (qx * ix + qx * rx + ix * rx) - 1/4 * (qx * ix * rx))
  rx4 <-  rx * (1 - 1/2 * (qx + ix + tx) + 1/3 * (qx * ix + qx * tx + ix * tx) - 1/4 * (qx * ix * tx))
  

  switch(match.arg(retorno),
         "qxT" = qx4 + ix4 + tx4 + rx4,
         "pxT" = 1 - (qx4 + ix4 + tx4 + rx4))
  
  } else { # se não cai nas duas opções acima, só resta essa.

  # tri decremental - com qx, ix e tx (assumo esses três)
  qx3 <-  qx * (1 - 1/2 * (ix + tx) + 1/3 * ix * tx)
  ix3 <-  ix * (1 - 1/2 * (qx + tx) + 1/3 * qx * tx)
  tx3 <-  tx * (1 - 1/2 * (qx + ix) + 1/3 * qx * ix)
  
  
  switch(match.arg(retorno),
         "qxT" = qx3 + ix3 + tx3,
         "pxT" = 1 - (qx3 + ix3 + tx3))
  } 

}







