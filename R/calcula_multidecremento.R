
# Função para calcular a px multidecremental considerando até 4 decrementos

calcula_multidecremento <- function(qx=NULL, ix=NULL, tx=NULL, rx=NULL, retorno=c("qxT", "pxT")) { 
  
  # qx - probabilidade de morte
  # ix - probabilidade de invalidar
  # tx - probabilidade de sair do plano de benefícios (turnover)
  # rx - probabilidade de se aposentar antes do previsto (retirement)
  
  # usualmente utiliza-se apenas os decrementos morte e invalidez
  
  # premissas: deve existir obrigatoriamente no mínimo os decrementos de  mortalidade e invalidez
  #            se existir três decrementos serão sempre mortalidade, invalidez e rotação
  #            
  
  # usei as fórmulas apresentads em José Angelo.
  # e considerei também o capítulo 4 de Daniel Wedan
  
  teste_qx <- is.null(qx)
  teste_ix <- is.null(ix)
  teste_tx <- is.null(tx)
  teste_rx <- is.null(rx)
  
  if(teste_qx) stop("Decremento mortalidade é obrigatório.")
  if(teste_ix) stop("O decremento de invalidez também.")
  
  
  if(all(c(teste_tx, teste_rx))) {  # se ambos são nulos é bi 
  
  # bi decremental
  qx2 <-  qx * (1 - 1/2 * ix)
  ix2 <-  ix * (1 - 1/2 * qx)
  
  qxT <- qx2 + ix2
  pxT <- 1 - qxT  
  
  retorno <- match.arg(retorno)
  switch(retorno,
         qxT = return(qxT),
         pxT = return(pxT))
  
  
  } else if(all(c(!teste_tx, !teste_rx))) {  # se os dois últimos são não nulos então é penta
  
  # penta decremental
  qx4 <-  qx * (1 - 1/2 * (ix + tx + rx) + 1/3 * (ix * tx + ix * rx + tx * rx) - 1/4 * (ix * tx * rx))
  ix4 <-  ix * (1 - 1/2 * (qx + tx + rx) + 1/3 * (qx * tx + qx * rx + tx * rx) - 1/4 * (qx * tx * rx))
  tx4 <-  tx * (1 - 1/2 * (qx + ix + rx) + 1/3 * (qx * ix + qx * rx + ix * rx) - 1/4 * (qx * ix * rx))
  rx4 <-  rx * (1 - 1/2 * (qx + ix + tx) + 1/3 * (qx * ix + qx * tx + ix * tx) - 1/4 * (qx * ix * tx))
  
  qxT <- qx4 + ix4 + tx4 + rx4
  pxT <- 1 - qxT
  
  retorno <- match.arg(retorno)  
  switch(retorno,
         qxT = return(qxT),
         pxT = return(pxT))
  
  } else { # se não cai nas duas opções acima, só resta essa.

  # tri decremental
  qx3 <-  qx * (1 - 1/2 * (ix + tx) + 1/3 * ix * tx)
  ix3 <-  ix * (1 - 1/2 * (qx + tx) + 1/3 * qx * tx)
  tx3 <-  tx * (1 - 1/2 * (qx + ix) + 1/3 * qx * ix)
  
  qxT <-  qx3 + ix3 + tx3
  pxt <-  1 - qxT
  
  retorno <- match.arg(retorno)
  switch(retorno,
         qxT = return(qxT),
         pxT = return(pxT))
  } 

}







