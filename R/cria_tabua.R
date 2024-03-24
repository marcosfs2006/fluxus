

cria_tabua <- function(qx, md = FALSE){
  
  # "qx" : string contendo o nome da tábua a ser utilizada. Caso seja utilizado multidecremento, o vetor de probabilidade multidecremental obtido com a função calcula_qxT()
  #  na documentação da função será informado
  #  os possíveis nomes das tábuas.
  
  require("lifecontingencies")
  
  if(!md){ # de md for verdadeiro cria a tábua a partir das tábuas cadastradas...
  
  tabuas <- readRDS("data/tabuas.Rds")
  if(!is.element(nome_tabua, names(tabuas))) stop("Tábua não cadastrada.")
  probs2lifetable(probs = na.omit(tabuas[[nome_tabua]]),
                  type = "qx",
                  radix = 100000,
                  name = nome_tabua) 
  } else { # se for falso, cria uma tabua a partir dos qxT fornecidos calculados de forma multidecremental.
    
  probs2lifetable(probs = na.omit(qx),
                  type = "qx",
                  radix = 100000,
                  name = "Tábua de Múltiplo Decremento")  
    
  }
  
}







