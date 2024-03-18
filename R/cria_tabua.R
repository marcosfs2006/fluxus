

cria_tabua <- function(nome_tabua){
    # "nome_tabua" : string contendo o nome da tábua a ser utilizada
  # na documentação da função será informado
  # os possíveis nomes das tábuas.
  
  require("lifecontingencies")
  
  tabuas <- readRDS("data/tabuas.Rds")
  
  if(!is.element(nome_tabua, names(tabuas))) stop("Tábua não cadastrada.")
  
  probs2lifetable(probs = na.omit(tabuas[[nome_tabua]]),
                  type = "qx",
                  radix = 100000,
                  name = nome_tabua) 
}







