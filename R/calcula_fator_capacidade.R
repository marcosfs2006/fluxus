calcula_fator_capacidade <- function(inflacao){
  
  # inflação - valor numérico indicando a taxa de inflação de longo prazo. fornecida em formato percentual
  
  inflacao <- inflacao / 100
  ifelse(inflacao == 0, 1, ((1-((1 + inflacao)^(1/12))^-12) / (1-((1 + inflacao)^(1/12))^-1)) / 12 )
  
} 

