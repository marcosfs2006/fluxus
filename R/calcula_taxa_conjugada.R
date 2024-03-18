calcula_taxa_conjugada <- function(tx_juros, tx_incremento){
  
  # combina a taxa de juros com a taxa de crescimento do benefício (ou salário??? no caso de VABF BaC)
  
  tx_juros <- tx_juros / 100
  tx_incremento <- tx_incremento / 100
  
  ((1 + tx_juros) / (1 + tx_incremento)) - 1
  
}
