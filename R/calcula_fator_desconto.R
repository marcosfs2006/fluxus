calcula_fator_desconto <- function(juros, id=1:150){
  
  # função para calcuar as taxas de desconto anuais
  # a partir da taxa de juros
  
  # colocar a versão completa que permite antecipada
  # e postecipada
  
  cumprod((1 + rep(juros / 100, 150))^-1)
}
