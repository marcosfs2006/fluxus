# função auxiliar. utilizada pela função vasf()
# calcula o salário projetado considerando as probabilidades multidecrementais 
# de um indivídio de idade x atingir a idade x + t

calcula_Sproj <- function(pxT, Sx, cs, t){
  
  # pxT - probabilidade multidecremental de um indivíduo de idade x chegar à idade x + t 
  # Sx - salário do servidor na data da avaliação atuarial
  # cs - taxa de crescimento salarial
  # t - tempo de diferimento
  
  pxT * 13 * Sx * (1 + cs)^t

}

