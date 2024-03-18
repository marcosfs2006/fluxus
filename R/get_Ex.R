
get_Ex <- function(id, idade, sexo, invalidez=NULL, tqm, tqf, tim, tif){
  
  # dado um conjunto fixo de idades, sexo, status de invalidez calcula o nEx
  
  # todo: 
  # 2 - caso não haja indicador de invalidez nos dados???? 
  #     possivel solução: se invalidez não for preenchido executa uma rotina sem o calculo de invalidez
  
  
  # id - vetor numérico indicando o comprimento do fluxo atuarial - será sempre 'id=1:150' visto que esse é o tamanho do fluxo utilizado pelo MPS 
  # idade - vetor da base cadastral contendo as idades dos aposentados
  # sexo - vetor da base cadastral contendo o sexo dos aposentados
  # invalidez - vetor da base cadastral contendo a indicação de que aposentado é valido ou inválido (aposentadoria por invalidez) 
  # tqm, tqf, tim, tif - tabuas de mortalidade geral e de mortalidade de inválidos
  
  
  data.frame(idade, sexo, invalidez) %>%  
    mutate(Ex = case_when((invalidez == "V" & sexo == "F") ~ map_dbl(idade, \(k) Exn(tqf, x = k, n = id)),
                          (invalidez == "V" & sexo == "M") ~ map_dbl(idade, \(k) Exn(tqm, x = k, n = id)),
                          (invalidez == "I" & sexo == "F") ~ map_dbl(idade, \(k) Exn(tif, x = k, n = id)),
                          (invalidez == "I" & sexo == "M") ~ map_dbl(idade, \(k) Exn(tim, x = k, n = id)))) %>% 
    select(Ex) 
  
}






