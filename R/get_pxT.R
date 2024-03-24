


get_pxT <- function(id, idade,  sexo,  tmdm, tmdf){ 
  
  #  id - vetor numerico de 1 a N sendo N o comprimento do fluxo atuarial. O MPS usa N = 150 
  #  idade - vetor da base cadastral contendo as idades dos servidores
  #  sexo - vetor da base cadastral contendo o sexo dos servidores
  #  tmdm e tmdf - tábuas de multiplos decrementos para os sexos masculino e feminino.

  data.frame(sexo, idade) %>% 
    mutate(pxT = case_when(sexo == "F" ~ pxt(tmdf, x = idade, t = id),
                           sexo == "M" ~ pxt(tmdm, x = idade, t = id))) %>% 
    select(pxT) 
  
}









