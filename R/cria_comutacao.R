cria_comutacao <- function(tabua, juros){
  
  # tabua - um objeto lifetable criado com a função 'cria tabua'
  # juros - taxa de juros anual a ser utilizada em formato percentual
  
  new("actuarialtable",
      x = tabua@x,
      lx = tabua@lx,
      interest = juros/100,
      name = tabua@name)
}

