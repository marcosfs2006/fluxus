

# retorna uma lista com as quantidades

extrai_resultados <- function(fluxo, tipo = NULL){

  if(!require(dplyr)) stop("É necessário o pacote dplyr.")

  #tipos_validos <- c("vasf", "vabf", "vacf", "vabf_bc", "vabf_bac", "vacf_bc",
  #                   "vacf_bac", "rm_bc", "rm_bac", "ag", "ra", rm)


  #if(length(tipo) > 1) stop("Forneça apenas um tipo.")
  #if(!is.element(tipo, tipos_validos) | !is.null(tipo)) stop("Forneça um tipo válido.")

    vasf <-  with(fluxo, sum(V109001 * V100401))
    vabf <-  fluxo %>% summarise(across(c(V210000, V220000), ~ sum(.x * V100401))) %>% sum()
    vacf <-  fluxo %>% summarise(across(c(V111000, V112000, V119900, V121000, V122000,
                                         V123000, V124000, V129000, V130101, V130201, V139901),
                                       ~ sum(.x * V100401))) %>% sum()
    vabf_bc  <-  with(fluxo, sum(V210000 * V100401))
    vabf_bac <-  with(fluxo, sum(V220000 * V100401))
    vacf_bc  <-  fluxo %>% summarise(across(c(V111000, V112000, V119900),  ~ sum(.x * V100401))) %>% sum()
    vacf_bac <-  fluxo %>% summarise(across(c(V121000, V122000, V123000, V124000, V129000), ~ sum(.x * V100401))) %>% sum()
    rm_bc    <-  vabf_bc - vacf_bc
    rm_bac   <-  vabf_bac - vacf_bac
    ag       <-   fluxo$V290001[1] - fluxo$V250001[1] - fluxo$V280001[1]   # ativo garantidor
    ra       <-   ag + vacf - vabf   # resultado atuarial
    rm       <-  vabf - vacf

  resultados <- list("vasf" = vasf,
                     "vabf" = vabf,
                     "vacf" = vacf,
                     "vabf_bc" = vabf_bc,
                     "vabf_bac" =  vabf_bac,
                     "vacf_bc" = vacf_bc,
                     "vacf_bac" = vacf_bac,
                     "rm_bc" = rm_bc,
                     "rm_bac" = rm_bac,
                     "ag" = ag,
                     "ra" = ra,
                     "rm" = rm)

  if(is.null(tipo)){
    return(resultados)
  } else {
    return(resultados[[tipo]])
  }

}


