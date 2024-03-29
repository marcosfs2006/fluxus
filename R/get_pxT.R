#' Calcula probabilidades de sobrevivência multidecremental (pxT)
#'
#' Esta função é usada internamente pela função [vasf()] e 
#' calcula para cada servidor ativo a probabilidade multidecremental
#' tpxT de um servidor de idade `x`, sobreviver atá a idade `x+t`
#' considerando um ambiente multidecremental. 
#'
#' @param id Valor passado pela função mãe.
#' @param idade Valor passado pela função mãe.
#' @param sexo Valor passado pela função mãe.
#' @param tmdm Valor passado pela função mãe.
#' @param tmdf Valor passado pela função mãe.
#'
#' @return `data frame` com os valores das probabilides
#'  de sobrevivência multidecrementais para cada
#'   beneficiário e período do fluxo atuarial indicado em `id`.
#'   
#' 
#'
#' 
get_pxT <- function(id, idade,  sexo,  tmdm, tmdf){ 
 
  data.frame(sexo, idade) %>% 
    dplyr::mutate(pxT = dplyr::case_when(sexo == "F" ~ lifecontingencies::pxt(tmdf, x = idade, t = id),
                                         sexo == "M" ~ lifecontingencies::pxt(tmdm, x = idade, t = id))) %>% 
    dplyr::select(pxT) 
  
}









