#' Obtém os valores presentes de quantidades atuarias
#'
#' A função calcula o valor presente atuarial das quantidades
#' atuariais desejadas. Até o presente momento estão disponíveis
#' as seguintes quantidades: `vasf`, `vabf`, `vacf`, `vabf_bc`
#' `vabf_bac`, `vacf_bc`, `vacf_bac`, `rm`, `rm_bc`, `rm_bac`,
#' `ag` e `ra`
#'
#' @param fluxo `data frame` contendo o fluxo atuarial
#' 
#' @param tipo Vetor de string indicando quais quantidades atuariais devem
#'  ser retornadas. Se `NULL` (opção _default_) retorna todas as quantidades
#'  atuariais disponíveis.
#'
#' @return Uma lista com os valores presentes das quantidades
#'  atuariais ou um vetor numérico unitário com o valor atual do tipo informado.
#'  
#' @export
#'
#'
extrai_resultados <- function(fluxo, tipo = NULL){

  tipos_validos <- c("vasf", "vabf", "vacf", "vabf_bc", "vabf_bac", "vacf_bc",
                     "vacf_bac", "rm_bc", "rm_bac", "ag", "ra", "rm")

  if(!is.null(tipo) && !all(is.element(tipo, tipos_validos))) stop("Tipo invalido.")
  
  #if(length(tipo) > 1) stop("Forneça apenas um tipo.")
  #if(!is.element(tipo, tipos_validos) | !is.null(tipo)) stop("Forneça um tipo válido.")
  
  
    # retorna uma lista com as quantidades
  

    vasf <-  with(fluxo, sum(V109001 * V100401))
    vabf <-  fluxo %>% dplyr::summarise(dplyr::across(c(V210000, V220000), ~ sum(.x * V100401))) %>% sum()
    vacf <-  fluxo %>% dplyr::summarise(dplyr::across(c(V111000, V112000, V119900, V121000, V122000,
                                         V123000, V124000, V129000, V130101, V130201, V139901),
                                       ~ sum(.x * V100401))) %>% sum()
    vabf_bc  <-  with(fluxo, sum(V210000 * V100401))
    vabf_bac <-  with(fluxo, sum(V220000 * V100401))
    vacf_bc  <-  fluxo %>% dplyr::summarise(dplyr::across(c(V111000, V112000, V119900),  ~ sum(.x * V100401))) %>% sum()
    vacf_bac <-  fluxo %>% dplyr::summarise(dplyr::across(c(V121000, V122000, V123000, V124000, V129000), ~ sum(.x * V100401))) %>% sum()
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
    return(resultados[tipo])
  }

}


