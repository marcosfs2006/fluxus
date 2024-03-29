#' Calcula o fator de desconto atuarial (Exn)
#' 
#' Essa é uma função interna que tem por objetivo calcular
#' para cada beneficiário o desconto atuarial relativo a
#' cada período de tempo futuro do fluxo atuarial.
#' É utilizada internamente pelas funções [vabf_bc_apos()](vabf_bc_apos) e
#' [vacf_bc_apos()](vacf_bc_apos).
#'
#' @param id  Valor passado pela função mãe.
#' @param idade Valor passado pela função mãe.
#' @param sexo Valor passado pela função mãe.
#' @param invalidez Valor passado pela função mãe.
#' @param tqm Valor passado pela função mãe.
#' @param tqf Valor passado pela função mãe.
#' @param tim Valor passado pela função mãe.
#' @param tif Valor passado pela função mãe.
#'
#' @importFrom magrittr %>%
#'
#' @return `data frame` com os valores dos fatores de
#'  desconto atuarial para cada beneficiário e período
#'  do fluxo atuarial indicado em `id`.
#' 
#'
#' 
get_Ex <- function(id, idade, sexo, invalidez, tqm, tqf, tim, tif){
  
  # todo: 
  # 2 - caso não haja indicador de invalidez nos dados???? 
  #     possivel solução: criar na base cadastral uma coluna (status_validez) com a indicação de que todos os aposentados são válidos.
  #     com isso não é preciso mexer na função como fiz.
  

  if(is.null(invalidez)){ # caso onde não exista na base cadastral uma coluna de status de invalidez.
    
    Ex <- data.frame(idade, sexo) %>%  
      dplyr::mutate(Ex = dplyr::case_when(sexo == "F" ~ purrr::map_dbl(idade, \(k) lifecontingencies::Exn(tqf, x = k, n = id)),
                                          sexo == "M" ~ purrr::map_dbl(idade, \(k) lifecontingencies::Exn(tqm, x = k, n = id)))) %>% 
      dplyr::select(Ex) 

  } else { # caso onde existe na base cadastral uma coluna com o status de invalidez

    Ex <- data.frame(idade, sexo, invalidez) %>%  
      dplyr::mutate(Ex = dplyr::case_when((invalidez == "V" & sexo == "F") ~ purrr::map_dbl(idade, \(k) lifecontingencies::Exn(tqf, x = k, n = id)),
                                          (invalidez == "V" & sexo == "M") ~ purrr::map_dbl(idade, \(k) lifecontingencies::Exn(tqm, x = k, n = id)),
                                          (invalidez == "I" & sexo == "F") ~ purrr::map_dbl(idade, \(k) lifecontingencies::Exn(tif, x = k, n = id)),
                                          (invalidez == "I" & sexo == "M") ~ purrr::map_dbl(idade, \(k) lifecontingencies::Exn(tim, x = k, n = id)))) %>% 
    dplyr::select(Ex) 
  }

  Ex # retorna o Ex
  
}






