#' Cria colunas iniciais do fluxo atuarial
#'
#' @param dt_focal Data focal da avaliação atuarial no formato 'dd/mm/yyyy'. 
#' @param juros Taxa de juros 
#'
#' @return `data frame` contendo as colunas iniciais do fluxo atuarial.
#' @export
#'
#' @examples
#'  id_fluxo("31/12/2023", 4.93)
id_fluxo <- function(dt_focal, juros){
  
  # dt_focal - data focal da avaliação. string no formado "dd/mm/yyyy"
  # juros - taxa de juros a ser utilizada em percentual
  
  ano_avaliacao <- as.integer(format(as.Date(dt_focal, "%d/%m/%Y"), "%Y")) + 1
  
  data.frame(V100101 = 1:150,
             V100201 = ano_avaliacao:(ano_avaliacao + 149),
             V100301 = rep(juros, 150)) %>%
      dplyr::mutate(V100401 = round(cumprod((1 + V100301 / 100)^-1), 5))  # o fluxo é sempre antecipado
  
}



