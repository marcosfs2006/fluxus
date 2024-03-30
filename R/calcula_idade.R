#' Calcula a quantidade de anos entre duas datas
#'
#' A função calcula a quantidade de anos ente duas datas.
#' A data mais antiga deve ser fornecida como primeiro
#' argumento e a data mais recente como o segundo argumento.
#'
#'
#'
#'@param dt_ini Data inicial. Pode ser uma string no formato "dd/mm/yyyy" ou 
#' um vetor que represente data (objeto da classe `Date` ou `POSIX`)
#'
#'@param dt_fim Data final. Pode ser uma string no formato "dd/mm/yyyy" ou
#' um vetor que represente data (objeto da classe `Date` ou `POSIX`)
#' 
#' @return Um vetor com a quantidade de anos entre as datas fornecidas.
#'
#' @export
calcula_idade <- function(dt_ini, dt_fim){
  
  # Função auxiliar para calcular a quantidade de anos entre duas datas. 
  
  # dt_focal - Data focal da avaliação. String no formato "dd/mm/yyyy" ou objeto da classe Date ou POSIXct, POSIXt
  # dt_nasc - data de nascimento do beneficário num formato de data seja ele qual for.
  
  if(is.character(dt_ini)){
    dt_ini <- as.Date(dt_ini, "%d/%m/%Y")
  } else {
    dt_ini <- as.Date(dt_ini)
  }
  
  if(is.character(dt_fim)){
    dt_fim <- as.Date(dt_fim, "%d/%m/%Y")
  } else {
    dt_fim <- as.Date(dt_fim)
  }

  
  # dt_ini <- ifelse(is.character(dt_ini), as.Date(dt_ini, "%d/%m/%Y"), as.Date(dt_ini))
  # dt_fim <- ifelse(is.character(dt_fim), as.Date(dt_fim, "%d/%m/%Y"), as.Date(dt_fim))
  
   as.integer(floor(as.numeric(dt_fim - dt_ini) / 365.25))
   
   #as.integer(floor(difftime(dt_focal, dt_nasc, units = "days") / 365.25))
}
