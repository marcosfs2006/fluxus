#' Calcula o fluxo das projeções atuariais futuras dos salários de contribuição 
#'
#' A função faz a projeção de salários futuros para uma quantidade especificada
#' de anos que vai do ano da avaliação até o ano anterior ao da elebibilidade ao
#' benefício. A projeção considera a probabilidade multidecremental.
#'      
#'
#' @param Sx Salário de contribuição na data focal da avaliação. Vetor numérico oriundo da base cadastral. 
#' @param idade Idade dos servidores. Vetor numérico oriundo da base cadastral.
#' @param idade_prev_apos Idade prevista para aposentadoria. Vetor numérico oriundo da base cadastral.
#' @param sexo Sexo do servidor. Vetor numérico oriundo da base cadastral.
#' @param cs Percentual relativo à taxa real de crescimento salarial. Premissa atuarial.
#' @param tmdm Tábua biométrica multidecremental para o sexo masculino. Premissa atuarial.
#' @param tmdf Tábua biométrica multidecremental para o sexo feminino.  Premissa atuarial.
#' @param flx Intervalo de 1 a N para o período de projeção. Por _default_ N = 150 
#'  para atender ao modelo de fluxo atuarial do Ministério da Previdência social - MPS.
#'
#' @return `data frame` contendo as projeções salariais para cada servidor
#' @export
#'
#' 
vasf <- function(Sx,
                 idade,
                 idade_prev_apos,
                 sexo,
                 cs,
                 tmdm,
                 tmdf,
                 flx=1:150){
  
   #cs <- cs / 100 
  
  # calcula 150 colunas contendo os valores de pxt
  pxT <- purrr::map(flx, \(x) get_pxT(id=x,
                                      idade = idade,
                                      sexo  = sexo,
                                      tmdm  = tmdm,
                                      tmdf  = tmdf)) %>%
    dplyr::bind_cols() %>%
    dplyr::rename_with(.fn = \(x) stringr::str_c("ano_", stringr::str_remove(x, "pxT\\.\\.\\."))) %>%
    suppressMessages()

  
  expoente <- as.list(1:ncol(pxT)) %>% stats::setNames(names(pxT))
  
  
  pxT %>%
    dplyr::mutate(id = 1:length(idade),
                  idade = idade,
                  idade_prev_apos = idade_prev_apos,
                  Sx = Sx) %>%
    dplyr::relocate(id, Sx, idade, idade_prev_apos) %>%
    dplyr::mutate(dplyr::across(tidyselect::starts_with("ano"), \(x) calcula_Sproj(pxT=x, Sx=Sx, cs=cs, t=(expoente[[dplyr::cur_column()]]))),
                  dplyr::across(tidyselect::starts_with("ano"), \(x) dplyr::if_else((idade_prev_apos - (expoente[[dplyr::cur_column()]] + idade)) <= 0, 0, x))) %>% # essa parte é só para 'aparar' o fluxo...
    dplyr::select(id, tidyselect::starts_with("ano"))
  
}

