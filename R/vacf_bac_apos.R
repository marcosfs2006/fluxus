#' Calcula o fluxo atuarial das contribuições futuras
#' 
#' A função calcula, para cada aposentado na base de dados, o fluxo futuro relativo
#' às receitas de contribuição. 
#'
#' @param cx Valor da contribuição realizada. Vetor numérico oriundo da base cadastral. 
#' @param inflacao Inflação de longo prazo. Usado para o calculo do fator de capacidade.
#'  Valor numérico no formato percentual. Premissa atuarial. 
#' @param juros Taxa de juros. Valor numérico em formato percentual. Premissa atuarial.
#' @param cb Taxa real de crescimento dos benefícios. Valor numérico em formato perncentual. Premissa atuarial.
#' @param idade Idade do aposentado na data focal. Vetor numérico oriundo da base cadastral.
#' @param invalidez Variável indicativa quanto a condição do aposentado em relação
#'  à invalidez. Se válido ou inválido. Vetor numérico oriundo da base cadastral.  
#' @param regra Variável indicativa da regra de aposentadoria do aposentado. Se integralidade
#'  e paridade ou média. Vetor numérico oriundo da base cadastral.
#' @param sexo Sexo do aposentado. Vetor numérico oriundo da base cadastral.
#' @param tmg Vetor de caracteres indicando os nomes das tábuas de mortalidade a
#'  serem utilizadas para os sexos masculino e feminino. Exemplo: `tmg = c("IBGE2022M", "IBGE2022F")`. Premissa atuarial.
#' @param tmi Vetor de caracteres indicando os nomes das tábuas de invalidez a
#'  serem utilizadas para os sexos masculino e feminino. Exemplo: `tmi = "ALVARO.VINDAS"`. Premissa atuarial.
#' @param flx Intervalo de 1 a N para o período de projeção. Por _default_ N = 150 
#'  para atender ao modelo de fluxo atuarial do Ministério da Previdência social - MPS.
#'
#' @return `data frame` contendo as projeções das contribuições para cada aposentado
#' @export
#'
#' 
vacf_bac_apos <- function(cx, # valor da contribuição obtido a partir do sx multiplicado pela alíquota de contribuição normal calculada por cn_ien()
                          idade_x,           
                          idade_r, 
                          sexo,
                          cs,
                          tmdm,
                          tmdf,
                          flx=1:150){
  
  # a projeção é de x até r. ambiente multidecremental.
  
  cs <- cs / 100 
  
  # calcula 150 colunas contendo os valores de pxt
  pxT <- purrr::map(flx, \(x) get_pxT(id=x,
                                      idade = idade_x,
                                      sexo  = sexo,
                                      tmdm  = tmdm,
                                      tmdf  = tmdf)) %>%
    dplyr::bind_cols() %>%
    dplyr::rename_with(.fn = \(x) stringr::str_c("ano_", stringr::str_remove(x, "pxT\\.\\.\\."))) %>%
    suppressMessages()
  
  
  expoente <- as.list(1:ncol(pxT)) %>% stats::setNames(names(pxT))
  
  
  pxT %>%
    dplyr::mutate(id = 1:length(idade),
                  idade_x = idade_x,
                  idade_r = idade_r,
                  cx = cx) %>%
    dplyr::relocate(id, cx, idade_x, idade_r) %>%
    dplyr::mutate(dplyr::across(tidyselect::starts_with("ano"), \(x) calcula_Sproj(pxT=x, cx=cx, cs=cs, t=(expoente[[dplyr::cur_column()]]))),
                  dplyr::across(tidyselect::starts_with("ano"), \(x) dplyr::if_else((idade_r - (expoente[[dplyr::cur_column()]] + idade_x)) <= 0, 0, x))) %>% # essa parte é só para 'aparar' o fluxo...
    dplyr::select(id, tidyselect::starts_with("ano"))

}