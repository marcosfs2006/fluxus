#' Calcula a alíquota de contribuição normal anual
#'
#' A função calcula a alíquota de contribuição normal anual 
#' utilizando o método de financiamento atuarial Idade de
#' Entrada Normal, conforme definido no Art. 22 do Anexo VI
#' da Portaria n. 1467/2022 do Ministério da Previdência Social - MPS
#'
#'
#'@param dt_ini Data inicial. Pode ser uma string no formato "dd/mm/yyyy" ou 
#' um vetor que represente data (objeto da classe `Date` ou `POSIX`)
#'
#' @param sx salário mensal na data focal (x) da avaliação atuarial
#' @param inflacao valor da taxa de inflação de longo prazo em percentual
#' @param cs valor da taxa real de crescimento salarial em valor percentual
#' @param sexo sexo do servidor indicado no formato "M" e "F". 
#' @param idade_y idade em anos na data de ingresso do servidor no sistema previdenciário (y) 
#' @param idade_r idade em anos na data prevista de aposentadoria (r)
#' @param idade_x idade em anos na data focal (x)
#' @param tmgm_act tabua de comutação unidecremental masculina 
#' @param tmgf_act tabua de comutação unidecremental feminina
#' @param tmdm_ltb tabua de mortalidade multidecremental masculina  - taxa de juros
#' @param tmdf_ltb tabua de mortalidade multidecremental feminina   - taxa de juros
#' @param tmdm_act tabua de comutação masculina multidecremental - taxa de juros
#' @param tmdf_act tabua de comutação feminina multidecremental - taxa de juros
#' @param tmdm_txcjg_act tabua de comutação masculina multidecremental - taxa conjugada
#' @param tmdf_txcjg_act tabua de comutação feminina multidecremental - taxa conjugada
#' @param peso peso de ponderação das alíquotas individuais, podendo assumir um dos seguintes
#' valores: `sx`, `axn_xr`, `vasf_x1` ou `vasf_xr`
#' 
#' @return Valor numérico indicando a alíquota de contribuição anual
#'
#' @export
acn_ien <- function(sx,         
                   inflacao,   
                   cs,         
                   sexo,       
                   idade_y,    
                   idade_r,    
                   idade_x,    
                   tmgm_act,    
                   tmgf_act,  
                   tmdm_ltb,    
                   tmdf_ltb,   
                   tmdm_act,  
                   tmdf_act,   
                   tmdm_txcjg_act, 
                   tmdf_txcjg_act,  
                   peso=c("sx", "axn_xr", "vasf_x1", "vasf_xr")){


# Ajuste. Se o tempo de plano for 0 passar para 1
# para não dar problema mais adiante no cálculo de anuidades
tempo_xr <- idade_r - idade_x
tempo_yx <- idade_x - idade_y
tempo_yx[tempo_yx == 0] <- 1 
 
# reunião das colunas passadas como argumento em um data frame
# para manipulações posteriores
dados <- data.frame(sx, sexo, idade_y, idade_x, idade_r, tempo_xr, tempo_yx)


# Projeção dos salário de x para r para o cálculo do vabf_r
# Esse salário projetado será considerado o valor do benefício inicial quando o servidor aposentar
 dados <- dados %>% 
   dplyr::mutate(pxT_xr = dplyr::case_when(sexo == "M" ~ lifecontingencies::pxt(tmdm_ltb, x=idade_x, t=tempo_xy), 
                                           sexo == "F" ~ lifecontingencies::pxt(tmdf_ltb, x=idade_x, t=tempo_xy)),
                 sproj_xr = sx * (1 + cs / 100 )^tempo_xy * pxT_xr) 
 
 
# Cálculo do vabf_r  
dados <- dados %>% 
   dplyr::mutate(axn_r. = dplyr::case_when(sexo == "M" ~ lifecontingencies::axn(tmg_msc_atb, x=idade_r, k=13, payment = "due"),        # anuidade vitalícia antecipada - ambiente unidecremental
                                           sexo == "F" ~ lifecontingencies::axn(tmg_fem_atb, x=idade_r, k=13, payment = "due"))) %>%   # anuidade vitalícia 
   dplyr::rowwise() %>% 
   dplyr::mutate(vabf_r = 13 * sproj_xr * axn_r. * calcula_fator_capacidade(inflacao)) %>% # considero todo mundo sem integralidade rever isso depois
   dplyr::ungroup()
 
 
# Cálculo do vabf_y
dados <- dados %>% 
  dplyr::mutate(Exn_ry = dplyr::case_when(sexo == "M" ~ purrr::map2_dbl(.x = idade_r, .y = idade_y,  .f = \(r, y) lifecontingencies::Exn(tmdm_act, x = r, n = r - y)),
                                          sexo == "F" ~ purrr::map2_dbl(.x = idade_r, .y = idade_y,  .f = \(r, y) lifecontingencies::Exn(tmdf_act, x = r, n = r - y))),
         vabf_y = vabf_r * Exn_ry)


# Cálculo do vasf_y e alíquota normal individual

# projeção do salário na data focal para a data y (na verdade descontar...)
dados <- dados %>% 
  dplyr::mutate(s_y = sx * (1 + cs / 100)^(-1 * tempo_yx),
                axn_yr = dplyr::case_when(sexo == "M" ~ lifecontingencies::axn(tmdm_txcjg_act, x=idade_y, n=tempo_yx, payment = "immediate"),
                                          sexo == "F" ~ lifecontingencies::axn(tmdf_txcjg_act, x=idade_y, n=tempo_yx, payment = "immediate")),
                vasf_y = s_y * axn_yr,
                ani = vabf_y / vasf_y) 


# Cálculo dos pesos a serem utlilzados no cálculo da alíquota média
# Anuidades temporárias considerando a taxa conjugada
dados <- dados %>% 
  dplyr::mutate(axn_xr = dplyr::case_when(sexo == "M" ~ lifecontingencies::axn(tmdm_txcjg_act, x=idade_x, n=tempo_xy, payment = "immediate"), # antecipado
                                          sexo == "F" ~ lifecontingencies::axn(tmdf_txcjg_act, x=idade_x, n=tempo_xy, payment = "immediate")),
         
         axn_x1 = dplyr::case_when(sexo == "M" ~ lifecontingencies::axn(tmdm_txcjg_act, x=idade_x, n=1, payment="immediate"), 
                                   sexo == "F" ~ lifecontingencies::axn(tmdf_txcjg_act, x=idade_x, n=1, payment="immediate")),
         
         vasf_x1 = sx * axn_x1,
         vasf_xr = sx * axn_xr)


# Cálculo das alíquota médias para todo o grupo

switch(match.arg(peso),
  "sx"      = with(dados, weighted.mean(ani, sx)),
  "anx_xr"  = with(dados, weighted.mean(ani, axn_xr)),
  "vasf_x1" = with(dados, weighted.mean(ani, vasf_x1)),
  "vasf_xr" = with(dados, weighted.mean(ani, vasf_xr)))


}


