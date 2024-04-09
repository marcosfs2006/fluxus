#' Calcula o fluxo atuarial dos benefícios futuros relativos aos 
#' servidores ativos - Benefícios a Conceder 
#' 
##' A função calcula, para cada servidor ativo na base de dados, o fluxo futuro relativo
#'  às despesas com o pagamento dos benefícios aos servidores ativos. 
#'
#' @param sx Valor do salário de contribuição na idade x. Vetor numérico oriundo da base cadastral.
#' @param inflacao Inflação de longo prazo. Usado para o calculo do fator de capacidade.
#'  Valor numérico no formato percentual. Premissa atuarial.
#' @param juros Taxa de juros. Valor numérico em formato percentual. Premissa atuarial.
#' @param cb Taxa real de crescimento dos benefícios. Valor numérico em formato perncentual. Premissa atuarial.
#' @param idade_x Idade do servidor na data focal. Vetor numérico oriundo da base cadastral.
#' @param idade_y Idade do servidor na data de ingresso no sistema previdenciário.
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
#' @return `data frame` contendo as projeções dos benefícios futuros para cada servidor ativo
#' @export
#'
#' 
vabf_bac_apos <- function(sx,
                          inflacao = 0,
                          juros,
                          cb = 0,
                          idade_x,
                          idade_y,
                          regra,
                          sexo,
                          tmg,
                          tmi,
                          tx=NULL,
                          flx=1:150){
  

# Criação das tábuas de mortalidade geral (tmg) - unidecremental
qx_msc_ltb <- cria_tabua(tmg[stringr::str_detect(tmg, "M")])
qx_fem_ltb <- cria_tabua(tmg[stringr::str_detect(tmg, "F")])
  
qx_msc_act <- qx_msc_ltb %>% cria_comutacao(juros = juros)
qx_fem_act <- qx_fem_ltb %>% cria_comutacao(juros = juros)
  
# Criação das tábuas de mortalidade multidecremental 
qxT_msc <-calcula_multidecremento(qx=tabuas[[tmg[stringr::str_detect(tmg, "M")]]],
                                  ix=tabuas[[tmi[stringr::str_detect(tmg, "M")]]],
                                  tx=tx) 
  
qxT_fem <-calcula_multidecremento(qx=tabuas[[tmg[stringr::str_detect(tmg, "F")]]],
                                  ix=tabuas[[tmi[stringr::str_detect(tmg, "F")]]],
                                  tx=tx) # se tx for NULL será que vai funcionar???? 
 
# cria tábua de multiplos decrementos   
tmdm_ltb <- cria_tabua(qxT_msc, md=TRUE) 
tmdf_ltb <- cria_tabua(qxT_fem, md=TRUE) 

# Projeção dos salários para r
dados <- data.frame(sx, sexo, idade_x, idade_y) %>% 
    dplyr::mutate(pxT_xr = dplyr::case_when(sexo == "M" ~ lifecontingencies::pxt(tmdm_ltb, x=idade_x, t=(idade_y - idade_x)), 
                                            sexo == "F" ~ lifecontingencies::pxt(tmdf_ltb, x=idade_x, t=(idade_y - idade_x))),
                  sproj_xr = sx * (1 + cs / 100 )^(idade_y - idade_x) * pxT_xr) 
  

  # ver essa função...
  Ex <- purrr::map(flx, \(x) get_Ex(id = x,
                                    idade = idade_x,
                                    sexo  = sexo,
                                    invalidez = NULL,
                                    tqm = qx_msc,
                                    tqf = qx_fem,
                                    tim = NULL,
                                    tif = NULL)) %>% 
    dplyr::bind_cols() %>% 
    dplyr::rename_with(.fn = \(x) stringr::str_c("ano_", stringr::str_remove(x, "Ex\\.\\.\\."))) %>% 
    suppressMessages()
  
  
  fc <- calcula_fator_capacidade(inflacao = inflacao)  
 
  expoente <- as.list(1:ncol(Ex)) %>% stats::setNames(names(Ex)) # confirmar uso da função cur_column() - necessidade de compensar coluanas
  
  Ex %>%
    dplyr::mutate(id = 1:length(idade_x),
                  Sx = dados$sproj_xr,
                  regra_apos = regra) %>%
    dplyr::relocate(id, Sx, regra_apos) %>%
    dplyr::mutate(dplyr::across(tidyselect::starts_with("ano"),  ~.x * 13 * Bx * (1 + cb)^(expoente[[dplyr::cur_column()]] - 3) * dplyr::if_else(regra_apos == "INTEGRALIDADE", 1, fc))) %>% 
    dplyr::select(id, tidyselect::starts_with("ano"))
}





