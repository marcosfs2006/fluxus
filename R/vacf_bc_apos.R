#' Calcula o fluxo atuarial das contribuições futuras
#' 
#' A função calcula, para cada aposentado na base de dados, o fluxo futuro relativo
#' às receitas de contribuição. 
#'
#' @param Cx Valor da contribuição realizada. Vetor numérico oriundo da base cadastral. 
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
vacf_bc_apos <- function(Cx,
                    inflacao = 0,
                    juros,
                    cb=0,
                    idade,
                    invalidez=NULL,
                    regra,
                    sexo,
                    tmg,
                    tmi=invalidez,
                    flx=1:150){
  
  # Assume-se que a base de dados já vem com todas as colunas necessárias tratadas
  # CX deve considerar o teto do RGPS 
  
  
  # Coisas a serem implementadas:
  #
  # 1 - não incluída taxa de crescimento do benefício. Isso vai acontecer se o aposentado
  #     recebe benefício por integralidade ou pela média.
  # 2. Em vez de receber a taxa de inflação, oferecer a possibilidade de já informar o fator de capacidade.
  # 3. Acertar a função para o caso de não utilizar o indicativo de invalidez.
  # 4. Incluir testes para as variáveis. Em especial as nominais que devem entrar com valores numéricos:
  #    sexo, invalidez e regra. Criar uma função para alterar os valores na base cadastral. 'ajusta_valores()'     
 
  
  # Criação das tábuas de mortalidade geral (tmg)
  qx_msc <- cria_tabua(tmg[stringr::str_detect(tmg, "M")]) %>% 
    cria_comutacao(juros = juros)
  
  qx_fem <- cria_tabua(tmg[stringr::str_detect(tmg, "F")]) %>% 
    cria_comutacao(juros = juros)
  
  
  # criar a tábua de mortalidade de invalidos (tmi), caso necessário
  if(!is.null(invalidez)){
    
    if(stringr::str_detect(tmi, "(M|F)$")){
      
      ix_msc <- cria_tabua(tmi[stringr::str_detect(tmi, "M")]) %>%
        cria_comutacao(juros = juros)
      
      ix_fem <- cria_tabua(tmi[stringr::str_detect(tmi, "F")]) %>%
        cria_comutacao(juros = juros)
      
    } else {
      
      # Para o caso de ser uma tábua única para homens e mulheres
      ix_msc <- ix_fem <- cria_tabua(tmi) %>%
        cria_comutacao(juros = juros)
      
    }
    
  } else {
    
    ix_msc <- NULL
    ix_fem <- NULL
    
  }
  
  Ex <- purrr::map(flx, \(x) get_Ex(id=x,
                                    idade = idade,
                                    sexo  = sexo,
                                    invalidez = invalidez,
                                    tqm = qx_msc,
                                    tqf = qx_fem,
                                    tim = ix_msc,       
                                    tif = ix_fem)) %>%
    dplyr::bind_cols() %>% 
    dplyr::rename_with(.fn = \(x) stringr::str_c("ano_", stringr::str_remove(x, "Ex\\.\\.\\."))) %>% 
    suppressMessages()
  
  
  fc <- calcula_fator_capacidade(inflacao = inflacao)  
  
  expoente <- as.list(1:ncol(Ex)) %>% stats::setNames(names(Ex))
  
  Ex %>%
    dplyr::mutate(id = 1:length(idade),
                  Cx = Cx,
                  REGRA_APOS = regra) %>%
    dplyr::relocate(id, Cx, REGRA_APOS) %>%
    dplyr::mutate(dplyr::across(tidyselect::starts_with("ano"),  ~.x * 13 * Cx * (1 + cb)^(expoente[[dplyr::cur_column()]] - 3) * dplyr::if_else(REGRA_APOS == "INTEGRALIDADE", 1, fc))) %>% 
    dplyr::select(id, tidyselect::starts_with("ano"))
}


