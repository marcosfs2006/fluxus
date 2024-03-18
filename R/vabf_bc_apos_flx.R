

### função para o cálculo do VABF_BC
###---------------------------------

vabf_bc <- function(Bx,
                    inflacao = 0,
                    i,
                    cb,
                    idade,
                    invalidez=NULL,
                    regra,
                    sexo,
                    tmg,
                    tmi=NULL,
                    flx=1:150){
  
  # Assume-se que a base de dados já vem com todas as colunas necessárias tratadas
  
  # Coisas a serem implementadas:
  #
  # 1 - não incluída taxa de crescimento do benefício. Isso vai acontecer se o aposentado
  #     recebe benefício por integralidade ou pela média.
  
  
  
  
  # Bx - valor da base contribuitiva. já deve ter sido calculada na base de dados
  # inflação - inflação de longo prazo. usada para compor o fator de capacidade
  # i - taxa de juros. formato percentual 
  # cb - taxa real de crescimento dos benefícios
  # idade - vetor numérico da base de dados cadastral contendo as idades dos aposentados
  # invalidez - vetor binário (1-inválido, 0-valido)  da base de dados cadastral contendo indicação se o aposentado é invalido ou não 
  # regra - vetor binário (1 - integralidade, 0 - média) indicando a regra de benefício do aposentado.
  # sexo - vetor binário (1 - masculino, 0 - feminino) da base de dados cadastral indicando o sexo do aposentado
  # tmg - vetor de caracteres indicando os nomes das tábuas de mortalidade geral. masculina e feminina exemplo:  'tmg = c("IBGE2022M", "IBGE2022F")'
  # tmi - vetor de caracteres indicando os nomes das tábuas de mortalidade de inválidos, masculina e feminina.
  # flx - tamanho do fluxo - o Ministério da Previdência usa um fluxo de 150 anos
  
  
  # Criação das tábuas de mortalidade geral (tmg)
  qx_msc <- cria_tabua(tmg[str_detect(tmg, "M")]) %>% 
    cria_comutacao(juros = i)
  
  qx_fem <- cria_tabua(tmg[str_detect(tmg, "F")]) %>% 
    cria_comutacao(juros = i)
  
  
  # criar a tábua de mortalidade de invalidos (tmi), caso necessário
  if(!is.null(tmi)){
    
    ix_msc <- cria_tabua(tmi[str_detect(tmi, "M")]) %>%
      cria_comutacao(juros = i)
    
    ix_fem <- cria_tabua(tmi[str_detect(tmi, "F")]) %>%
      cria_comutacao(juros = i)  
  }

   Ex <- map(flx, \(x) get_Ex(id=x,
                              idade = idade,
                              sexo  = sexo,
                              invalidez = invalidez,
                              tqm = qx_msc,
                              tqf = qx_fem,
                              tim = ix_msc,       # no caso de não existir invalidos????
                              tif = ix_fem)) %>%
    bind_cols() %>% 
    rename_with(.fn = \(x) str_c("ano_", str_remove(x, "Ex\\.\\.\\."))) %>% 
    suppressMessages()
  
  
  fc <- calcula_fator_capacidade(inflacao = inflacao)  
  
  
  # isto será usado para calcular a capitalização do benefício dentro do mutate
  # usando a posicão da coluna como expoente.
  # como tem 3 colunas antes dos Ex, é necessário compensar.
  
  expoente <- as.list(1:ncol(Ex)) %>% setNames(names(Ex))
  
  Ex %>%
    mutate(id = 1:length(idade),
           Bx = Bx,
           REGRA_APOS = regra) %>%
    relocate(id, Bx, REGRA_APOS) %>%
    mutate(across(starts_with("ano"),  ~.x * 13 * Bx * (1 + cb)^(expoente[[cur_column()]] - 3) * if_else(REGRA_APOS == "INTEGRALIDADE", 1, fc))) %>% 
    select(id, starts_with("ano"))
}





