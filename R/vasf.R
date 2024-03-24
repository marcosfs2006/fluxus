


vasf <- function(Sx,
                 idade,
                 idade_prev_apos,
                 sexo,
                 cs,
                 tmdm,
                 tmdf,
                 flx=1:150){
  
  # Sx - Salário de contribuição na idade x
  # idade - coluna da base de dados contendo as idades dos servidores
  # idade_prev_apos - idade prevista para aposentadoria conforme regras do plano.
  # sexo - coluna da base de dados contendo o sexo das pessoas
  # cs - taxa de crescimento salarial
  # tmdm - tábua de mortalidade com probabilidades multidecrementais para o sexo masculino
  # tmdf - tabua de mortalidade com probabilidades multidecrementais para o sexo feminino
  
  # calcula 150 colunas contendo os valores de pxt
  pxT <- map(flx, \(x) get_pxT(id=x,
                               idade = idade,
                               sexo  = sexo,
                               tmdm  = tmdm,
                               tmdf  = tmdf)) %>%
    bind_cols() %>%
    rename_with(.fn = \(x) str_c("ano_", str_remove(x, "pxT\\.\\.\\."))) %>%
    suppressMessages()

  
  expoente <- as.list(1:ncol(pxT)) %>% setNames(names(pxT))
  
  
  pxT %>%
    mutate(id = 1:length(idade),
           idade = idade,
           idade_prev_apos = idade_prev_apos,
           Sx = Sx) %>%
    relocate(id, Sx, idade, idade_prev_apos) %>%
    mutate(across(starts_with("ano"), \(x) calcula_Sproj(pxT=x, Sx=Sx, cs=0.01, t=(expoente[[cur_column()]]))),
           across(starts_with("ano"), \(x) if_else((idade_prev_apos - (expoente[[cur_column()]] + idade)) <= 0, 0, x))) %>% # essa parte é só para 'aparar' o fluxo...
    select(id, starts_with("ano"))
  
}

