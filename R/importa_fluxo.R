#' Importa o fluxo atuarial
#' 
#' A função importa os fluxos atuariais no formato `.csv`
#' conforme encaminhados ao Ministério da Previdência Social - MPS
#' no modelo disponibilizado.
#'
#' @param arquivo Arquivo contendo o fluxo atuarial no formato `.csv`
#'
#' @return `data frame` contendo o fluxo atuarial
#' 
#' @export
#'
#' 
importa_fluxo <- function(arquivo){

  fluxo <- readr::read_csv2(arquivo,
                            skip = 5,
                            col_names = FALSE,
                            show_col_types = FALSE,
                            n_max = 150,
                            locale=readr::locale(decimal_mark = ",",
                                                 grouping_mark = "."),
                            col_types = readr::cols())

  colfluxo <- c(100101, 100201, 100301, 100401, 109001, 111000, 111101, 111201, 111301,
                111401, 112000, 119900, 121000, 121100, 121200, 121300, 121400, 121500,
                121600, 121700, 122000, 122100, 122200, 122300, 122400, 122500, 122600,
                122700, 123000, 123100, 123200, 123300, 123400, 124000, 124100, 124200,
                124300, 124400, 124500, 129000, 130101, 130201, 139901, 190000, 210000,
                211001, 212001, 213001, 214001, 215001, 219901, 220000, 221000, 222000,
                223000, 224000, 225000, 226000, 227000, 229000, 239901, 240000, 250001,
                260001, 270001, 280001, 290001)


    names(fluxo) <- paste("V", colfluxo, sep="")

    fluxo %>%
      dplyr::mutate(
        # Recálculo de algumas variáveis.
        V111000 = V111101 + V111201 + V111301 + V111401,
        V121000 = V121100 + V121200 + V121300 + V121400 + V121500 + V121600 + V121700,
        V122000 = V122100 + V122200 + V122300 + V122400 + V122500 + V122600 + V122700,
        V123000 = V123100 + V123200 + V123300 + V123400,
        V124000 = V124100 + V124200 + V124300 + V124400 + V124500,
        V190000 = V111000 + V121000 + V122000 + V123000 + V124000,   # TOTAL DAS RECEITAS

        V210000 = V211001 + V212001 + V213001 + V214001 + V215001 + V219901,
        V220000 = V221000 + V222000 + V223000 + V224000 + V225000 + V226000 + V227000 + V229000 + V239901,
        V240000 = V210000 + V220000                                 # TOTAL DAS DESPESAS
      )

}

