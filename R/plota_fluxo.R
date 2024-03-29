#' Elabora gráfico do fluxo atuarial da quantidade atuarial desejada.
#'
#' @param fluxo `data frame` contendo o fluxo atuarial
#' @param tipo quantidade atuarial cujo fluxo se deseja plotar. Opção _default_ `rm` (reserva matemática)
#'
#' @return Gráfico de linha com a evolução da quantidade atuarial especificada.
#' @export
#' @importFrom rlang .data
#' 
plota_fluxo <- function(fluxo, tipo="rm"){


# prepara a base de dados
 qtd_atuarial <- fluxo %>%
   dplyr::mutate( ano  = V100201,
           vasf = V109001,

           vabf     = V210000 + V220000,
           vabf_bc  = V210000,
           vabf_bac = V220000,

           vacf     = V111000 + V112000 + V119900 + V121000 + V122000,
                      V123000 + V124000 + V129000,
           vacf_bc  = V111000 + V112000 + V119900,
           vacf_bac = V121000 + V122000 + V123000 + V124000 + V129000,

           rm       = vabf - vacf,
           rm_bc    = vabf_bc - vabf_bc,
           rm_bac   = vabf_bac - vacf_bac,

           ag       = V290001,

           ra       = ag - rm) %>%
   dplyr::select(-tidyselect::matches("^V\\d{6}"))


# elabora o gráfico
ggplot2::ggplot(qtd_atuarial, ggplot2::aes(x = ano, y = .data[[tipo]])) +
  ggplot2::geom_point(size=2, color="blue") +
  ggplot2::geom_line(color="blue") +
  ggplot2::labs(title = stringr::str_c("Evolucao da quantidade ", tipo),
       y = "",
       x = "") +
  ggplot2::theme_bw()

}



