
library(tidyverse)

plota_fluxo <- function(fluxo, tipo="rm"){


# prepara a base de dados
 qtd_atuarial <- fluxo %>%
   mutate( ano  = V100201,
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
   select(-starts_with("V"))

# elabora o gráfico
ggplot(qtd_atuarial, aes(x = ano, y = .data[[tipo]])) +
  geom_point(size=2, color="blue") +
  geom_line(color="blue") +
  labs(title = str_c("Evolução da quantidade ", tipo),
       y = "",
       x = "") +
  theme_bw()

}



