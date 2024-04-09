>**NOTA:** O pacote encontra-se em fase de desenvolvimento e testes. 

<br>

# fluxus
Pacote para análise e produção de fluxos atuariais dos RPPS.

<br>

Este pacote vem sendo desenvolvido como parte da monografia apresentada ao Instituto de Matemática da UFRJ
para o curso de [pós-grauação Especializaçao em Atuária](https://www.im.ufrj.br/index.php/pt/ensino/pos-graduacao/pos-graduacao-do-im/cursos-lato-sensu/especializacao-atuaria). 

### Instalação

Para instalação e carregamento do pacote, usar o código a seguir:

```
# install.packages("devtools")
devtools::install_github("marcosfs2006/fluxus")
library(fluxus)
```

# Objetivo geral do pacote 

O objetivo das funções até o momento implementadas, e do pacote
de forma geral, é fornecer um conjunto de funções que facilitem a
análise de fluxos atuariais dos RPPS e também permita a elaboração
dos fluxos atuariais ao menos para algumas rubricas.

Até o momento, poucas funções foram implmentadaspermitindo
que apenas um conjunto restrito de operações sejam realizadas.
Com o tempo objetiva-se aumentar o leque de funções de forma
que seja possível gerar a maioria das rubricas do fluxo 
atuarial dos RPPS conforme modelo disponibilizado pelo
Ministério da Previdência Social - MPS e também mais 
funções dedicadas à análise dos fluxos.

O pacote utiliza extensivamente funções dos pacotes
[`{lifecontingencies}`](https://cran.r-project.org/web/packages/lifecontingencies/index.html) e [`{tidyverse}`](https://www.tidyverse.org/) e até o momento
disponibiliza dois conjuntos de funções: um para auxiliar na **análise**
dos fluxos atuariais e outro para **gerar** o fluxo 
atuarial de algumas rubricas do modelo de fluxo
atuarial do MPS.

# Funções para análise do fluxo atuarial:

Para a *análise* do fluxo atuarial até o momento
já foram elaboradas as seguintes funções:

### `importa_fluxo()` 

Esta função faz a importação dos dados contidos nos
arquivos `csv` do fluxo atuarial. Para o seu correto
funcionamento é fundamental que o arquivo esteja
estritamente no formato disponibilizado e exigido
pelo MPS.

### `extrai_resultados()`

Esta função calcula o VPA (valor presente atuarial) de
algumas quantidades atuariais.

No momento, a função retorna o VPA das seguintes quantidades atuariais: 
`vasf`, `vabf`, `vacf`,`vabf_bc`,`vabf_bac`, `vacf_bc`, `vacf_bac`,
`rm_bc`, `rm_bac`, `ag`, `ra` e `rm`   


### `plota_fluxo()`

Esta função permite a elaboração de gráficos de linha mostrando
a evolução temporal da quantidade atuarial escolhida dentre
aquelas mencionadas no tópico anterior.

No momento a função só suporta a realização do gráfico
de apenas uma quantidade atuarial. Objetiva-se
aperfeiçoar a função para que possa "plotar" mais de
uma quantidade atual no mesmo gráfico facilitando a
comparação de fluxos.

Por ora essas são as três funções disponibilizadas pelo
pacote para auxiliar na análise do fluxo atuarial. As
funções ainda estão numa fase bem inicial de desenvolvimento
e serão aperfeiçoadas com o tempo à medida que forem sendo testadas,
bem como outras funções poderão vir a ser adicionadas.


# Funções para elaboração do fluxo atuarial:

Para a *elaboração* do fluxo atuarial, até o momento
já estão "prontas" as funções a seguir elencadas:


### `id_fluxo()` 

Esta função produz as quatro primeiras colunas do fluxo atuarial que 
correspondem às rubricas `100101 (Instante)`, `100201 (Ano)`,
`100301 (Taxa de Juros %)` e `100401 (Fator de Desconto)` 

### `vasf()`

Esta função produz o fluxo atuarial relativo à folha de
contribuições futuras. Corresponde à rubrica `109001
(Base de Cálculo da Contribuição Normal)`

### `vabf_bc_apos()`

Esta função produz o fluxo atuarial dos encargos de benefícios
futuros (benefício concedido) para o benefício de aposentadoria
programada. Corresponde à rubrica `211001 (Benefícios Concedidos - 
Encargos - Aposentadorias Programadas)`

### `vacf_bc_apos()` 

Esta função produz o fluxo atuarial das contribuições
futuras (benefício concedido) para o benefício de aposentadoria programada.
Corresponde à rubrica `100101 (Benefícios Concedidos - Contribuições
Futuras dos Aposentados - Aposentadorias Programadas)`  


## Funções auxiliares:

Além das funções principais, acima elencadas, que produzem
diretamente o fluxo atuarial, foram desenvolvidas funções
auxiliares que, ou produzem resultados que são fornecidos
às funções principais acima elencadas como argumentos, ou
são por elas utilizadas internamente e não são disponibilizadas
aos usuários.

Essas funções são as seguintes:

* `calcula_fator_capacidade()` - calcula o fator de capacidade   
* `calcula_multidecremento()` - calcula probabilidades multidecrementais   
* `calcula_taxa_conjugada()` - calcula taxa conjugada   
* `cria_comutacao()` - cria um objeto `actuarialtable` do pacote `{lifecontingencies}`   
* `cria_tabua()` - cria um objeto `lifetable` do pacote `{lifecontingencies}`   
* `Calcula_Sproj()` - chamada internamente - calcula projeções salariais   
* `get_Ex()` - chamada internamente - calcula fatores de desconto atuariais  
* `get_pxT()` - chamada internamente - calcula probabilidades de sobrevivência   


# Exemplos de Uso das Funções de Análise do Fluxo Atuarial

(em elaboração...)

