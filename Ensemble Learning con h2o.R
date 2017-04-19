
is.installed <- function(mypkg) { is.element(mypkg, installed.packages()[,1]) }#creo funcion que se fija si me dice si mi paquete est? instalado o no

if (is.installed("h2o") == FALSE) {install.packages("h2o")} #si openxlsx no est? instalado hago que me lo instale automaticamente



library(devtools) ## cargo el paquete devtools para poder instalar el paquete h2oEnsemble desde github

install_github("h2oai/h2o-3/h2o-r/ensemble/h2oEnsemble-package") ## instalo el paquete h2oEnsemble, para poder hacer super ensemble. Lo instalo desde github