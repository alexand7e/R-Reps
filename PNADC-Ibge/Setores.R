padroes <- list(
  # list(padrao = "^(2012|2013|20517|10660|21220|283)$", segmento = "Insumos"),
  list(padrao = "^(20)$", segmento = "Insumos"),
  list(padrao = "^(011|012|013|014|02|015|017|03)$", segmento = "Primário"),
  list(padrao = "^(101|102|105|107|193|108|103|104|109|11|12|1311|1312|1321|1322|14|1510|1529|1531|16|17|3101)$", segmento = "Agroindústria"),
  list(padrao = "^(46|47|49|50|51|52|53|55|56|58|59|60|61|62|63|64|65|66|68|69|70|71|72|73|74|75|77|78|79|80|81|82|84)$", segmento = "Agrosserviços")
)

classificar_cnae <- function(cnae) {
  if (is.na(cnae)) {
    return("Outro")
  }
  
  cnae_prefixo <- substr(cnae, 1, 2)
  cnae_completo <- substr(cnae, 1, 6)
  
  # Tratamento especial para "106 exceto 10660"
  if(cnae_prefixo == "106" && cnae_completo != "10660") {
    return("Agroindústria")
  }
  for (padrao in padroes) {
    if (grepl(padrao$padrao, cnae_prefixo) || grepl(padrao$padrao, cnae_completo)) {
      return(padrao$segmento)
    }
  }
  
  return("Outro")
}

classificar_cnae("103")

