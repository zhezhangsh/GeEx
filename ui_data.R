navbarMenu(
  "Data",
  
  source('ui_data_meta.R', local=TRUE)$value,
  source('ui_data_gene.R', local=TRUE)$value,
  source('ui_data_expr.R', local=TRUE)$value,
  source('ui_data_comb.R', local=TRUE)$value,
  
  br()
) 