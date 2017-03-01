navbarMenu(
  "Analysis",

  source('ui_analysis_coex.R', local=TRUE)$value,
  source('ui_analysis_cluster.R', local=TRUE)$value,
  source('ui_analysis_de.R', local=TRUE)$value
  # source('ui_analysis_demeta.R', local=TRUE)$value
) 