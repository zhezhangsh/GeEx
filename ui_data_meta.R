tabPanel(
  "Metadata", 
  h2("Metadata"),
  h5(HTML('Information about data sets, sample groups, samples, and genes.')),
  
  conditionalPanel(
    condition = 'input[["meta.options"]] == ""',
    hr(),
    list(h3(msg.nocollection))
  ),
  
  conditionalPanel(
    condition = 'input["meta.options"] != ""',
    wellPanel(
      div(style="display: inline-block;", h5(HTML("<b>Select metadata table</b>&nbsp&nbsp"))),
      div(style="display: inline-block;", selectizeInput("meta.options", NULL, NULL, width='160px')),
      div(style="display: inline-block;", h5(HTML("&nbsp&nbsp&nbsp(highlight row to see more details)")))
    ),
    
    div(style="display: inline-block;", actionButton('meta.table.button', 'Create table', icon=icon('table'), class='dB')), 
    div(style="display: inline-block; width: 10px", h4('')),  
    div(style="display: inline-block;", downloadLink('meta.download', HTML('<u>Download table</u>'))),
    br(), 
    
    htmlOutput('meta.message'), 
    
    DT::dataTableOutput('meta.table'),
    
    htmlOutput('meta.detail')
  ) 
)
