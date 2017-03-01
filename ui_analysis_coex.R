tabPanel(
  "Gene co-expression",
  
  h2("Gene co-expression"),
  h5(HTML("Find genes correlated to a selected gene across data sets.")),
  
  conditionalPanel(
    condition = 'input[["meta.options"]] == "" ',
    hr(),
    list(h3(msg.nocollection))
  ),
  
  conditionalPanel(
    condition = 'input[["meta.options"]] != "" ',
    
    fluidRow(
      column(
        5,
        wellPanel(
          style='padding-top: 0px; min-height: 120px',
          
          # div(style="display: inline-block; width: 24%; vertical-align: middle", h5(HTML("Search gene:"))),
          # div(style="display: inline-block; width: 32%;", textInput('coex.gene.text', NULL, 'Enter keyword ...')),
          # div(style="display: inline-block; width: 1%;", h5(HTML(''))),
          # div(style="display: inline-block; width: 10%", actionButton('coex.gene.button', '', icon=icon('search'))),
          # div(style="display: inline-block; width: 2%;", h5(HTML(''))),
          # div(style="display: inline-block; width: 25%; vertical-align: middle", checkboxInput('coex.gene.all', 'show all', TRUE)),
          
          h3(HTML("<b>Parameters</b>")),
          
          conditionalPanel(
            condition = 'input.coex3 == true',
            h5(HTML(geex.html.msg("Highlight row to select a gene"))), 
            DT::dataTableOutput('coex.gene.table', width='100%')
          ), 
          
          conditionalPanel(
            condition = 'input.coex1 == true',

            checkboxInput('coex.dataset.all', 'Use all available datasets', value = TRUE),
            
            conditionalPanel(
              condition = 'input["coex.dataset.all"]==false',
              
              h5(HTML(geex.html.msg('Highlight row(s) to select data set(s)'))),
              DT::dataTableOutput('coex.dataset.table', width='100%')
            )
          ), br(),
          
          div(style="display: inline-block;", actionButton('coex.run', 'Run analysis', icon=icon('paper-plane'), class='dB')),
          div(style="display: inline-block; width: 5px;", h5(HTML(""))),
          div(style="display: inline-block; ", htmlOutput('coex.run.msg'))
        )
      ),
      column(
        7,
        conditionalPanel(
          condition = 'input.coex2 != true',
          
          wellPanel(
            style='padding-top: 0px; min-height: 580px',
            
            h3(HTML("<b>Results</b>")),
            
            DT::dataTableOutput('coex.result', width='100%'),
            
            div(style="display: inline-block;", downloadLink('coex.download.result', label = HTML('<u>Download table</u>'))),
            div(style="display: inline-block; width: 10px"),
            div(style="display: inline-block;", downloadLink('coex.download.column', label = HTML('<u>Download column description</u>'))),
            div(style="display: inline-block; width: 10px"),
            div(style="display: inline-block;", downloadLink('coex.download.metadata', label = HTML('<u>Download metadata</u>')))
          )
        )
      )
    )
  ),
  
  fluidRow(
    conditionalPanel(
      condition = "2 < 1", 
      checkboxInput('coex1', 'coex1', FALSE),
      checkboxInput('coex2', 'coex2', FALSE),
      checkboxInput('coex3', 'coex3', TRUE)
    )
  )
)