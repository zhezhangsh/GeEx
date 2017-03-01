tabPanel(
  "Meta-analysis",
  
  # h2("Meta-analysis of differential expression"),
  
  conditionalPanel(
    condition = 'input[["de.result.index"]] == "" ',
    list(h4("No results; run individual DE comparisons first to obtain results."))
  ),
  
  conditionalPanel(
    condition = 'input["de.result.index"] != ""',
    
    h4(HTML("Run a meta-analysis using results from individual DE comparisons.")),
    
    fluidRow(
      column(
        5,
        wellPanel(
          style='min-height: 300px; padding-top: 0px',
          
          h3(HTML("<b>Parameters</b>")), 
          
          div(style="display: inline-block; width: 100px", h6(HTML("P values"))),
          div(style="display: inline-block; width: 100px", selectizeInput('demeta.method.p', NULL, as.list(CombinePvalueMethods()))), 
          div(style="display: inline-block; width: 5px", h6(HTML(""))),
          div(style="display: inline-block; width: 30%; vertical-align: middle", htmlOutput('demeta.msg.p')), br(),
          
          div(style="display: inline-block; width: 100px", h6(HTML("Mean differences"))),
          div(style="display: inline-block; width: 100px", selectizeInput('demeta.method.m', NULL, c("MD", "SMD", "ROM"))),
          div(style="display: inline-block; width: 5px", h6(HTML(""))),
          div(style="display: inline-block; width: 30%; vertical-align: middle", htmlOutput('demeta.msg.m')), br(),
          
          checkboxInput('demeta.select.all', 'Use all available comparisons', TRUE),
          
          conditionalPanel(
            condition = 'input["demeta.select.all"] == false',
            h5(HTML(geex.html.msg("Highlight rows to select comparisons for meta-analysis:"))),
            DT::dataTableOutput('demeta.comparisons', width='100%')
          ),
          
          div(style="display: inline-block; ", actionButton('demeta.run', 'Run analysis', icon=icon('paper-plane'), class='dB')),
          div(style="display: inline-block; width: 5px;", h5(HTML(""))),
          div(style="display: inline-block; width: 50%; vertical-align: middle", htmlOutput('demeta.run.msg'))
        )
      ),
      column(
        7,
        conditionalPanel(
          condition = 'input["demeta1"] != false', 
          wellPanel(
            style='min-height: 120px; padding-top: 0px',
            
            div(style="display: inline-block;", h3(HTML("<b>Results</b>"))),
            div(style="display: inline-block; width: 25px", h5(HTML(""))),
            div(style="display: inline-block;", h6(HTML("Test statistic:"))),
            div(style="display: inline-block; width: 5px", h5(HTML(""))),
            div(style="display: inline-block;", radioButtons(
              'demeta.result.option', NULL, choices=c('P value' = 1, 'Mean difference' = 2), selected= 1, inline = TRUE)), 
            div(style="display: inline-block; width: 10px", h6(HTML(""))),
            div(
              style="display: inline-block;",
              div(style="display: inline-block;", h6(HTML(">="))),
              div(style="display: inline-block;", selectizeInput('demeta.result.n', NULL, choices=as.character(1:2), width='50px')),
              div(style="display: inline-block;", h6(HTML("comparison(s)")))
            ),
            
            htmlOutput('demeta.result.msg'),
            
            DT::dataTableOutput('demeta.result.table', width='100%'),
            
            div(style="display: inline-block;", downloadLink('demeta.download.table',  label = '[Download table]')),
            div(style="display: inline-block; width: 5px", h5(HTML(""))),
            div(style="display: inline-block;", downloadLink('demeta.download.column', label = '[Download column description]'))
          )            
        )
      )
    )
  )
)