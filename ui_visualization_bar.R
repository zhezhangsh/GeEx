tabPanel(
  "Single gene expression",
  
  h2("Expression of individual gene(s)"),
  h5(HTML("Make bar plot of individual genes to compare them to other genes and between groups")),
  
  conditionalPanel(
    condition = 'input[["meta.options"]]==""', 
    hr(),
    list(h3(msg.nocollection))
  ),
  
  conditionalPanel(
    condition = 'input["meta.options"]!= ""',
    fluidRow(
      column(
        5,
        wellPanel(
          div(style="display: inline-block; width: 125px; vertical-align: top", h5(HTML("<b>Select data set:</b>"))),
          div(style="display: inline-block; width: 60%", selectizeInput("bar.dataset", NULL, NULL)), br(),
          div(style="display: inline-block; width: 125px; vertical-align: top", h5(HTML("<b>Select groups:</b>"))),
          div(style="display: inline-block; width: 60%",
              selectizeInput("bar.group", NULL, NULL, multiple=TRUE),
              checkboxInput("bar.mean", HTML(geex.html.msg("Use group means")), value = TRUE)), br(),
          div(style="display: inline-block; width: 125px; height: 36px", h5(HTML("<b>Select scale:</b>"))),
          div(style="display: inline-block; height: 36px;", selectizeInput("bar.scale", NULL, c('logged', 'unlogged', 'percentile'), width='150px')), br(),
          div(style="display: inline-block; width: 125px; height: 36px", h5(HTML("<b>Select colors:</b>"))),
          div(style="display: inline-block; height: 36px;", selectizeInput("bar.color", NULL, GetColorTypes(), width='150px')), br(),
  
          h5(HTML(geex.html.msg("Highlight row(s) to select one or more genes"))),
          DT::dataTableOutput('bar.table'),
          geex.clear.button('bar.clear'),
          
          checkboxInput('lookup.option', HTML(geex.html.msg('Look up gene')), value = FALSE),
          conditionalPanel(
            condition = 'input[["lookup.option"]] == true',
            div(style="display: inline-block; vertical-align: top", 
                div(style="display: inline-block; width: 125px", h5(HTML("<b>Enter keyword:</b>"))),
                div(style="display: inline-block", textInput("bar.lookup.key", "", '', width='100px')), br(),
                div(style="display: inline-block; width: 125px", h5(HTML("<b>Select species:</b>"))),
                div(style="display: inline-block", selectizeInput("bar.lookup.species", NULL, NULL, width='100px'))
            ),
            div(style="display: inline-block; width: 200px", DT::dataTableOutput('bar.lookup.table'))
          )
        )
      ),
      column(
        7,
        actionButton('bar.plot.button', 'Create plot', icon=icon('area-chart'), class='dB'), 
        htmlOutput("bar.message"),
        plotlyOutput('bar.plot', height="400px"),
        DT::dataTableOutput('bar.stat', width='95%')
      )
    )
  )
) # end of tabPanel
