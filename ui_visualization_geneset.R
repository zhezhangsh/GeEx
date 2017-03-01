tabPanel(
  "Gene set expression",
  
  h2("Expression pattern of gene set"),
  h5(HTML("Overall expression pattern of a gene set in sample groups.")),
  
  conditionalPanel(
    condition = 'input[["meta.options"]] == "" ', 
    hr(),
    list(h3(msg.nocollection))
  ),
  
  conditionalPanel(
    condition = 'input["meta.options"] != ""',
    fluidRow(
      column(
        5,
        wellPanel(
          div(style="display: inline-block; width: 125px; vertical-align: top", h5(HTML("<b>Select data set:</b>"))),
          div(style="display: inline-block; width: 60%;", selectizeInput("geneset.dataset", NULL, NULL)), br(),
          div(style="display: inline-block; width: 125px; vertical-align: top", h5(HTML("<b>Select groups:</b>"))),
          div(style="display: inline-block; width: 60%;", selectizeInput("geneset.group", NULL, NULL, multiple=TRUE)),
          div(style="display: inline-block; width: 125px", h5(HTML("<b>Select plot:</b>"))),
          div(style="display: inline-block; width: 150px; height: 36px;", selectizeInput("geneset.type", NULL, PlotMatrixGroupTypes())), br(),
          div(style="display: inline-block; width: 125px", h5(HTML("<b>Select colors:</b>"))),
          div(style="display: inline-block; width: 150px; height: 36px;", selectizeInput("geneset.color", NULL, GetColorTypes())), br(),
          div(style="display: inline-block; width: 125px; vertical-align: top", h5(HTML("<b>Select scale:</b>"))),
          div(style="display: inline-block; width: 150px; height: 48px", 
              selectizeInput("geneset.scale", NULL, c('logged', 'unlogged', 'percentile')),
              checkboxInput("geneset.normalize", HTML(geex.html.msg("Re-scale gene")), value = TRUE)), br(),
          
          div(style="display: inline-block;", h5(HTML("<b>Select gene set:</b>"))),
          fluidRow(
            column(1),
            column(
              10, 
              style = "padding: 0px", 
              geex.geneset.ui("", '', 'geneset.source', 'geneset.coll', 'geneset.species', 'geneset.table', 'geneset.clear', hei=40)
            ),
            column(1)
          )
        )
      ),
      
      column(
        7,
        actionButton('geneset.plot.button', 'Create plot', icon=icon('area-chart'), class='dB'), 
        htmlOutput("geneset.message"),
        plotlyOutput('geneset.plot', width='100%', height="640px"),
        DT::dataTableOutput('geneset.stat', width='95%')
      )
    )
  )
)