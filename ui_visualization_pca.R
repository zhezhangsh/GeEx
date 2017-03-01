tabPanel(
  "Principal components analysis",
  
  h2("Principal Components Analysis"),
  h5(HTML(paste(
    'Unsupervised clustering of samples in the same data set by',
    '<a href="https://en.wikipedia.org/wiki/Principal_component_analysis" target="_blank">PCA</a>'))),
  
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
          div(style="display: inline-block; width: 60%;", selectizeInput("pca.dataset", NULL, NULL)), br(),
          div(style="display: inline-block; width: 125px; vertical-align: top", h5(HTML("<b>Select groups:</b>"))),
          div(style="display: inline-block; width: 60%;", selectizeInput("pca.group", NULL, NULL, multiple=TRUE)), br(),
          div(style="display: inline-block; width: 125px", h5(HTML("<b>Select PCs:</b>"))),
          div(style="display: inline-block;", selectizeInput("pca.x", NULL, NULL, width='90px')),
          div(style="display: inline-block;", h5(HTML("&nbsp&nbsp&nbsp&nbsp"))),
          div(style="display: inline-block;", selectizeInput("pca.y", NULL, NULL, width='90px')), br(),
          div(style="display: inline-block; width: 125px", h5(HTML("<b>Select colors:</b>"))),
          div(style="display: inline-block;", selectizeInput("pca.color", NULL, GetColorTypes(), width='150px')), br(),
          
          div(style="display: inline-block;", checkboxInput('pca.sample', HTML(geex.html.msg('Highlight sample(s)')))),
          conditionalPanel(
            condition = 'input[["pca.sample"]] == true',
            DT::dataTableOutput('pca.table'),
            geex.clear.button('pca.clear')
          ), br(),
          
          div(
            style="display: inline-block; padding: 15px",
            geex.geneset.ui("pca.geneset.filter", 'Re-run PCA using known gene set(s)', 'pca.geneset.source',
                            'pca.geneset.coll', 'pca.geneset.species', 'pca.geneset.table', 'pca.geneset.clear')
          )
        )
      ),
      column(
        7,
        actionButton('pca.plot.button', 'Create plot', icon=icon('area-chart'), class='dB'), 
        plotlyOutput('pca.plot', height="600px")
      )
    )
  )
)