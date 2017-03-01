tabPanel(
  "Compare samples", 
  
  h2("Compare two samples/groups"),
  h5(HTML("Compare data of all or selected genes between two samples or groups.")),
  
  conditionalPanel(
    condition = 'input[["meta.options"]]==""', 
    hr(),
    list(h3(msg.nocollection))
  ),
  
  conditionalPanel(
    condition = 'input["meta.options"]!=""',
    fluidRow(
      column(
        5,
        wellPanel(
          div(style="display: inline-block; width: 115px; vertical-align: top", h6(HTML("<b><u>Select data set X:</u></b>"))),
          div(style="display: inline-block; width: 60%", selectizeInput("x.dataset", NULL, NULL)), br(),
          div(style="display: inline-block; width: 115px; vertical-align: top", h6(HTML("<b>Select group X:</b>"))),
          div(style="display: inline-block; width: 60%", selectizeInput("x.group", NULL, NULL)), br(),
          div(style="display: inline-block; width: 115px; vertical-align: top", h6(HTML("<b>Select sample X:</b>"))),
          div(style="display: inline-block; width: 60%", selectizeInput("x.sample", NULL, NULL)), br(),
          
          div(style="display: inline-block; width: 115px; vertical-align: top", h6(HTML("<b><u>Select data set Y:</u></b>"))),
          div(style="display: inline-block; width: 60%", selectizeInput("y.dataset", NULL, NULL)), br(),
          div(style="display: inline-block; width: 115px; vertical-align: top", h6(HTML("<b>Select group Y:</b>"))),
          div(style="display: inline-block; width: 60%", selectizeInput("y.group", NULL, NULL)), br(),
          div(style="display: inline-block; width: 115px; vertical-align: top", h6(HTML("<b>Select sample Y:</b>"))),
          div(style="display: inline-block; width: 60%", selectizeInput("y.sample", NULL, NULL)), br(),
          
          div(style="display: inline-block; width: 115px; vertical-align: top", h5(HTML("<b>Select plot:</b>"))),
          div(style="display: inline-block;", selectizeInput("two.type", NULL, PlotlyPairDiffTypes(), width='150px')), br(),
          div(style="display: inline-block; width: 115px; vertical-align: top", h5(HTML("<b>Select scale:</b>"))),
          div(style="display: inline-block;", selectizeInput("two.scale", NULL, c('logged', 'unlogged', 'percentile'), width='150px')), br(),
          
          div(
            style="display: inline-block; padding: 15px",
            geex.geneset.ui('two.filter.geneset', 'Highlight gene set(s)', 'two.source', 'two.coll', 
                            'two.species','two.table.geneset', 'two.clear.geneset', hei = 0)
          )
          
          # checkboxInput("two.filter.gene", HTML(geex.html.msg("Highlight gene(s)")), value=FALSE), 
          # conditionalPanel( 
          #   condition = 'input["two.filter.gene"] == true',
          #   DT::dataTableOutput('two.table.gene', width='100%'),
          #   actionButton("two.clear.gene", 'Clear selection', style="font-size:80%; height: 30px")
          # )
        )
      ), 
      column(
        7,
        actionButton('two.plot.button', 'Create plot', icon=icon('area-chart'), class='dB'), 
        htmlOutput('two.message'),
        plotlyOutput('two.plot', width='95%', height='500px'),
        DT::dataTableOutput('two.geneset.stat', width='95%')
      )
    )
  )
) # end of tabPanel
