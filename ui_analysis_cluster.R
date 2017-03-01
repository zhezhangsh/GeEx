tabPanel(
  "Gene clustering",
  
  h2("Gene clustering"),
  h5(HTML("Identify clusters of co-expressed genes in selected data set.")),
  
  conditionalPanel(
    condition = 'input[["cl.dataset"]] == "" ',
    hr(),
    list(h3(msg.nocollection))
  ),
  
  conditionalPanel(
    condition = 'input[["cl.dataset"]] != "" ',

    fluidRow(
      column(
        5,
        wellPanel(
          style='padding-top: 0px; min-height: 120px',

          h3(HTML("<b>Parameters</b>")),

          conditionalPanel(
            condition = 'input.cluster1 == true',
            div(style="display: inline-block; width: 100px; height: 45px; vertical-align: top", h4(HTML("<b><u>Data set:</u></b>"))),
            div(style="display: inline-block; width: 68%; height: 30px; vertical-align: top", selectizeInput("cl.dataset", NULL, NULL)), br(),
            
            h5(HTML("<b><u>Select top genes (%):</u></b>")),
            div(style="display: inline-block; width: 100px; vertical-align: top", h6(HTML("Mean expression"))),
            div(style="display: inline-block; width: 65%; height: 50px", sliderInput("cl.top.mean", NULL, 0, 100, 25, 1)),
            div(style="display: inline-block; width: 100px; vertical-align: top;", h6(HTML("Total variance"))),
            div(style="display: inline-block; width: 65%; height: 50px", sliderInput("cl.top.sd0", NULL, 0, 100, 60, 1)),
            div(style="display: inline-block; width: 100px; vertical-align: top", h6(HTML("Between-group variance"))),
            div(style="display: inline-block; width: 65%; height: 50px", sliderInput("cl.top.sd1", NULL, 0, 100, 60, 1)),
            
            h5(HTML("<b><u>Clustering options:</u></b>")),
            div(style="display: inline-block; width: 100px; vertical-align: top", h6(HTML("Minimal number of genes"))),
            div(style="display: inline-block; width: 65%; height: 50px", sliderInput("cl.option.size", NULL, 2, 100, 10, 1)),
            div(style="display: inline-block; width: 100px; vertical-align: top", h6(HTML("Minimal correlation"))),
            div(style="display: inline-block; width: 65%; height: 50px", sliderInput("cl.option.corr", NULL, 0, 1, 0.6, 0.01)),
            
            h5(HTML("<b><u>Re-clustering options:</u></b>")),
            div(style="display: inline-block; width: 100px; vertical-align: top", h6(HTML("Number of iterations"))),
            div(style="display: inline-block; width: 65%; height: 50px", sliderInput("cl.re.time", NULL, 0, 100, 0, 1)),
            div(style="display: inline-block; width: 100px; vertical-align: top", h6(HTML("Correlation to centroid"))),
            div(style="display: inline-block; width: 65%; height: 50px", sliderInput("cl.re.corr", NULL, 0, 1, 0.6, 0.01))
          ), br(),
          
          div(style="display: inline-block; width:120px", actionButton('cl.run', 'Run analysis', icon=icon('paper-plane'), class='dB')),
          div(style="display: inline-block; width: 1%;", h5(HTML(""))),
          div(style="display: inline-block; width: 55%; vertical-align: middle", htmlOutput('cl.run.msg'))
        )
      ),
      column(
        7,
        conditionalPanel(
          condition = 'input[["cl.select.table"]] != "" ',

          wellPanel(
            style='padding-top: 0px; min-height: 660px',

            div(style="display: inline-block;", h3(HTML("<b>Results</b>"))),
            div(style="display: inline-block; width: 20px", h5(HTML(""))),

            div(style="display: inline-block;", h5(HTML("Select table:"))),
            div(style="display: inline-block; width: 5px", h5(HTML(""))),
            div(style="display: inline-block;", radioButtons("cl.option.table", NULL, list("Summary"=1, "Cluster"=2), inline=TRUE)),
            div(style="display: inline-block; width: 3px", h5(HTML(""))),
            div(style="display: inline-block; width: 24%", selectizeInput("cl.select.table", NULL, NULL)),

            DT::dataTableOutput('cl.result.table', width='100%'),

            div(style="display: inline-block;", downloadLink('cl.download.summary',  label = '[Download summary table]')),
            div(style="display: inline-block; width: 5px", h5(HTML(""))),
            div(style="display: inline-block;", downloadLink('cl.download.gene', label = '[Download clustered genes]')), 

            checkboxInput('cl.show.plot', 'Show plot'),

            conditionalPanel(
              condition = 'input[["cl.show.plot"]]',
              plotlyOutput("cl.plot", width = '95%', height = '360px')
            )
          )
        )
      )
    )
  ),
  
  fluidRow(
    conditionalPanel(
      condition = "2 < 1", 
      checkboxInput('cluster1', 'cluster1', TRUE)
    )
  )
) 