tabPanel(
  "Differential expression",
  
  h2("Differential gene expression"),
  # h5(HTML("Use selected statistical method to test gene-level differential expression.")),

  tabsetPanel( 
    type = 'pills', 
    tabPanel(
      "Run analysis", 
        conditionalPanel(
        condition = 'input[["meta.options"]] == "" ',
        hr(),
        list(h4(msg.nocollection))
      ),
      
      conditionalPanel(
        condition = 'input["meta.options"] != ""',
        h4(HTML("Define two-group comparison(s) and run DE analysis.")),
        
        fluidRow(
          column(
            6,
            wellPanel(
              style='min-height: 80px; padding-top: 0px',
              
              h3(HTML("Parameters")),
              
              conditionalPanel(
                condition = 'input.de1 == true',
                
                div(style="display: inline-block; width: 130px; vertical-align: top", h5(HTML("<b><u>Data set, control:</u></b>"))),
                div(style="display: inline-block; width: 60%;", selectizeInput("de.dataset1", NULL, NULL)), br(),
                div(style="display: inline-block; width: 130px; vertical-align: top", h5(HTML("<b>Group, control:</b>"))),
                div(style="display: inline-block; width: 60%;", selectizeInput("de.group1", NULL, NULL)), br(),
                
                div(style="display: inline-block; width: 130px; vertical-align: top", h5(HTML("<b><u>Data set, case:</u></b>"))),
                div(style="display: inline-block; width: 60%", selectizeInput("de.dataset2", NULL, NULL)), br(),
                div(style="display: inline-block; width: 130px; vertical-align: top", h5(HTML("<b>Group, case:</b>"))),
                div(style="display: inline-block; width: 60%", selectizeInput("de.group2", NULL, NULL)), br(),
                
                div(style="display: inline-block; width: 130px;", h5(HTML("<b>Select method:</b>"))),
                div(style="display: inline-block;", selectizeInput("de.method", NULL, de.method[, 1], width='120px')),
                div(style="display: inline-block; width: 5px;", h5(HTML(""))),
                div(style="display: inline-block; width: 130px; vertical-align: middle;", htmlOutput("de.info")), br(),
                div(style="display: inline-block; width: 130px;", h5(HTML("<b>Select species:</b>"))),
                div(style="display: inline-block;", selectizeInput("de.species", NULL, NULL, width='110px')), br(),
                
                div(style="display: inline-block;", 
                    actionButton('de.add', 'Add comparison', width='125px', style='font-size:85%; height: 36px')),
                div(style="display: inline-block; width: 5px;", h5(HTML(""))),
                div(style="display: inline-block; width: 60%; vertical-align: middle;", htmlOutput("de.add.msg")), br(), br(), 
                
                div(style="display: inline-block;", 
                    actionButton('de.remove', 'Remove comparison', width='125px', style='font-size:82%; height: 36px')),
                div(style="display: inline-block; width: 3px;", h5(HTML(""))),
                div(style="display: inline-block; width: 135px;", selectizeInput('de.remove.index', NULL, NULL)),
                div(style="display: inline-block; width: 5px;", h5(HTML(""))),
                div(style="display: inline-block; width: 27%; vertical-align: middle;", htmlOutput("de.remove.msg"))
              )
            )
          ),
          column(
            6,
            conditionalPanel(
              condition = 'input["de.remove.index"] != ""',
              
              wellPanel(
                style='height: 560px; padding-top: 0px',
                h3(HTML("Comparisons")),
                DT::dataTableOutput('de.comparisons', width='100%'),
                div(style="display: inline-block; ", actionButton('de.run', 'Run analysis', icon=icon('paper-plane'), class='dB')),
                div(style="display: inline-block; width: 5px;", h5(HTML(""))),
                div(style="display: inline-block; ", htmlOutput('de.run.msg'))
              )
            )
          )
        )
      )
    ),
    
    tabPanel(
      "View result", 
      
      conditionalPanel(
        condition = 'input["de.result.index"] == ""',
        list(h4('No results; run DE analysis first to obtain results.'))
      ),
      
      conditionalPanel(
        condition = 'input["de.result.index"] != ""',
        h4(HTML("View and compare results of two-group DE comparison(s).")),
        
        fluidRow(
          column(
            6,
            wellPanel(
              style='height: 560px; padding-top: 0px',
              h3(HTML("Results")),
              div(style="display: inline-block;", h5(HTML("<b>Select comparison:</b>"))),
              div(style="display: inline-block; width: 5px;", h5(HTML(""))),
              div(style="display: inline-block; width: 140px;", selectizeInput('de.result.index', NULL, NULL)), 
              
              DT::dataTableOutput('de.result.table', width='100%'),
              
              div(style="display: inline-block;", downloadLink('de.download', label = '[Download current table]')),
              div(style="display: inline-block; width: 20px;", h5(HTML(""))),
              div(style="display: inline-block;", downloadLink('de.download.all', label = '[Download all comparisons]'))
            )
          ),
          column(
            6,
            wellPanel(
              style='height: 560px; padding-top: 0px',
              h3(HTML("Figures")),
              
              div(style="display: inline-block; width: 80px;", h5(HTML("<b>Plot type:</b>"))),
              div(style="display: inline-block; width: 120px;", selectInput('de.plot.type', NULL, de.plot.type, selected = 1)),
              
              plotlyOutput('de.plot', width = '100%', height = '440px')
            )
          )
        )
      )
    ),
    source('ui_analysis_demeta.R', local=TRUE)$value,
    
    fluidRow(
      conditionalPanel(
        condition = "2 < 1", 
        checkboxInput('de1', 'de1', TRUE),
        checkboxInput('demeta1', 'demeta1', FALSE)
      )
    )
  )
)
