tabPanel(
  "Data set summary",
  h2("Data set summary"),
  h5(HTML('Gene expression data matrix and descriptive statistics.')),
  
  conditionalPanel(
    condition = 'input[["meta.options"]] == "" ',
    hr(),
    list(h3(msg.nocollection))
  ),
  
  conditionalPanel(
    condition = 'input["meta.options"] != ""',
    wellPanel(
      div(style="display: inline-block;", h4(HTML('<b>Select data set and table columns</b>'))),
      div(style="display: inline-block; width: 20px; ", h4("")),
      div(style="display: inline-block;", checkboxInput('expr.select.all', HTML(geex.html.msg('Select all')), FALSE)),
      br(),
      
      div(
        style="display: inline-block; width: 64%",
        div(style="display: inline-block; width: 16%;", h6(HTML("<b>Select data set:</b>"))), 
        div(style="display: inline-block; width: 72%", selectizeInput("expr.dataset", "", NULL))
      ),
      div(
        style="display: inline-block; width: 32%",
        div(style="display: inline-block; width: 24%;", h6(HTML("<b>Select species:</b>"))), 
        div(style="display: inline-block; width: 72%", selectizeInput("expr.species", "", NULL))
      ),
      br(),
      
      div(style="display: inline-block; width: 10.25%;", h6(HTML("<b>Select group(s):</b>"))), 
      div(style="display: inline-block; width: 85%;", checkboxGroupInput("expr.group", NULL, c(), inline=TRUE)), 
      br(),
      
      div(
        style="display: inline-block; width: 64%",
        div(style="display: inline-block; width: 16%;", h6(HTML("<b>Select statistics:</b>"))), 
        div(style="display: inline-block; width: 80%;", 
            checkboxGroupInput('expr.desc', NULL, choices=GetRowStatTypes(), selected=GetRowStatTypes(), inline=TRUE)), br(),
        div(style="display: inline-block; width: 16%;", h6(HTML("<b>Select annotation:</b>"))), 
        div(style="display: inline-block; width: 80%;", 
            checkboxGroupInput('expr.anno', NULL, choices=expr_anno_columns[-1], selected=expr_anno_columns[length(expr_anno_columns)], inline=TRUE)) 
      ),
      
      div(
        style="display: inline-block; width: 32%",
        div(style="display: inline-block; width: 24%;", h6(HTML("<b>Select scale:</b>"))), 
        div(style="display: inline-block; width: 72%", radioButtons("expr.scale", NULL, choices=c('logged', 'unlogged', 'percentile'), inline=TRUE)), br(),
        div(style="display: inline-block; width: 24%;", h6(HTML("<b>Select value:</b>"))), 
        div(style="display: inline-block; width: 72%", checkboxGroupInput('expr.column', NULL, choices=expr_columns, selected=expr_columns[1], inline=TRUE))
      ),

      checkboxInput("expr.filter", HTML(geex.html.msg("Filter genes by known gene sets"))), 
      conditionalPanel(
        condition = 'input["expr.filter"] == true', 
        hr(),
        div(
          style="display: inline-block; width: 60%; vertical-align: top;", 
          DT::dataTableOutput("expr.gs.table")
        ),
        div(style="display: inline-block; width: 4%"),
        div(
          style="display: inline-block; width: 32%",
          div(style="display: inline-block; width: 75px;", h6(HTML("<b>Source:</b>"))),
          div(style="display: inline-block; width: 65%", selectizeInput('expr.gs.source', NULL, choices=names(geneset), selected=names(geneset)[1])), br(),
          div(style="display: inline-block; width: 75px", h6(HTML("<b>Collection:</b>"))),
          div(style="display: inline-block; width: 65%", selectizeInput('expr.gs.coll', NULL, choices=names(geneset[[1]]), selected=names(geneset[[1]])[1])), br(), 
          div(style="display: inline-block; width: 75px", h6(HTML("<b>Species:</b>"))),
          div(style="display: inline-block; width: 65%", selectizeInput('expr.gs.species', NULL, choices=NULL)), br(),
          actionButton("expr.gs.clear", "Clear selection", icon=icon('refresh'))
        )
      )  
    ),
    
    div(style="display: inline-block;", actionButton('expr.table.button', 'Create table', icon=icon('table'), class='dB')), 
    div(style="display: inline-block; width: 10px", h4('')),  
    div(style="display: inline-block;", downloadLink('expr.download', HTML('<u>Download table</u>'))),
    br(), 
    
    htmlOutput('expr.message'),
    DT::dataTableOutput('expr.table')
  )
)
