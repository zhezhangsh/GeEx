tabPanel(
  "Gene summary", 
  h2("Single gene summary"),
  h5(HTML('Summarize the descriptive statistics of a single gene across data sets.')),
  
  conditionalPanel(
    condition = 'input[["meta.options"]] == "" ',
    hr(),
    list(h3(msg.nocollection))
  ),
  
  conditionalPanel(
    condition = 'input[["meta.options"]] != "" ',
    wellPanel(
      h4(HTML(geex.html.msg("Highlight row to select a gene"))),
      DT::dataTableOutput('gene.table'), 
      
      checkboxInput("gene.filter", HTML(geex.html.msg("Filter genes by known gene sets"))),
      conditionalPanel(
        condition = 'input["gene.filter"] == true', 
        hr(),
        div(
          style="display: inline-block; width: 60%; vertical-align: top;", 
          DT::dataTableOutput("gene.gs.table")
        ),
        div(style="display: inline-block; width: 4%"),
        div(
          style="display: inline-block; width: 32%",
          div(style="display: inline-block; width: 75px;", h6(HTML("<b>Source:</b>"))),
          div(style="display: inline-block; width: 65%", selectizeInput('gene.gs.source', NULL, choices=names(geneset), selected=names(geneset)[1])), br(),
          div(style="display: inline-block; width: 75px", h6(HTML("<b>Collection:</b>"))),
          div(style="display: inline-block; width: 65%", selectizeInput('gene.gs.coll', NULL, choices=names(geneset[[1]]), selected=names(geneset[[1]])[1])), br(), 
          div(style="display: inline-block; width: 75px", h6(HTML("<b>Species:</b>"))),
          div(style="display: inline-block; width: 65%", selectizeInput('gene.gs.species', NULL, choices=NULL)), br(),
          actionButton("gene.gs.clear", "Clear selection", icon=icon('refresh'))
        )
      )
    ),
    
    div(style="display: inline-block;", actionButton('gene.table.button', 'Create table', icon=icon('table'), class='dB')), 
    div(style="display: inline-block; width: 10px", h4('')),  
    div(style="display: inline-block;", downloadLink('gene.download', HTML('<u>Download table</u>'))),
    br(), 
    
    htmlOutput('gene.message'),
    DT::dataTableOutput('gene.stat.table')
  )
)
