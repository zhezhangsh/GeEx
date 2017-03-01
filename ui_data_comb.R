tabPanel(
  "Gene - data set",
  h2("Gene - data set"),
  h5(HTML('Summary of gene-level data across all data sets.')),
  
  conditionalPanel(
    condition = 'input[["meta.options"]] == "" ',
    hr(),
    list(h3(msg.nocollection))
  ),
  
  conditionalPanel(
    condition = 'input[["meta.options"]] != "" ',
    wellPanel(
      div(style="display: inline-block;", h4(HTML('<b>Select data sets and table columns</b>'))),
      div(style="display: inline-block; width: 20px;", h4("")),
      div(style="display: inline-block;", checkboxInput('comb.select.all', HTML(geex.html.msg('Select all')), FALSE)),
      br(),
      
      div(style="display: inline-block; width: 10.25%; vertical-align: middle;", h6(HTML("<b>Select data set(s):</b>"))), 
      div(style="display: inline-block; width: 85%;", checkboxGroupInput("comb.dataset", NULL, NULL, inline=TRUE)), 
      br(),

      div(
        style="display: inline-block; width: 64%",
        div(style="display: inline-block; width: 16%;", h6(HTML("<b>Select statistics:</b>"))), 
        div(style="display: inline-block; width: 80%;", 
            checkboxGroupInput('comb.desc', NULL, choices=GetRowStatTypes(), selected=GetRowStatTypes(), inline=TRUE)), br(),
        div(style="display: inline-block; width: 16%;", h6(HTML("<b>Select annotation:</b>"))), 
        div(style="display: inline-block; width: 80%;", 
            checkboxGroupInput('comb.anno', NULL, choices=expr_anno_columns[-1], selected=expr_anno_columns[length(expr_anno_columns)], inline=TRUE)) 
      ),

      div(
        style="display: inline-block; width: 32%",
        div(style="display: inline-block; width: 24%;", h6(HTML("<b>Select scale:</b>"))), 
        div(style="display: inline-block; width: 72%", radioButtons("comb.scale", NULL, choices=c('logged', 'unlogged', 'percentile'), inline=TRUE)), br(),
        div(style="display: inline-block; width: 24%;", h6(HTML("<b>Select value:</b>"))), 
        div(style="display: inline-block; width: 72%", selectizeInput("comb.species", "", NULL))
      ),
      br(),

      checkboxInput("comb.filter", HTML(geex.html.msg("Filter genes by known gene sets"))), 
      conditionalPanel(
        condition = 'input["comb.filter"] == true', 
        hr(),
        div(
          style="display: inline-block; width: 60%; vertical-align: top;", 
          DT::dataTableOutput("comb.gs.table")
        ),
        div(style="display: inline-block; width: 4%"),
        div(
          style="display: inline-block; width: 32%",
          div(style="display: inline-block; width: 75px;", h6(HTML("<b>Source:</b>"))),
          div(style="display: inline-block; width: 65%", selectizeInput('comb.gs.source', NULL, choices=names(geneset), selected=names(geneset)[1])), br(),
          div(style="display: inline-block; width: 75px", h6(HTML("<b>Collection:</b>"))),
          div(style="display: inline-block; width: 65%", selectizeInput('comb.gs.coll', NULL, choices=names(geneset[[1]]), selected=names(geneset[[1]])[1])), br(), 
          div(style="display: inline-block; width: 75px", h6(HTML("<b>Species:</b>"))),
          div(style="display: inline-block; width: 65%", selectizeInput('comb.gs.species', NULL, choices=NULL)), br(),
          actionButton("comb.gs.clear", "Clear selection", icon=icon('refresh'))
        )
      )      
    ),
    
    div(style="display: inline-block;", actionButton('comb.table.button', 'Create table', icon=icon('table'), class='dB')), 
    div(style="display: inline-block; width: 10px", h4('')),  
    div(style="display: inline-block;", downloadLink('comb.download', HTML('<u>Download table</u>'))),
    br(), 
    
    div(style="display: inline-block;", htmlOutput('comb.message')),
    DT::dataTableOutput('comb.table')
  )
) # end of tabPanel