############################################################################### 
################################ "Data" menu ##################################     
navbarMenu(
  "Data",
  ####################
  
  ############################################################################
  ############################# "Data set" tab ###############################
  tabPanel(
    "Data set summary",
    h2("Data set summary"),
    h5(HTML('Gene expression data matrix and descriptive statistics.')),
    
    conditionalPanel(
      condition = 'input["expr.dataset"] != ""',
      wellPanel(
        fluidRow(
          column(
            3, 
            selectizeInput("expr.dataset", "Select data set", NULL, width='95%'),
            checkboxGroupInput("expr.group", label='Select group(s)', choices=c()),
            checkboxGroupInput("expr.species", label='Select species', choices=c()),
            selectizeInput("expr.scale", label='Select data scale', choices=c('logged', 'unlogged', 'percentile'), selected='logged', width='80%')
          ),
          column(
            3,
            h5('Select table column(s)'),
            checkboxGroupInput('expr.column', NULL, choice=expr_columns, selected=expr_columns[1], inline=FALSE),
            checkboxGroupInput('expr.desc', h5('-Descriptive statistics'), choices=GetRowStatTypes(),
                               selected=GetRowStatTypes(), inline=TRUE),
            checkboxGroupInput('expr.anno', h5('-Gene annotation'), choices=expr_anno_columns,
                               selected=expr_anno_columns[c(1, length(expr_anno_columns))], inline=TRUE), br(),
            checkboxInput('expr.select.all', HTML(geex.html.msg('Select all')), FALSE)
          ),
          column(
            6, 
            fluidRow(
              h4(HTML('<b>Filter table with gene set(s)</b>')),
              div(style="display: inline-block;", selectizeInput("expr.gs.source", "Source", names(geneset), width='115px')),
              div(style="display: inline-block;", selectizeInput("expr.gs.coll", "Collection", names(geneset[[1]]), width='230px')),
              div(style="display: inline-block;", selectizeInput("expr.gs.species", "Species", names(geneset[[1]][[1]]), width='85px')),
              DT::dataTableOutput('expr.gs.table', width='100%'),
              actionButton("expr.gs.clear", 'Clear selection')
            )
          )
        )
      )
    ),
    
    htmlOutput('expr.message'),
    DT::dataTableOutput('expr.table')
  ), # end of tabPanel
  
  ############################################################################
  ############################# "Metadata" tab ###############################
  tabPanel(
    "Metadata", 
    h2("Metadata"),
    h5(HTML('Information about data sets, sample groups, samples, and genes.')),
    
    conditionalPanel(
      condition = 'input["meta.options"] != ""',
      wellPanel(
        div(style="display: inline-block;", htmlOutput('meta.message')),
        div(style="display: inline-block;", h5(HTML("&nbsp&nbsp&nbsp&nbsp&nbsp<b>Select metadata table</b>&nbsp&nbsp"))),
        div(style="display: inline-block;", selectizeInput("meta.options", NULL, NULL, width='160px')),
        div(style="display: inline-block;", h5(HTML("&nbsp&nbsp&nbsp(highlight row to get more details)")))
      ),
      fluidRow(
        column(7, DT::dataTableOutput('meta.table')),
        column(5, htmlOutput('meta.detail'))
      ) 
    ) 
  ) # end of tabPanel
  

  # ############################################################################
  # ############################# "Gene summary" tab ###########################
  # tabPanel(
  #   "Gene summary", htmlOutput('gene.title'), br(), 
  #   wellPanel(
  #     h4("Select the gene to show"), 
  #     fluidRow(column(2, checkboxInput('gene.filter', HTML(geex.html.msg('Filter by gene set')), value=FALSE)), 
  #              column(10, uiOutput("gene.ui"))),
  #     wellPanel(DT::dataTableOutput('gene.table'))),
  #   htmlOutput('gene.message'), br(), 
  #   DT::dataTableOutput('gene.stat.table')
  # ), # end of tabPanel
  # 
  # ############################################################################
  # ############################# "Gene-data set " tab #########################
  # tabPanel(
  #   "Gene-data set", htmlOutput('comb.title'), br(), 
  #   wellPanel(
  #     fluidRow(column(2, checkboxInput('comb.filter', HTML(geex.html.msg('Filter by gene set')), value=FALSE)), 
  #              column(10, uiOutput("comb.ui"))), br(),
  #     fluidRow( 
  #       column(4, wellPanel(
  #         fluidRow(column(5, h5('Select species')), 
  #                  column(6, selectizeInput("comb.species", NULL, choices='human'))),
  #         fluidRow(column(5, h5('Select data scale')), 
  #                  column(6, selectizeInput("comb.scale", NULL, choices=c('logged', 'unlogged', 'percentile'), 
  #                                           selected='percentile'))))),
  #       column(4, wellPanel(
  #         checkboxGroupInput('comb.desc', 'Select descriptive statistics', choices=GetRowStatTypes(), 
  #                            selected=GetRowStatTypes()[1:(length(GetRowStatTypes())-1)], inline=TRUE))),
  #       column(4, wellPanel(
  #         checkboxGroupInput('comb.anno', 'Select gene annotation', choices=expr_anno_columns, 
  #                            selected=expr_anno_columns[1], inline=TRUE))))),
  #   DT::dataTableOutput('comb.table')
  # ) # end of tabPanel
) # end of navbarMenu
