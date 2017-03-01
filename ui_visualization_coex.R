
# ############################################################################
# ############################# "Coexpression" tab ###########################
# tabPanel(
#   "Two gene coexpression", htmlOutput('coex.title'), br(),
#   fluidRow(
#     column(8, 
#            wellPanel( fluidRow(
#              column(4, wellPanel(
#                selectizeInput('coex.type', 'Select plot unit', rev(data.level.options), width='60%'),
#                selectizeInput("coex.scale", 'Select data scale', c('logged', 'unlogged', 'percentile'),  width='60%'),
#                selectizeInput("coex.color", 'Select color range', GetColorTypes(),  width='60%'),
#                
#                fluidRow(
#                  column(4,
#                         checkboxInput('coex.lookup.option', HTML(geex.html.msg("Look up gene in dictionary")), FALSE),
#                         uiOutput("coex.lookup.key.ui"),
#                         uiOutput("coex.lookup.species.ui")                       
#                  ), column(8, uiOutput("coex.lookup.table.ui")))
#              )),
#              column(4, wellPanel(h4("Select gene 1"), DT::dataTableOutput('coex.gene1', width='100%'))),
#              column(4, wellPanel(h4("Select gene 2"), DT::dataTableOutput('coex.gene2', width='100%'))))),
#            column(12, fluidRow(
#              column(12, htmlOutput('coex.message')),
#              column(12, plotOutput('coex.plot', width='640px', height='640px'))))),
#     column(4, wellPanel(
#       h4("Highlight data set(s)"), 
#       DT::dataTableOutput('coex.dataset', width='100%'),
#       actionButton('coex.clear.ds', 'Clear selection')))))
# 
# #####################################################################################################
# #####################################################################################################

