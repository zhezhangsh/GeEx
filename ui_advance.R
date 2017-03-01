######################################################################################## 
################################ "Advance" menu ########################################       
####################
navbarMenu(
  "Advance",
  ####################
  
  ############################################################################
  ############################# "Filter genes" tab ###########################
  tabPanel(
    "Filter genes", htmlOutput('filter.title'), br(),
    wellPanel( 
      fluidRow( 
        column(6, wellPanel( 
          h3('Specify comparison'),
          selectizeInput('filter.dataset', 'Select data set vs. other data sets', c(), width='75%'),
          selectizeInput('filter.group', 'Select group vs. other groups in the same data set', c(), width='75%'),
          selectizeInput('filter.sample', 'Select sample vs. other samples in the same data set', c(), width='75%')
        )),
        column(6, wellPanel( 
          h3('Specify cutoffs'),
          sliderInput('filter.absolute', 'Range of absolute expression level in percentile', 0, 100, c(0, 100), 1),
          sliderInput('filter.relative', 'Range of relative difference in percentile', 0, 100, c(0, 100), 1),
          radioButtons('filter.direction', NULL, c('Higher', 'Lower'), inline=TRUE)
        ))
      )),
    htmlOutput('filter.msg'), 
    DT::dataTableOutput('filter.table')
  ),
  
  ############################################################################
  ############################# "Re-group" tab ###############################
  tabPanel(
    "Re-group samples", htmlOutput('regroup.title'), br(),
    wellPanel(checkboxInput('regroup.check', 'Check box to activate sample re-grouping', FALSE)),
    fluidRow(
      column(6, 
             htmlOutput('regroup.ds.msg'), 
             DT::dataTableOutput('regroup.ds.tbl')), 
      column(6, wellPanel(
        h4("Step 1: select sample feature(s) to define data set"), 
        DT::dataTableOutput('regroup.dataset'), 
        actionButton('regroup.clear.ds', 'Clear selection')))),
    hr(),
    fluidRow(
      column(6, 
             htmlOutput('regroup.grp.msg'), 
             DT::dataTableOutput('regroup.grp.tbl')),
      column(6, wellPanel(
        h4("Step 2: select sample feature(s) to define group"), 
        DT::dataTableOutput('regroup.group'), 
        actionButton('regroup.clear.grp', 'Clear selection'))))
  ),
  
  ############################################################################
  ############################# "User data" tab ##############################
  tabPanel("Upload user data", htmlOutput('upload.title'))
)

