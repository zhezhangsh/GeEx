#####################
####### GeEx ########
#####################
source("/srv/shiny-server/geex/preload_test.R"); 

shinyUI(
  # tags$head(includeScript("google-analytics.js")),
  
  navbarPage(
    id="GeEx", 
    collapsible = TRUE, 
    windowTitle = "Awsomics GeEx", 
    title = HTML("<b><u><i>GeEx</i></u></b>", "<font color='#F0F0F0' size=1>&nbsp<u>G</u>ene <u>e</u>xpression <u>Ex</u>plorer</font>"),
    theme = shinytheme("united"),
    
    ############################################################################ 
    ################################ "Home" tab ################################ 
    # tabPanel(
    #   "Home", htmlOutput('home.title'), hr(),
    #   htmlOutput('home.message'), br(),
    #   DT::dataTableOutput('home.table')
    # ),
  
    source('ui_data.R', local=TRUE)$value
    
    # source('ui_visualization.R', local=TRUE)$value,
    # source('ui_advance.R', local=TRUE)$value
    
    ############################################################################## 
    ################################# "About" tab ################################
    # "About" tab
    # tab.home <- 
    #   tabPanel("About", id = 'tab.home', wellPanel(style = "background-color: #ffffff;", includeMarkdown('./intro.Rmd')))
  ) # end of navbarPage
) # end of shinyUI
