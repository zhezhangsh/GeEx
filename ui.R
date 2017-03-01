#####################
####### GeEx ########
#####################
source("/srv/shiny-server/geex/preload.R");

shinyUI(fluidPage(
  tags$head(includeScript("google-analytics.js")),
  
  navbarPage(
    id="GeEx",
    collapsible = TRUE,
    windowTitle = "Awsomics GeEx",
    title = HTML("<b><i>GeEx</i></b>", "<font color='#F0F0F0' size=1>&nbsp<u>G</u>ene <u>e</u>xpression <u>Ex</u>plorer</font>"),
    theme = shinythemes::shinytheme("united"),

    tabPanel(
      "Home",

      tags$head(
        tags$style(HTML("h1{text-shadow: 0 1px 0 #ccc, 0 2px 0 #c9c9c9, 0 3px 0 #bbb, 0 4px 0 #b9b9b9,
                        0 5px 0 #aaa, 0 6px 1px rgba(0,0,0,.1), 0 0 5px rgba(0,0,0,.1), 0 1px 3px rgba(0,0,0,.3),
                        0 3px 5px rgba(0,0,0,.2), 0 5px 10px rgba(0,0,0,.25), 0 10px 10px rgba(0,0,0,.2),
                        0 20px 20px rgba(0,0,0,.15); color: #666666; font-family: Copperplate;}"))),
      tags$head(
        tags$style(HTML("h2{text-shadow: 0 1px 0 #ccc, 0 2px 0 #c9c9c9, 0 3px 0 #bbb, 0 4px 0 #b9b9b9,
                        0 5px 0 #aaa, 0 6px 1px rgba(0,0,0,.1), 0 0 5px rgba(0,0,0,.1), 0 1px 3px rgba(0,0,0,.3),
                        0 3px 5px rgba(0,0,0,.2), 0 5px 10px rgba(0,0,0,.25), 0 10px 10px rgba(0,0,0,.2),
                        0 20px 20px rgba(0,0,0,.15); color: #666666; font-family: Copperplate;}"))),
      tags$head(
        tags$style(".dB{background-color: tomato; border: 1px solid #bfbfbf; box-shadow: inset 0 1px 0 white,
                    inset 0 -1px 0 #d9d9d9, inset 0 0 0 1px #f2f2f2, 0 2px 4px rgba(0, 0, 0, 0.2);
                    color: white; text-shadow: 0 1px 0 rgba(255, 255, 255, 0.5); border-radius: 3px;
                    cursor: pointer; display: inline-block; font-family: Verdana, sans-serif; font-size: 12px;
                    font-weight: 400; line-height: 20px; padding: 9px 16px 9px; margin: 0px 0 0 0px;
                    transition: all 20ms ease-out;}")),
      
      h1(HTML('Welcome to Awsomics - GeEx')),

      div(style="padding: 0px; border-style: solid; border-width: 1px; width: 1280px", DT::dataTableOutput('home.table', width='100%')),

      htmlOutput('home.message'),
      uiOutput('home.ui'),
      br(),

      div(style="display: inline-block; vertical-align: top; face: Copperplate;",
          h3(HTML('<font color="tomato">Quick start: </font>'))),
      div(style="display: inline-block;",
          p(style="text-indent: 15pt; line-height:15px", HTML('1. Highlight a row to select a data collection.')),
          p(style="text-indent: 15pt; line-height:15px", HTML('2. Click button to load the data collection.')),
          p(style="text-indent: 15pt; line-height:15px", HTML('3. Use the drop-down menus on the top to browse, visualize and analyze data.')),
          h6(style="text-indent: 25pt; line-height:10px", HTML('<b>Data:</b> metadata, summary statistics, ...')),
          h6(style="text-indent: 25pt; line-height:10px", HTML('<b>Visualization:</b> PCA, heatmap, bar plot, ...')),
          h6(style="text-indent: 25pt; line-height:10px", HTML('<b>Analysis:</b> co-expression analysis, gene clustering, group comparison, ...'))
      ),
      br(),

      div(style="display: inline-block; line-height:10px", h6(HTML('<a href="http://awsomics.org" target="_blank">[Awsomics home]</a>'))),
      div(style="display: inline-block; width: 5px", h6()),
      div(style="display: inline-block; line-height:10px", h6(HTML('<a href="mailto:zhangz@email.chop.edu">[Contact us]</a>'))),
      div(style="display: inline-block; width: 5px", h6()),
      div(style="display: inline-block;", downloadLink('home.download.table', label = h6(HTML('[Download collection table]')))),
      div(style="display: inline-block; width: 5px", h6()),
      div(style="display: inline-block;", downloadLink('home.download.column', label = h6(HTML('[Download column description]'))))
    ),
    
    source('ui_data.R', local=TRUE)$value,
    source('ui_visualization.R', local=TRUE)$value,
    source('ui_analysis.R', local=TRUE)$value
    
  ) # end of navbarPage
)) # end of shinyUI
