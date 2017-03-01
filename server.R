# options(shiny.maxRequestSize=64000000);
options(shiny.sanitize.errors = FALSE);

source('server_home.R', local=TRUE);
source('server_data.R', local=TRUE);
source('server_visualization.R', local=TRUE);
source('server_analysis.R', local=TRUE);

shinyServer(function(input, output, session) { 
  cat("new visitor: ", session$token, '\n');
  
  session.data <- reactiveValues(
    loaded = NULL,
    # loaded = geex.load.collection('C0000', GEX_HOME, input, output, session),
    data = list(meta=NULL, expr=NULL, gene=NULL, comb=NULL),
    two = list(gene=NULL, geneset=NULL), 
    de = list(startover=FALSE, comparison=NULL, result=NULL, meta=NULL),
    coex = list(startover=FALSE, dataset=NULL, result=NULL, cluster=NULL)
  );

  session.data <- server_home(input, output, session, session.data);
  session.data <- server_data(input, output, session, session.data);
  session.data <- server_visualization(input, output, session, session.data);
  session.data <- server_analysis(input, output, session, session.data);
});

