# options(shiny.maxRequestSize=64000000); 

# source('server_home.R', local=TRUE);
source('server_data.R', local=TRUE);
# source('server_visualization.R', local=TRUE);
# source('server_advance.R', local=TRUE);

shinyServer(function(input, output, session) { 
  cat("new visitor: ", session$token, '\n');
  
  session.data <- reactiveValues(
    loaded = geex.load.collection('C0000', GEX_HOME, input, output, session)
  );

  # sid <- paste(Sys.Date(), session$token, sep='/');
  # dir <- paste(APP_HOME, 'log', sid, sep='/');
  # session.data <- reactiveValues(id = sid, dir = dir, show = 0, run = 0, matrix = NULL, result = NULL, meta = NULL);
  # if (!file.exists(dir)) dir.create(dir, recursive = TRUE);
  
  # session.data <- server_home(input, output, session, session.data);
  session.data <- server_data(input, output, session, session.data);
  # session.data <- server_visualization(input, output, session, session.data);
  # session.data <- server_advance(input, output, session, session.data);
});

