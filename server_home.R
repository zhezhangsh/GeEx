server_home <- function(input, output, session, session.data) {
  output$home.table <- DT::renderDataTable({
    data.frame(ID=rownames(coll), coll, stringsAsFactors=FALSE)
  }, options=dt.options0, selection='single', rownames=FALSE, escape=FALSE);

  output$home.ui <- renderUI({
    rw <- input$home.table_rows_selected; 
    if (length(rw) == 0) {
      h5(HTML('')); 
    } else {
      actionButton('home.button', "Load data collection", icon = icon('download'), class='dB');
    }
  });
  
  observeEvent(input$home.button, {
    rid <- input$home.table_rows_selected; 
    if (length(rid) > 0) {
      cid <- rownames(coll)[as.integer(rid)[1]]; 
      if (!identical(cid, session.data$loaded$id)) {
        withProgress({
          session.data$loaded <- geex.load.collection(cid, GEX_HOME, input, output, session);
          session.data$coex <- list(startover = FALSE, dataset = NULL, result = NULL, cluster = NULL);
          session.data$data <- list(meta=NULL, expr=NULL, gene=NULL, comb=NULL);
          session.data$two <- list(gene=NULL, geneset=NULL);
          session.data$de <- list(startover=FALSE, comparison=NULL, result=NULL, meta=NULL);
        }, min = 0, max = 100, message = "Loading data collection ... ...", detail = "please wait")
      }
    } else session.data$loaded <- NULL; 
  });

  output$home.message<-renderUI({
    cll <- session.data$loaded;
    if (identical(NULL, cll)) h5(HTML("Data collection loaded:"), HTML('<b>None</b>')) else 
      h5(HTML("Data collection loaded:"), HTML('<b><u>', geex.html.msg(cll$name), '</u></b>'))
  }); 

  output$home.download.table <- downloadHandler(
    filename = function() { 'data_collection.txt' },
    content  = function(file) {
      tbl <- data.frame(ID=rownames(coll), coll, stringsAsFactors=FALSE); 
      write.table(tbl, file, sep='\t', row.names = FALSE, quote = FALSE);
    }
  );
  
  output$home.download.column <- downloadHandler(
    filename = function() { 'column_description.txt' },
    content  = function(file) {
     tbl <- readRDS('data/collection_column.rds');
     write.table(tbl, file, sep='\t', row.names = FALSE, quote = FALSE);
    }
  );
  
  session.data;
}