server_data_meta <- function(input, output, session, session.data) {
  observeEvent(input$meta.options, {
    output$meta.message <- renderUI({''});
    output$meta.detail <- renderUI({''});
    output$meta.table <- DT::renderDataTable({NULL});
  });
  observeEvent(input$meta.table.button, {
    cll <- isolate(session.data$loaded);
    
    output$meta.message <- renderUI({
      if (!identical(NULL, cll)) h4(HTML(paste('Data collection -', geex.html.msg(cll$name))))}
    );
    
    output$meta.table <- DT::renderDataTable({
      if (!identical(cll, NULL)) {
        tbl.nm <- isolate(input$meta.options);  
        if (tbl.nm=='' | is.na(tbl.nm)) {
          tbl <- cll$browse_table[[1]];
          updateSelectizeInput(session, 'meta.options', choices=names(cll$browse_table));
        } else tbl <- cll$browse_table[[tbl.nm]];    
        isolate(session.data$data$meta <- tbl);
        tbl;
      } else {
        isolate(session.data$data$meta <- NULL);
        NULL;
      }
    }, options = dt.options6, selection='single', rownames=FALSE, escape = FALSE);
  });
  
  output$meta.download <- downloadHandler(
    filename = function() { paste(session.data$loaded$name, "metadata.txt", sep='_'); }, 
    content = function(file) {
      tbl <- session.data$data$meta;
      if (!is.null(tbl)) {
        for (i in 1:ncol(tbl)) tbl[[i]] <- CleanHtmlTags(tbl[[i]], remove.empty = FALSE); 
        write.table(tbl, file, row.names = FALSE, col.names = TRUE, quote = FALSE, sep='\t'); 
      }
    }
  );
  
  observeEvent(input$meta.table_rows_selected, {
    output$meta.detail <- renderUI({
      rid1 <- isolate(input$meta.table_rows_selected);
      rid2 <- isolate(input$meta.table_row_last_clicked);
      if (length(rid1)>0 & length(rid2)>0)
        if (rid2[1] %in% rid1) {
          cll <- isolate(session.data$loaded);
          if (!identical(cll, NULL)) {
            r <- rownames(cll$browse_table[[isolate(input$meta.options)]])[rid2[1]];
            t <- sapply(cll$metadata_by_id, function(x) r %in% names(x));
            
            if (isolate(input$meta.options) == 'Gene') {
              id <- rownames(session.data$data$meta)[rid2[1]];
              HtmlKeyValue(c(ID=id, as.list(cll$gene[id, ])), separator2 = ' ');
            } else if (length(t[t])>0) {
              HtmlKeyValue(cll$metadata_by_id[[which(t)[1]]][[rid2[1]]], separator2=' ');
            } 
          }
        }
    });
  }); 

  
  session.data;
}