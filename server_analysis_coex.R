server_analysis_coex <- function(input, output, session, session.data) {
  # observeEvent(input$coex.gene.all, { 
  #   cll <- session.data$loaded;
  #   all <- input$coex.gene.all;
  #   tbl <- NULL;
  #   if (identical(NULL, cll)) {
  #     msg <- msg.nocollection;
  #   } else {
  #     if (all) {
  #       tbl <- cll$browse_table$Gene[, 1:5];
  #       msg <- 'Highlight row to select a gene';
  #     } else msg <- msg.nogene;
  #   };
  #   
  #   isolate({
  #     if (!all) output$coex.gene.msg <- renderUI(h5('')) else
  #       output$coex.gene.msg <- renderUI(h5(HTML(geex.html.msg("Highlight row to select a gene"))));
  #     output$coex.gene.table <- DT::renderDataTable({ tbl;
  #     }, options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE, selection = 'single');
  #     session.data$coex$table <- tbl;      
  #   });
  # 
  #   updateTextInput(session, 'coex.gene.text', NULL, 'Enter keyword ...');
  #   updateCheckboxInput(session, 'coex1', value=FALSE);
  #   updateCheckboxInput(session, 'coex2', value=FALSE);
  # });
  
  # observeEvent(input$coex.gene.button, {
  #   key <- isolate(input$coex.gene.text);
  #   cll <- isolate(session.data$loaded);
  #   tbl <- NULL;
  #   
  #   if (!identical(NULL, cll) & !identical('', key) & !identical(NA, key)) {
  #     ann <- cll$browse_table$Gene;
  #     spe <- unique(ann$Species);
  #     res <- lapply(spe, function(spe) geex.lookup.gene(ann, spe, key, GENE_HOME));
  #     res <- res[sapply(res, ncol)>1];
  #     if (length(res) == 0) {
  #       output$coex.dataset.msg <- renderUI(h5(HTML(geex.html.msg('No genes match the search key'))));
  #       tbl <- ann[0, , drop=FALSE];
  #     } else {
  #       output$coex.dataset.msg <- renderUI(h5(HTML(geex.html.msg('Highlight rows to select data set(s)'))));
  #       tbl <- do.call('rbind', res);
  #       tbl <- ann[rownames(tbl), 1:5, drop=FALSE];
  #     };
  #     updateCheckboxInput(session, 'coex1', value=FALSE);
  #     output$coex.gene.table <- DT::renderDataTable({ tbl;
  #     }, options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE, selection = 'single');
  #     isolate(session.data$coex$table <- tbl);
  #     # isolate(updateCheckboxInput(session, 'coex.gene.all', value = FALSE)); 
  #   };
  # });
  
  observeEvent(input$coex.gene.table_rows_selected, { 
    cll <- session.data$loaded; 
    gtb <- cll$browse_table$Gene;
    rid <- input$coex.gene.table_rows_selected;
    tbl <- NULL;
    
    ex1 <- FALSE
    if (!identical(NULL, cll)) { 
      if (length(rid)>0) {
        gid <- rownames(gtb)[as.integer(rid)[1]];
        dst <- cll$mapping$gene2dataset[[gid]];
        tbl <- cll$browse_table$`Data set`[dst, , drop=FALSE];
      } else tbl <- cll$browse_table$`Data set`[0, , drop=FALSE];
      output$coex.dataset.table <- DT::renderDataTable({ tbl[, c(1, 2, 5, 6, 7), drop=FALSE];
      }, options = dt.options3, rownames=FALSE, server=TRUE, escape = FALSE, selection = 'multiple');
      updateCheckboxInput(session, 'coex1', value=TRUE);
      isolate(session.data$coex$dataset <- tbl);
    };    
  });
  
  output$coex.run.msg <- renderUI({
    if (!session.data$de$startover) h6(HTML("Click to run analysis")) else h6(HTML("Click to start over"))
  });
  
  observeEvent(input$coex.run, {
    withProgress({
      cll <- session.data$loaded; 
      
      if (session.data$coex$startover) {
        session.data$coex$startover <- FALSE;
        session.data$coex$dataset <- NULL;
        session.data$coex$result <- NULL;
        session.data$coex$table <- NULL; 
        
        updateCheckboxInput(session, 'coex1', value=TRUE); 
        updateCheckboxInput(session, 'coex2', value=FALSE); 
        updateCheckboxInput(session, 'coex3', value=TRUE); 
        updateCheckboxInput(session, 'coex.gene.all', NULL, TRUE);
        updateTextInput(session, 'coex.gene.text', NULL, 'Enter keyword ...');
        updateRadioButtons(session, 'coex.dataset.all', selected=1); 
        output$coex.gene.table <- DT::renderDataTable({ cll$browse_table$Gene[, 1:5, drop=FALSE];
        }, options = dt.options8, rownames=FALSE, server=TRUE, escape = FALSE, selection = 'single');
        
        updateActionButton(session, 'coex.run', 'Run analysis');
        output$coex.run.msg <- renderUI({ h6(HTML("Click to run analysis")); });
        output$coex.result <- DT::renderDataTable(NULL, options = dt.options7);
        output$coex.dataset.table <- DT::renderDataTable(NULL, options = dt.options7);
      } else { 
        gtb <- cll$browse_table$Gene;
        gid <- rownames(gtb)[input$coex.gene.table_rows_selected]; 
        dtb <- session.data$coex$dataset; 
        did <- input$coex.dataset.table_rows_selected; 
        
        if (identical(NULL, cll)) msg <- geex.html.msg(msg.nocollection) else 
          if (identical(NULL, gtb) | length(gid)<1) msg <- geex.html.msg(msg.nogene) else
            if (identical(NULL, dtb)) msg <- geex.html.msg(msg.nodataset) else {
              msg <- "Click to start over"; 
              updateActionButton(session, 'coex.run', 'Start over');
              updateCheckboxInput(session, 'coex1', value=FALSE); 
              updateCheckboxInput(session, 'coex2', value=TRUE); 
              updateCheckboxInput(session, 'coex3', value=FALSE); 
              
              if (length(did)==0) did <- rownames(dtb) else did <- rownames(dtb)[did]; 
              setProgress(value=10);
              isolate(session.data$coex$result <- res <- geex.coex.corr(cll, gid, did));
              
              output$coex.result <- DT::renderDataTable({ res[, 1:(ncol(res)-2), drop=FALSE]; 
              }, options = dt.options6, rownames=FALSE, server=TRUE, escape = FALSE, selection = 'none');
            }
            setProgress(80);
            output$coex.run.msg <- renderUI({ h6(HTML(msg)); });
            if (!identical(NULL, session.data$coex$result)) isolate(session.data$coex$startover <- TRUE);
      };
      setProgress(95); 
    }, message = "Running co-expression analysis ...", detail = "Please wait.");
  });
  
  output$coex.download.result <- downloadHandler(
    filename = function() { 'correlation2gene.txt' },  
    content  = function(file) { 
      tbl <- session.data$coex$result;
      tbl[[1]] <- CleanHtmlTags(tbl[[1]], remove.empty = FALSE); 
      if (!identical(NULL, tbl)) write.table(tbl, file, sep='\t', row.names = FALSE, quote = FALSE); 
    } 
  );
  output$coex.download.column <- downloadHandler(
    filename = function() { 'column_description.txt' },  
    content  = function(file) { 
      tbl <- readRDS('data/coex_column.rds');
      write.table(tbl, file, sep='\t', row.names = FALSE, quote = FALSE); 
    } 
  );
  output$coex.download.metadata <- downloadHandler(
    filename = function() { 'datasets.txt' },  
    content  = function(file) { 
      tbl <- session.data$coex$dataset;
      for (i in 1:ncol(tbl)) tbl[[i]] <- CleanHtmlTags(tbl[[i]], CleanHtmlTags=FALSE); 
      if (!identical(NULL, tbl)) write.table(tbl, file, sep='\t', row.names = FALSE, quote = FALSE); 
    } 
  );
  
  session.data;
}