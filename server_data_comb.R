server_data_comb <- function(input, output, session, session.data) {
  observeEvent(input$comb.gs.source, {
    updateSelectizeInput(session, 'comb.gs.coll', choices=names(geneset[[input$comb.gs.source]]));
    output$comb.message <- renderUI({''});
    output$comb.table <- DT::renderDataTable({NULL});
  });
  observeEvent(input$comb.gs.coll, {
    cll <- session.data$loaded;
    sp<-names(geneset[[input$comb.gs.source]][[input$comb.gs.coll]]);
    if (!identical(NULL, cll)) sp<-union(sp[sp %in% as.vector(cll$gene$Species)], 'human');
    updateSelectizeInput(session, 'comb.gs.species', choices=sp, selected='human');
    output$comb.message <- renderUI({''});
    output$comb.table <- DT::renderDataTable({NULL});
  });
  observeEvent(input$comb.gs.species, {
    output$comb.gs.table <- DT::renderDataTable({
      geex.geneset.table(geneset, input$comb.gs.source, input$comb.gs.coll, input$comb.gs.species);
    }, options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE);
    output$comb.message <- renderUI({''});
    output$comb.table <- DT::renderDataTable({NULL});
  });
  observeEvent(input$comb.gs.clear, {
    output$comb.gs.table <- DT::renderDataTable({
      geex.geneset.table(geneset, input$comb.gs.source, input$comb.gs.coll, input$comb.gs.species);
    }, options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE);
    output$comb.message <- renderUI({''});
    output$comb.table <- DT::renderDataTable({NULL});
  });
  observeEvent(input$comb.select.all, { 
    if (!input$comb.select.all) {
      updateCheckboxGroupInput(session, 'comb.desc', choice=GetRowStatTypes(), selected=GetRowStatTypes(), inline=TRUE);
      updateCheckboxGroupInput(
        session, 'comb.anno', choice=expr_anno_columns[-1], selected=expr_anno_columns[length(expr_anno_columns)], inline=TRUE)
    } else {
      updateCheckboxGroupInput(session, 'comb.desc', choice=GetRowStatTypes(), selected=GetRowStatTypes(), inline=TRUE);
      updateCheckboxGroupInput(session, 'comb.anno', choice=expr_anno_columns[-1], selected=expr_anno_columns[-1], inline=TRUE)
    };
    output$comb.message <- renderUI({''});
    output$comb.table <- DT::renderDataTable({NULL});
  });
  observeEvent(input$comb.dataset, {
    output$comb.message <- renderUI({''});
    output$comb.table <- DT::renderDataTable({NULL});
  });
  observeEvent(input$comb.species, {
    output$comb.message <- renderUI({''});
    output$comb.table <- DT::renderDataTable({NULL});
  });
  observeEvent(input$comb.scale, {
    output$comb.message <- renderUI({''});
    output$comb.table <- DT::renderDataTable({NULL});
  });
  observeEvent(input$comb.desc, {
    output$comb.message <- renderUI({''});
    output$comb.table <- DT::renderDataTable({NULL});
  });
  observeEvent(input$comb.anno, {
    output$comb.message <- renderUI({''});
    output$comb.table <- DT::renderDataTable({NULL});
  });
  observeEvent(input$comb.gs.table_rows_selected, {
    output$comb.message <- renderUI({''});
    output$comb.table <- DT::renderDataTable({NULL});
  });
  
  observeEvent(input$comb.table.button, {
    cll <- session.data$loaded;
    ds  <- input$comb.dataset;
    
    output$comb.message <- renderUI({
      gn <- session.data$data$comb;
      if (!identical(NULL, cll)) {
        if (length(ds) > 1) x <- 'data sets' else x <- 'data set'
        h4(HTML(paste(geex.html.msg(length(ds)), x, 'and',  geex.html.msg(nrow(gn)), 'genes')))}
    });
    
    output$comb.table <- DT::renderDataTable({
      withProgress({
        tbl <- geex.combined.table(cll, isolate(input$comb.species), isolate(input$comb.scale), ds);
        setProgress(value=50); 
        if (identical(NULL, cll) | identical(NULL, tbl)) tbl0 <- NULL else {
          if (colnames(tbl)[1] != 'ID') tbl0 <- tbl else {
            cmn <- c('ID', 'Name', isolate(input$comb.anno), isolate(input$comb.desc), 'Mx-Mn', rownames(cll$metadata$Dataset));
            cmn <- cmn[cmn %in% colnames(tbl)];
            tbl <- tbl[, cmn, drop=FALSE];
            
            rid <- isolate(CleanHtmlTags(input$comb.gs.table_rows_selected));
            if (length(rid)==0) tbl0 <- tbl else {
              gs1 <- isolate(input$comb.gs.source); 
              gs2 <- isolate(input$comb.gs.coll); 
              gs3 <- isolate(input$comb.gs.species); 
              rid <- rownames(geneset[[gs1]][[gs2]][[gs3]])[as.integer(rid)]; 
              gs  <- readRDS(paste(GENESET_HOME, '/', tolower(gs1), '_list.rds', sep=''))[rid];
              gn  <- unique(unlist(gs, use.names=FALSE));
              gn  <- gn[gn %in% rownames(tbl)];
              if (length(gn) == 0) tbl0 <- geex.empty.matrix("No more genes after filtering") else 
                tbl0 <- tbl[gn, , drop=FALSE];
            }
          }
        };
      }, message="Calculating column values ...", detail="\nPlease wait for the results.")

      isolate(session.data$data$comb <- tbl0);
      tbl0; 
    }, options = dt.options1, filter='bottom', selection='none', rownames=FALSE, server=TRUE, escape = FALSE);
  });
  

  output$comb.download <- downloadHandler(
    filename = function() { 'gene_datasets.txt' }, 
    content = function(file) { 
      tbl <- session.data$data$comb;
      if (!is.null(tbl)) {
        for (i in 1:ncol(tbl)) tbl[[i]] <- CleanHtmlTags(tbl[[i]], remove.empty = FALSE); 
        write.table(tbl, file, row.names = FALSE, col.names = TRUE, quote = FALSE, sep='\t'); 
      }
    }
  );
  
  session.data;
}