server_visualization_geneset <- function(input, output, session, session.data) {
  observeEvent(input$geneset.dataset, {
    cll <- session.data$loaded
    ds <- geex.load.dataset(cll, input$geneset.dataset);
    if (!identical(NA, ds) & !identical(cll, NULL)) { 
      sp0 <- input$geneset.species;
      sp  <- names(geneset[[input$geneset.source]][[input$geneset.coll]]);
      ds  <- geex.load.dataset(cll, input$geneset.dataset);
      ds.sp <- unique(as.vector(ds$anno$Species));
      sp <- sp[sp %in% ds.sp];
      sp <- c(sp[tolower(sp)!='human'], 'human');
      if (sp0 %in% sp) sp1<-sp0 else sp1<-sp[1];
      updateSelectizeInput(session, 'geneset.species', choices=sp, selected=sp1);
      updateSelectizeInput(session, 'geneset.group', choices=names(ds$group), selected=names(ds$group));
    }; 
    output$geneset.message <- renderUI({''});
    output$geneset.plot <- renderPlotly({plotly_empty();});
    output$geneset.stat <- DT::renderDataTable({NULL});
  });
  observeEvent(input$geneset.group, {
    output$geneset.message <- renderUI({''});
    output$geneset.plot <- renderPlotly({plotly_empty();});
    output$geneset.stat <- DT::renderDataTable({NULL});
  });
  observeEvent(input$geneset.type, {
    output$geneset.message <- renderUI({''});
    output$geneset.plot <- renderPlotly({plotly_empty();});
    output$geneset.stat <- DT::renderDataTable({NULL});
  });  
  observeEvent(input$geneset.color, {
    output$geneset.message <- renderUI({''});
    output$geneset.plot <- renderPlotly({plotly_empty();});
    output$geneset.stat <- DT::renderDataTable({NULL});
  });
  observeEvent(input$geneset.scale, {
    output$geneset.message <- renderUI({''});
    output$geneset.plot <- renderPlotly({plotly_empty();});
    output$geneset.stat <- DT::renderDataTable({NULL});
  });
  observeEvent(input$geneset.normalize, {
    output$geneset.message <- renderUI({''});
    output$geneset.plot <- renderPlotly({plotly_empty();});
    output$geneset.stat <- DT::renderDataTable({NULL});
  });
  
  observeEvent(input$geneset.source, {
    updateSelectizeInput(session, 'geneset.coll', choices=names(geneset[[input$geneset.source]]));
    output$geneset.message <- renderUI({''});
    output$geneset.plot <- renderPlotly({plotly_empty();});
    output$geneset.stat <- DT::renderDataTable({NULL});
  });
  observeEvent(input$geneset.coll, {
    cll <- session.data$loaded;
    ds<-geex.load.dataset(cll, input$pca.dataset);
    if (!identical(NULL, cll) & !identical(NA, ds)) {
      if (identical(ds , NA)) ds.sp <- c() else ds.sp<-unique(as.vector(ds$anno$Species));
      sp <- names(geneset[[input$pca.geneset.source]][[input$pca.geneset.coll]]);
      sp<-sp[sp %in% ds.sp];
      sp<-c(sp[tolower(sp)!='human'], 'human');
      updateSelectizeInput(session, 'geneset.species', choices=sp);
    };
    output$geneset.message <- renderUI({''});
    output$geneset.plot <- renderPlotly({plotly_empty();});
    output$geneset.stat <- DT::renderDataTable({NULL});
  });
  observeEvent(input$geneset.species, {
    output$geneset.table <- DT::renderDataTable({
      geex.geneset.table(geneset, input$geneset.source, input$geneset.coll, input$geneset.species);
    }, options = dt.options2, rownames=FALSE, selection='single', server=TRUE, escape = FALSE);
    output$geneset.message <- renderUI({''});
    output$geneset.plot <- renderPlotly({plotly_empty();});
    output$geneset.stat <- DT::renderDataTable({NULL});
  });
  observeEvent(input$geneset.clear, {
    output$geneset.table <- DT::renderDataTable({ 
      geex.geneset.table(geneset, input$geneset.source, input$geneset.coll, input$geneset.species);
    }, options = dt.options2, rownames=FALSE, selection='single', server=TRUE, escape = FALSE);
    output$geneset.message <- renderUI({''});
    output$geneset.plot <- renderPlotly({plotly_empty();});
    output$geneset.stat <- DT::renderDataTable({NULL});
  });
  observeEvent(input$geneset.table_rows_selected, {
    output$geneset.message <- renderUI({''});
    output$geneset.plot <- renderPlotly({plotly_empty();});
    output$geneset.stat <- DT::renderDataTable({NULL});
  });
  
  observeEvent(input$geneset.plot.button, {
    withProgress({
      cll <- isolate(session.data$loaded);
      src <- isolate(input$geneset.source);
      gst <- isolate(input$geneset.coll);
      spe <- isolate(input$geneset.species);
      
      output$geneset.message<-renderUI({
        rid <- isolate(input$geneset.table_rows_selected);
        
        if (!identical(NULL, cll) & length(rid)>0) {
          if (length(input$geneset.group) == 0) h3(HTML(geex.html.msg(msg.nogroup))) else { 
            nm<-as.vector(geneset[[src]][[gst]][[spe]][as.integer(rid), 'Name'])
            h4(HTML(geex.html.msg(nm)));            
          }
        } else if(length(rid) == 0) h4(HTML(geex.html.msg(msg.nogeneset))) else h4(HTML(geex.html.msg("")));
      });
      
      setProgress(value=5);
      
      output$geneset.plot <- renderPlotly({
        rid0 <- isolate(input$geneset.table_rows_selected);
        rid  <- isolate(input$geneset.table_row_last_clicked);
        
        if (!identical(NULL, cll) & length(rid0)>0 & length(rid)>0) {
          if (rid0[length(rid0)]==rid) {
            grp <- isolate(input$geneset.group);
            rid <- isolate(CleanHtmlTags(input$geneset.table_rows_selected));
            rid <- rownames(geneset[[src]][[gst]][[spe]])[as.integer(rid)];
            rid <- rid[length(rid)];
            typ <- isolate(input$geneset.type);
            scl <- isolate(input$geneset.scale);
            col <- isolate(input$geneset.color);
            nom <- isolate(input$geneset.normalize);
            
            setProgress(value = 10);
            
            gs  <- readRDS(paste(GENESET_HOME, '/', tolower(src), '_list.rds', sep=''))[rid]; 
            names(gs) <- as.vector(geneset[[src]][[gst]][[spe]][names(gs), 'Name']);
            
            setProgress(value = 60);
            
            geex.plot.geneset(cll, grp, typ, scl, col, nom, gs[[1]]); 
          } else plotly_empty(); 
        } else plotly_empty(); 
      });
      
      setProgress(value=75);
      
      output$geneset.stat <- DT::renderDataTable({
        ds  <- isolate(input$geneset.dataset);
        rid <- isolate(input$geneset.table_rows_selected); 
        grp <- isolate(input$geneset.group); 
        
        if (!identical(NULL, cll) & length(ds)>0 & length(grp)>0 & length(rid)>0) {
          rid <- rownames(geneset[[src]][[gst]][[spe]])[as.integer(rid)];
          gs  <- readRDS(paste(GENESET_HOME, '/', tolower(src), '_list.rds', sep=''))[[rid]]; 
          anno <- cll$browse_table$Gene;
          anno <- anno[rownames(anno) %in% gs, 1:5, drop=FALSE]; 
          data <- cll$gex_combined$logged[rownames(anno), cll$mapping$longname2id[grp], drop=FALSE]; 
          d <- cll$gex_combined$percentile[rownames(anno), colnames(data), drop=FALSE];
          colnames(d) <- paste(colnames(d), 'Percentile', sep='_');
          cbind(anno, data, d); 
        } else NULL; 
        # geex.select.geneset(cll, ds, input$geneset.group, rid);
      }, options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE);
    }, message="Creating bar plot ...", detail="\nPlease wait.");
  });
  
  session.data;
}