server_visualization_pca <- function(input, output, session, session.data) {
  observeEvent(input$pca.dataset, {
    cll <- session.data$loaded;
    ds  <- geex.load.dataset(cll, input$pca.dataset); 
    if (!identical(NA, ds) & !identical(NULL, cll)) {       
      updateSelectizeInput(session, 'pca.group', choices = names(ds$group), selected = names(ds$group)); 
      updateSelectizeInput(session, 'pca.x', choices=paste('PC', 1:ncol(ds$data[[1]]), sep=''), selected='PC1');
      updateSelectizeInput(session, 'pca.y', choices=paste('PC', 1:ncol(ds$data[[1]]), sep=''), selected='PC2');
      
      sp0<-input$pca.geneset.species;
      sp<-names(geneset[[input$pca.geneset.source]][[input$pca.geneset.coll]]);
      ds<-geex.load.dataset(cll, input$pca.dataset);
      ds.sp<-unique(as.vector(ds$anno$Species));
      sp<-sp[sp %in% ds.sp];
      sp<-c(sp[tolower(sp)!='human'], 'human');
      if (sp0 %in% sp) sp1<-sp0 else sp1<-sp[1];
      updateSelectizeInput(session, 'pca.geneset.species', choices=sp, selected=sp1);
    };
    output$pca.plot <- renderPlotly({plotly_empty();}); 
  });
  observeEvent(input$pca.group, {
    output$pca.plot <- renderPlotly({plotly_empty();}); 
  });
  observeEvent(input$pca.x, {
    output$pca.plot <- renderPlotly({plotly_empty();}); 
  });
  observeEvent(input$pca.y, {
    output$pca.plot <- renderPlotly({plotly_empty();}); 
  });
  observeEvent(input$pca.color, {
    output$pca.plot <- renderPlotly({plotly_empty();}); 
  });
  observeEvent(input$pca.table_rows_selected, {
    output$pca.plot <- renderPlotly({plotly_empty();}); 
  });
  observeEvent(input$pca.geneset.table_rows_selected, {
    output$pca.plot <- renderPlotly({plotly_empty();}); 
  });
  observeEvent(input$pca.clear, {
    cll <- session.data$loaded;
    if (!identical(NULL, cll)) {
      output$pca.table <- DT::renderDataTable({
        tbl <- cll$metadata$Sample[cll$metadata$Sample$Dataset == strsplit(input$pca.dataset, ': ')[[1]][1], , drop=FALSE];
        cbind(ID=rownames(tbl), tbl); 
      }, options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE);
    } else NULL;
    output$pca.plot <- renderPlotly({plotly_empty();}); 
  });
  observeEvent(input$pca.geneset.source, {
    updateSelectizeInput(session, 'pca.geneset.coll', choices=names(geneset[[input$pca.geneset.source]]));
    output$pca.plot <- renderPlotly({plotly_empty();}); 
  });
  observeEvent(input$pca.geneset.coll, {
    cll <- session.data$loaded;
    sp <- names(geneset[[input$pca.geneset.source]][[input$pca.geneset.coll]]);
    if (!identical(NULL, cll)) {
      ds<-geex.load.dataset(cll, input$pca.dataset);
      if (identical(ds , NA)) ds.sp <- c() else ds.sp<-unique(as.vector(ds$anno$Species));
      sp<-sp[sp %in% ds.sp];
      sp<-c(sp[tolower(sp)!='human'], 'human');
    };
    updateSelectizeInput(session, 'pca.geneset.species', choices=sp);
    output$pca.plot <- renderPlotly({plotly_empty();}); 
  });
  observeEvent(input$pca.geneset.species, {
    output$pca.geneset.table <- DT::renderDataTable({
      geex.geneset.table(geneset, input$pca.geneset.source, input$pca.geneset.coll, input$pca.geneset.species);
    }, options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE)
  });
  observeEvent(input$pca.geneset.clear, {
    output$pca.geneset.table <- DT::renderDataTable({ 
      geex.geneset.table(geneset, input$pca.geneset.source, input$pca.geneset.coll, input$pca.geneset.species);
    }, options = dt.options2, rownames=FALSE, selection='multiple', server=TRUE, escape = FALSE)
    output$pca.plot <- renderPlotly({plotly_empty();}); 
  });
  
  observeEvent(input$pca.plot.button, {
    withProgress({
      cll <- session.data$loaded;
      if (length(input$pca.geneset.table_rows_selected) > 0) {
        src <- isolate(input$pca.geneset.source);
        col <- isolate(input$pca.geneset.coll);
        spe <- isolate(input$pca.geneset.species);
        rid <- isolate(CleanHtmlTags(input$pca.geneset.table_rows_selected));
        rid <-rownames(geneset[[src]][[col]][[spe]])[as.integer(rid)];
        if (length(rid) > 0) gn.subset<-unique(unlist(readRDS(paste(GENESET_HOME, '/', tolower(src), '_list.rds', sep=''))[rid], use.names=FALSE)) else
          gn.subset<-c();
      } else gn.subset<-NA;
      
      output$pca.plot <- renderPlotly({
        geex.plot.pca(isolate(cll), isolate(input$pca.x), isolate(input$pca.y), isolate(input$pca.color), 
                      isolate(input$pca.dataset), isolate(input$pca.group),
                      isolate(input$pca.table_rows_selected), isolate(gn.subset));
      })
    }, message="Creating PCA plot ...", detail="\nPlease wait.");
  })

  output$pca.table <- DT::renderDataTable({
    cll <- session.data$loaded;
    if (identical(cll, NULL)) NULL else {
      tbl <- cll$metadata$Sample[cll$metadata$Sample$Dataset == strsplit(input$pca.dataset, ': ')[[1]][1], , drop=FALSE];
      cbind(ID=rownames(tbl), tbl); 
    }
  }, options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE);
  
  session.data;
}