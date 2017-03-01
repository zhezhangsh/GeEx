server_visualization_bar <- function(input, output, session, session.data) {
  observeEvent(input$bar.dataset, {
    cll <- session.data$loaded; 
    ds  <- geex.load.dataset(cll, input$bar.dataset); 
    if (!identical(NULL, ds) & !identical(NA, ds)) {
      updateSelectizeInput(session, 'bar.group', choices=names(ds$group), selected=names(ds$group));
      output$bar.table <- DT::renderDataTable({ 
        cll$browse_table$Gene[rownames(ds$anno), 1:5, drop=FALSE];
      }, options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE);
    } else {
      output$bar.table <- DT::renderDataTable({ NULL }, options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE);
    };
    output$bar.message <- renderUI({''});
    output$bar.plot <- renderPlotly({plotly_empty();});
    output$bar.stat <- DT::renderDataTable({NULL});
  });
  observeEvent(input$bar.group, {
    output$bar.message <- renderUI({''});
    output$bar.plot <- renderPlotly({plotly_empty();});
    output$bar.stat <- DT::renderDataTable({NULL});
  });
  observeEvent(input$bar.mean, {
    output$bar.message <- renderUI({''});
    output$bar.plot <- renderPlotly({plotly_empty();});
    output$bar.stat <- DT::renderDataTable({NULL});
  });
  observeEvent(input$bar.scale, {
    output$bar.message <- renderUI({''});
    output$bar.plot <- renderPlotly({plotly_empty();});
    output$bar.stat <- DT::renderDataTable({NULL});
  });
  observeEvent(input$bar.color, {
    output$bar.message <- renderUI({''});
    output$bar.plot <- renderPlotly({plotly_empty();});
    output$bar.stat <- DT::renderDataTable({NULL});
  });
  observeEvent(input$bar.table_rows_selected, {
    output$bar.message <- renderUI({''});
    output$bar.plot <- renderPlotly({plotly_empty();});
    output$bar.stat <- DT::renderDataTable({NULL});
  });
  observeEvent(input$bar.clear, {
    cll <- session.data$loaded
    ds  <- input$bar.dataset;
    output$bar.table <- DT::renderDataTable({
      if (!identical(NULL, cll) & !identical(NA, ds)) {
        anno <- geex.load.dataset(cll, ds)$anno;
        cll$browse_table$Gene[rownames(anno), 1:5, drop=FALSE];
      } else NULL
    }, options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE);
    updateSelectInput(session, 'bar.dataset', choices=cll$extra$longname$dataset, selected=ds);
    output$bar.message <- renderUI({''});
    output$bar.plot <- renderPlotly({plotly_empty();});
    output$bar.stat <- DT::renderDataTable({NULL});
  });
  observeEvent(input$bar.plot.button, {
    withProgress({
      cll <- session.data$loaded;
      
      ds <- isolate(input$bar.dataset);
      rd <- isolate(CleanHtmlTags(input$bar.table_rows_selected)); 
      cl <- isolate(input$bar.color); 
      sc <- isolate(input$bar.scale); 
      gp <- isolate(input$bar.group);
      mn <- isolate(input$bar.mean); 
      
      output$bar.message <- renderUI({
        if (!identical(NULL, cll) & !identical(NA, ds) & !identical(NULL, ds) & length(rd)>0) {
          if (length(gp) == 0) h4(HTML(geex.html.msg(msg.nogroup))) else {
            gn.nm <- as.vector(geex.load.dataset(cll, ds)$anno[as.integer(rd), 'Name']);
            h4(HTML(geex.html.msg(paste(gn.nm, collapse='/'))));
          }
        } else if (length(rd) == 0) h4(HTML(geex.html.msg(msg.nogene))) else h4(HTML(""));
      });
      
      output$bar.plot <- renderPlotly({
        dd <- geex.load.dataset(cll, ds);
        setProgress(value = 20); 
        geex.plot.bar(dd, rd, sc, cl, gp, mn); 
      });
      
      output$bar.stat <- DT::renderDataTable({
        selected <- geex.select.gene(cll, ds, gp, rd);
        selected$table;
      }, options = dt.options6, rownames=FALSE, server=TRUE, escape = FALSE);
    }, message="Creating bar plot ...", detail="\nPlease wait.");
  });

  ########################## Gene dictionary
  observeEvent(input$lookup.option, {
    cll <- session.data$loaded
    if (input$lookup.option & !identical(NULL, cll)) {
      output$lookup.table<-DT::renderDataTable({ NULL; }, options=list(dom='t'));
      updateSelectizeInput(session, "bar.lookup.species", '', choices=unique(cll$gene$Species), selected='human')
    } else {
      # updateTextInput(session, "bar.lookup.key", NULL, NULL);
      updateSelectizeInput(session, "bar.lookup.species", NULL, NULL, NULL);
    };
  });
  observeEvent(input$bar.lookup.key, {
    ky <- input$bar.lookup.key; 
    output$bar.lookup.table <- DT::renderDataTable({
      if (ky!='' & !is.null(ky)) { 
        ds <- geex.load.dataset(session.data$loaded, input$bar.dataset); 
        geex.lookup.gene(ds$anno, input$bar.lookup.species, ky, GENE_HOME) 
      } else NULL
    }, options = dt.options4, selection='none', rownames=FALSE, server=TRUE, escape = FALSE);
  });
  
  session.data;
}
  