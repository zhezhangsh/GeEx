server_visualization_two <- function(input, output, session, session.data) {
  observeEvent(input$x.dataset, {
    ch.x <- geex.longname.set2groups(session.data$loaded, input$x.dataset);
    updateSelectizeInput(session, 'x.group', choices=ch.x);
    output$two.message <- renderUI({''});
    output$two.plot <- renderPlotly({plotly_empty();});
    output$two.geneset.stat <- DT::renderDataTable({NULL});
  });
  observeEvent(input$y.dataset, {
    ch.y<-geex.longname.set2groups(session.data$loaded, input$y.dataset);
    updateSelectizeInput(session, 'y.group', choices=ch.y, selected=ch.y[length(ch.y)]);
    output$two.message <- renderUI({''});
    output$two.plot <- renderPlotly({plotly_empty();});
    output$two.geneset.stat <- DT::renderDataTable({NULL});
  });
  observeEvent(input$x.group, {
    cll <- session.data$loaded;
    sp  <- names(geneset[[input$two.source]][[input$two.coll]]);
    sp  <- unique(sp[sp %in% geex.get2group.species(cll, input$x.group, input$y.group)]);
    
    if (input$x.sample == 'Group mean') geex.get2data(input, output, session, session.data); 
    updateSelectizeInput(session, 'x.sample', choices=c('Group mean', geex.longname.group2samples(cll, input$x.group)));
    if (!identical(sp, input$two.species)) updateSelectizeInput(session, 'two.species', choices=sp);
    output$two.message <- renderUI({''});
    output$two.plot <- renderPlotly({plotly_empty();});
    output$two.geneset.stat <- DT::renderDataTable({NULL});
  });
  observeEvent(input$y.group, {
    cll <- session.data$loaded;
    sp  <- names(geneset[[input$two.source]][[input$two.coll]]);
    sp  <- unique(sp[sp %in% geex.get2group.species(cll, input$x.group, input$y.group)]);
    
    if (input$y.sample == 'Group mean') geex.get2data(input, output, session, session.data); 
    updateSelectizeInput(session, 'y.sample', choices=c('Group mean', geex.longname.group2samples(cll, input$y.group)));
    if (!identical(sp, input$two.species)) updateSelectizeInput(session, 'two.species', choices=sp); 
    output$two.message <- renderUI({''});
    output$two.plot <- renderPlotly({plotly_empty();});
    output$two.geneset.stat <- DT::renderDataTable({NULL});
  });
  observeEvent(input$x.sample, { 
    # geex.get2data(input, output, session, session.data); 
    output$two.message <- renderUI({''});
    output$two.plot <- renderPlotly({plotly_empty();});
    output$two.geneset.stat <- DT::renderDataTable({NULL});
  });
  observeEvent(input$y.sample, { 
    # geex.get2data(input, output, session, session.data); 
    output$two.message <- renderUI({''});
    output$two.plot <- renderPlotly({plotly_empty();});
    output$two.geneset.stat <- DT::renderDataTable({NULL});
  }); 
  observeEvent(input$two.scale, { 
    # geex.get2data(input, output, session, session.data); 
    output$two.message <- renderUI({''});
    output$two.plot <- renderPlotly({plotly_empty();});
    output$two.geneset.stat <- DT::renderDataTable({NULL});
  }); 
  observeEvent(input$two.type, { 
    # geex.get2data(input, output, session, session.data); 
    output$two.message <- renderUI({''});
    output$two.plot <- renderPlotly({plotly_empty();});
    output$two.geneset.stat <- DT::renderDataTable({NULL});
  }); 
  observeEvent(input$two.filter.geneset, { 
    if (!input$two.filter.geneset) gs <- NULL else  
      gs <- geex.geneset.table(geneset, input$two.source, input$two.coll, input$two.species);
    session.data$two$geneset <- gs; 
    
    output$two.table.geneset <- DT::renderDataTable({ gs; 
    }, options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE);
  });
  observeEvent(input$two.source, {
    updateSelectizeInput(session, 'two.coll', choices=names(geneset[[input$two.source]]));
    output$two.message <- renderUI({''});
    output$two.plot <- renderPlotly({plotly_empty();});
    output$two.geneset.stat <- DT::renderDataTable({NULL});
  });
  observeEvent(input$two.coll, {
    cll <- session.data$loaded;
    sp  <- names(geneset[[input$two.source]][[input$two.coll]]);
    sp  <- unique(sp[sp %in% geex.get2group.species(cll, input$x.group, input$y.group)]);
    updateSelectizeInput(session, 'two.species', choices=sp);
    
    session.data$two$geneset <- geex.geneset.table(geneset, input$two.source, input$two.coll, input$two.species);
    output$two.table.geneset <- DT::renderDataTable({ session.data$two$geneset;
    }, options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE);
    output$two.message <- renderUI({''});
    output$two.plot <- renderPlotly({plotly_empty();});
    output$two.geneset.stat <- DT::renderDataTable({NULL});
  });  
  observeEvent(input$two.species, {
    output$two.message <- renderUI({''});
    output$two.plot <- renderPlotly({plotly_empty();});
    output$two.geneset.stat <- DT::renderDataTable({NULL});
  });
  observeEvent(input$two.clear.geneset, {
    output$two.table.geneset <- DT::renderDataTable({ 
      session.data$two$geneset; 
    }, options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE);
    output$two.message <- renderUI({''});
    output$two.plot <- renderPlotly({plotly_empty();});
    output$two.geneset.stat <- DT::renderDataTable({NULL});
  });
  observeEvent(input$two.table.geneset_rows_selected, {
    output$two.message <- renderUI({''});
    output$two.plot <- renderPlotly({plotly_empty();});
    output$two.geneset.stat <- DT::renderDataTable({NULL});
  });
  # observeEvent(input$two.filter.gene, {
  #   if (input$two.filter.gene) geex.get2data(input, output, session, session.data) else {
  #     output$two.table.gene <- DT::renderDataTable({ NULL;
  #     }, options=dt.options2, rownames=FALSE, filter='bottom', server=TRUE, escape = FALSE);
  #   };
  # });
  # observeEvent(input$two.clear.gene, {
  #   output$two.table.gene <- DT::renderDataTable({
  #     session.data$two$gene; 
  #   }, options=dt.options2, rownames=FALSE, filter='bottom', server=TRUE, escape = FALSE);
  #   output$two.message <- renderUI({''});
  #   output$two.plot <- renderPlotly({plotly_empty();});
  #   output$two.geneset.stat <- DT::renderDataTable({NULL});
  # });   
  # observeEvent(input$two.table.gene_rows_selected, {
  #   output$two.message <- renderUI({''});
  #   output$two.plot <- renderPlotly({plotly_empty();});
  #   output$two.geneset.stat <- DT::renderDataTable({NULL});
  # });
  
  observeEvent(input$two.plot.button, {
    withProgress({
      output$two.message <- renderUI({
        x.smp <- isolate(input$x.sample); 
        y.smp <- isolate(input$y.sample);
        x.grp <- isolate(input$x.group);
        y.grp <- isolate(input$y.group);
        
        if (x.grp=='' | y.grp=='') msg <- '' else {
          if (x.smp=='Group mean' | x.smp=='') lab.x <- x.grp else lab.x <- x.smp;
          if (y.smp=='Group mean' | y.smp=='') lab.y <- y.grp else lab.y <- y.smp;
          msg <- paste(geex.html.msg(lab.x), 'vs.', geex.html.msg(lab.y)); 
        }
        
        list(h4(HTML(msg)));
      });
      
      output$two.plot <- renderPlotly({
        cll <- isolate(session.data$loaded);
        typ <- isolate(input$two.type);
        dat <- isolate(session.data$two$gene); 
        
        if (!identical(NULL, cll) & !identical(NULL, dat) & typ!='') {
          id1 <- isolate(rownames(session.data$two$gene)[input$two.table.gene_rows_selected]); 
          id1 <- dat[intersect(id1, rownames(dat)), 2];
          
          id2 <- isolate(CleanHtmlTags(session.data$two$geneset[input$two.table.geneset_rows_selected, 1])); 
          if (length(id2) > 0) {
            id2 <- isolate(readRDS(paste(GENESET_HOME, '/', tolower(input$two.source), '_list.rds', sep=''))[id2]); 
            gns <- isolate(session.data$two$geneset);
            rownames(gns) <- CleanHtmlTags(gns[, 1]);  
            nms <- gns[names(id2), 2]; 
            id2 <- lapply(id2, function(x) dat[intersect(x, rownames(dat)), 2]);
            names(id2) <- nms; 
            if (length(id2) > 0) id2 <- id2[sapply(id2, length)>0]; 
          }; 
          
          rownames(dat) <- paste(as.vector(dat[, 2]), dat[, 1], sep=':');
          dat <- dat[, 3:4];
          colnames(dat) <- cll$mapping$id2longname[colnames(dat)];
          
          p <- PlotlyPairDiff(dat, typ, id2, id1); 
          if (typ == PlotlyPairDiffTypes()[1]) {
            tt <- paste(paste("Corr =", round(cor(dat[, 1], dat[, 2]), 4)), paste('N =', nrow(dat)), sep='; '); 
            p <- layout(p, title=tt, margin=list(t=30)); 
          };
          p; 
        } else plotly_empty();
      });
      
      output$two.geneset.stat <- DT::renderDataTable({
        dat <- isolate(session.data$two$gene); 
        gst <- isolate(session.data$two$geneset);
        sel <- isolate(input$two.table.geneset_rows_selected); 
        if (!identical(NULL, dat) & !identical(NULL, gst) & length(sel)>0) {
          id2 <- CleanHtmlTags(gst[sel, 1]); 
          id2 <- isolate(readRDS(paste(GENESET_HOME, '/', tolower(input$two.source), '_list.rds', sep=''))[id2]); 
          rownames(gst) <- CleanHtmlTags(gst[, 1]); 
          nms <- gst[names(id2), 2];
          id2 <- lapply(id2, function(x) intersect(x, rownames(dat)));
          names(id2) <- nms;
          if (length(id2) > 0) id2 <- id2[sapply(id2, length)>0];
          if (length(id2) > 0) {
            stat <- lapply(id2, function(id) {
              s <- c(length(id), mean(dat[id, 4]-dat[id, 3]), median(dat[id, 4]-dat[id, 3])); 
              if (s[1]>1) {
                s <- c(s, t.test(dat[id, 4], dat[id, 3], paired = TRUE)$p.value[[1]],
                       wilcox.test(dat[id, 4], dat[id, 3], paired = TRUE)$p.value[[1]]);
              } else s <- c(s, NA, NA);
            });
            tb <- do.call('rbind', stat);
            dt <- dat[setdiff(rownames(dat), unlist(id2)), 3:4, drop=FALSE]; 
            tb <- rbind(tb, c(nrow(dt), mean(dt[,2]-dt[,1]), median(dt[,2]-dt[,1]), NA, NA));
            rownames(tb)[nrow(tb)] <- 'All other genes';
            colnames(tb) <- c('N', 'Mean', 'Median', 'p_T', 'p_RST'); 
            data.frame(ID=rownames(tb), FormatNumeric(tb)); 
          }
        }
      }, options = dt.options6, rownames=FALSE, server=TRUE, escape = FALSE);
    }, message="Creating plot ...", detail="\nPlease wait.");
  });
  
  session.data;
}