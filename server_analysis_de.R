server_analysis_de <- function(input, output, session, session.data) {
  output$de.info <- renderUI({
    sel <- DeMethodMeta[DeMethodMeta[, 1]==input$de.method, , drop=FALSE];
    if (nrow(sel) > 0) h6(HTML(paste('<u>', AddHref(geex.html.msg(sel$Test), sel$Link), '</u>', sep=''))) else '';
  }); 
  
  observeEvent(input$de.dataset1, {
    ch.1 <- geex.longname.set2groups(session.data$loaded, input$de.dataset1);
    updateSelectizeInput(session, 'de.group1', choices = ch.1, selected = character(0));
  });
  observeEvent(input$de.dataset2, {
    ch.2<-geex.longname.set2groups(session.data$loaded, input$de.dataset2);
    updateSelectizeInput(session, 'de.group2', choices = ch.2, selected = character(0));
  });
  
  observeEvent(input$de.group1, {
    cll <- session.data$loaded;
    g1  <- input$de.group1; 
    g2  <- input$de.group2; 
    if (!identical('', g1) & !identical('', g2) & !identical(NULL, cll)) {
      sp <- unique(c(input$de.species, geex.de.get2species(cll, g1, g2, session.data$de$species)));
      updateSelectInput(session, 'de.species', choices = sp[sp!='']);
    }
  }); 
  observeEvent(input$de.group2, {
    cll <- session.data$loaded;
    g1  <- input$de.group1;
    g2  <- input$de.group2; 
    if (!identical('', g1) & !identical('', g2) & !identical(NULL, cll)) {
      sp <- unique(c(input$de.species, geex.de.get2species(cll, g1, g2, session.data$de$species)));
      updateSelectInput(session, 'de.species', choices = sp[sp!='']);
    }
  }); 
  
  output$de.add.msg <- renderUI({ h6(HTML("Click to add a new comparison")); });
  observeEvent(input$de.add, {
    if (!session.data$de$startover) {
      cll <- session.data$loaded;
      g1  <- input$de.group1;
      g2  <- input$de.group2;
      mth <- input$de.method;
      spe <- input$de.species;
      if (identical(NULL, cll)) output$de.add.msg <- renderUI({h6(HTML("Data collection not loaded"))}) else
        if (identical('', g1)) output$de.add.msg <- renderUI({h6(HTML("Control group not selected"))}) else
          if (identical('', g2)) output$de.add.msg <- renderUI({h6(HTML("Case group not selected"))}) else
            if (identical('', mth)) output$de.add.msg <- renderUI({h6(HTML("DE method not selected"))}) else
              if (identical('', spe)) output$de.add.msg <- renderUI({h6(HTML("Species not selected"))}) else {
                n1 <- cll$metadata$Group[cll$mapping$longname2id[g1], 'Num_Sample'];
                n2 <- cll$metadata$Group[cll$mapping$longname2id[g2], 'Num_Sample'];
                if (n1>1 & n2>1) {
                  session.data$de$comparison <- comp <- geex.de.comp(cll, g1, g2, mth, spe, session.data);
                  updateSelectizeInput(session, 'de.remove.index', choices = comp[[1]]); 
                  output$de.add.msg <- renderUI({ h6(HTML("Click to add a new comparison")); });
                  output$de.comparisons <- DT::renderDataTable({ comp;
                  }, options = dt.options3, rownames=FALSE, server=TRUE, escape = FALSE);                  
                } else {
                  output$de.add.msg <- renderUI({ h6(HTML(
                    "<font color='red'>Comparison not added; require replicates in both groups</font>")); });
                }
              }
    } else output$de.add.msg <- renderUI({ h6(HTML("Analysis already run")); });
    
  });
  
  output$de.remove.msg <- renderUI({ h6(HTML("Click to remove comparison")); });
  observeEvent(input$de.remove, {
    if (!session.data$de$startover) {
      x <- input$de.remove.index;
      y <- session.data$de$comparison;
      if (identical(NULL, y)) output$de.remove.msg <- renderUI({h6(HTML("No comparisons"))}) else 
        if (identical('', x)) output$de.remove.msg <- renderUI({h6(HTML("Comparison not selected"))}) else {
          if (nrow(y) == 1) tbl <- NULL else {
            tbl <- y[y[, 1]!=x, , drop=FALSE]; 
            rownames(tbl) <- 1:nrow(tbl); 
            tbl[[1]] <- paste('Comparison', 1:nrow(tbl), sep='_'); 
          }; saveRDS(y, 'x.rds'); 
          if (identical(NULL, tbl)) ch <- '' else ch <- tbl[[1]]; 
          output$de.remove.msg <- renderUI({ h6(HTML("Click to remove comparison")); }); 
          updateSelectizeInput(session, 'de.remove.index', choices = ch); 
          output$de.comparisons <- DT::renderDataTable({ tbl;
          }, options = dt.options3, rownames=FALSE, server=TRUE, escape = FALSE);
          session.data$de$comparison <- tbl; 
        }
    } else {
      output$de.remove.msg <- renderUI({ h6(HTML("Analysis already run")); }); 
    } 
  });
  
  output$de.run.msg <- renderUI({ 
    if (!session.data$de$startover) h6(HTML("Click to run DE analysis")) else h6(HTML("Click to start over"))
  });
  observeEvent(input$de.run, {
    if (session.data$de$startover) {
      session.data$de$comparison <- NULL;
      session.data$de$result <- NULL;
      session.data$de$startover <- FALSE;
      
      output$de.comparisons <- DT::renderDataTable({ NULL;
      }, options = dt.options3, rownames=FALSE, server=TRUE, escape = FALSE);
      output$de.run.msg <- renderUI({ h6(HTML("Click to run DE analysis")); });
      output$de.add.msg <- renderUI({ h6(HTML("Click to add a new comparison")); });
      output$de.remove.msg <- renderUI({ h6(HTML("Click to remove comparison")); }); 
      
      updateCheckboxInput(session, 'de1', value=TRUE);
      updateSelectizeInput(session, 'de.remove.index', choices = ''); 
      updateSelectizeInput(session, 'de.result.index', choices = ''); 
      updateActionButton(session, 'de.run', 'Run analysis'); 
    } else {
      cll <- session.data$loaded; 
      cmp <- session.data$de$comparison; 
      if (!identical(NULL, cmp) & !identical(NULL, cll)) {
        dat <- geex.de.input(cll, cmp); 
        res <- lapply(dat, function(d) DeWrapper(mtrx=d[[1]], grps=d[[2]], mthd=d[[3]])$results$stat); 
        session.data$de$result <- res; 
        updateSelectizeInput(session, 'de.result.index', NULL, choices=names(res)); 
        updateActionButton(session, 'de.run', 'Start over'); 
        updateCheckboxInput(session, 'de1', value=FALSE);
        
        session.data$de$startover <- TRUE;
        
        output$de.run.msg <- renderUI({ h6(HTML("Click to start over")); });
        output$de.add.msg <- renderUI({ h6(HTML("Click to add a new comparison")); });
        output$de.remove.msg <- renderUI({ h6(HTML("Click to remove comparison")); });       
      };
    };
    output$demeta.result.table <- DT::renderDataTable({ NULL });
    output$demeta.result.msg <- renderUI({ ""; });
    updateCheckboxInput(session, 'demeta1', value=FALSE);
    isolate(session.data$de$meta <- NULL); 
  });
  
  output$de.result.table <- DT::renderDataTable({
    cll <- session.data$loaded;
    res <- session.data$de$result; 
    ind <- input$de.result.index;
    if (identical(NULL, cll) | identical(NULL, res) | identical('', ind)) NULL else {
      tb <- res[[ind]]; 
      an <- cll$browse_table$Gene[rownames(tb), c(1, 3), drop=FALSE]; 
      tbl <- data.frame(an[, 1:2], FormatNumeric(tb), stringsAsFactors = FALSE); 
      names(tbl) <- c('ID', 'Name', colnames(tb)); 
      tbl; 
    } 
  }, options = dt.options8, rownames=FALSE, server=TRUE, escape = FALSE, selection = 'none');
  
  output$de.download <- downloadHandler(
    filename = function() { paste(input$de.result.index, '.txt', sep=''); },  
    content  = function(file) { 
      tb  <- session.data$de$result[[input$de.result.index]]; 
      cll <- session.data$loaded;
      an  <- cll$browse_table$Gene[rownames(tb), c(1, 3, 7), drop=FALSE]; 
      tbl <- data.frame(an[, 1:2], FormatNumeric(tb), an[, 3], stringsAsFactors = FALSE); 
      names(tbl) <- c('ID', 'Name', colnames(tb), 'Title'); 
      for (j in 1:ncol(tbl)) tbl[[j]] <- CleanHtmlTags(tbl[[j]], remove.empty = FALSE); 
      write.table(tbl, file, sep='\t', row.names = FALSE, quote = FALSE)
    } 
  );
  
  output$de.download.all <- downloadHandler(
    filename = function() { 'result.zip'; },  
    content  = function(file) { 
      res <- session.data$de$result;
      fns <- sapply(names(res), function(i) {
        tb <- res[[i]];
        an <- cll$browse_table$Gene[rownames(tb), c(1, 3, 7), drop=FALSE]; 
        tbl <- data.frame(an[, 1:2], FormatNumeric(tb), an[, 3], stringsAsFactors = FALSE); 
        names(tbl) <- c('ID', 'Name', colnames(tb), 'Title'); 
        f <- paste(tempdir(), '/', i, '.txt', sep='');
        for (j in 1:ncol(tbl)) tbl[[j]] <- CleanHtmlTags(tbl[[j]], remove.empty = FALSE); 
        write.table(tbl, f, sep='\t', row.names = FALSE, quote = FALSE); 
        f; 
      }); 
      zip(zipfile=file, files=fns, zip='zip'); 
    },
    contentType = "application/zip"
  );
  
  # Plots
  output$de.plot <- renderPlotly({
    res <- session.data$de$result;
    ind <- input$de.result.index;
    typ <- input$de.plot.type;
    
    if (identical(NULL, res) | ind=='' | typ=='') plotly_empty(type='scatter', mode='markers') else {
      tbl <- res[[ind]]; 
      grp <- sub('^Mean_', '', colnames(tbl)[1:2]); 
      if (!is.null(tbl) & typ=='5') PlotPA(rowMeans(tbl[, 1:2], na.rm=TRUE), tbl[, 5], npoints=round(nrow(tbl)/20), plotly=TRUE) else 
        if (!is.null(tbl) & typ=='4') PlotMA(rowMeans(tbl[, 1:2], na.rm=TRUE), tbl[, 4], npoints=round(nrow(tbl)/20), plotly=TRUE) else 
          if (!is.null(tbl) & typ=='3') PlotVolcano(tbl[, 4], tbl[, 5], plotly=TRUE, npoints=round(nrow(tbl)/20)) else 
            if (!is.null(tbl) & typ=='1') PlotPValue(tbl[, 5], plotly=TRUE) else 
              if (!is.null(tbl) & typ=='2') PlotFDR(tbl[, 6], plotly=TRUE) else plotly_empty(type='scatter', mode='markers'); 
    }
  });
  
  session.data;
}
  