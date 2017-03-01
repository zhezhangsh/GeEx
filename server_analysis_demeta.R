server_analysis_demeta <- function(input, output, session, session.data) {
  
  # observeEvent(input$demeta.select.all, {
  #   tbl <- session.data$de$comparison; 
  #   output$demeta.comparisons <- DT::renderDataTable({ tbl; 
  #   }, options = dt.options3, rownames=FALSE, server=TRUE, escape = FALSE);
  # }); 
  
  output$demeta.comparisons <- DT::renderDataTable({ session.data$de$comparison; 
  }, options = dt.options3, rownames=FALSE, server=TRUE, escape = FALSE);
  
  observeEvent(input$demeta.method.p, {
    mth <- input$demeta.method.p; 
    if (mth=='fisher') msg <- c("Fisher's meta analysis", "https://en.wikipedia.org/wiki/Fisher's_method") else 
      if (mth=='simes') msg <- c("Simes' method", "http://www-stat.wharton.upenn.edu/~steele/Courses/956/ResourceDetails/MultipleComparision/Simes86pdf.pdf") else
        if (mth=='bonferroni') msg <- c("Bonferroni correction", "https://en.wikipedia.org/wiki/Bonferroni_correction") else
          if (mth=='max') msg <- c("Maximal p value of all comparisons", '') else 
            if (mth=='min') msg <- c("Minimal p value of all comparisons", '') else 
              if (mth=='average') msg <- c('Logarithmic mean of all p values', '') else msg <- c('', '');
              if (msg[2] == '') msg <- h6(HTML(geex.html.msg(msg[1]))) else 
                msg <- h6(HTML(paste('<u>', AddHref(geex.html.msg(msg[1]), msg[2]), '</u>', sep='')))
              output$demeta.msg.p <- renderUI({ msg; });  
  });
  
  observeEvent(input$demeta.method.m, {
    mth <- input$demeta.method.m; 
    if (mth=='MD') msg <- c("Mean difference") else 
      if (mth=='SMD') msg <- c("Standardized mean difference") else
        if (mth=='ROM') msg <- c("Ratio of means") else msg <- ""
        output$demeta.msg.m <- renderUI({ h6(HTML(geex.html.msg(msg))); });  
  });
  
  output$demeta.run.msg <- renderUI({ h6(HTML("Click to run meta-analysis")); });
  
  output$demeta.result.msg <- renderUI({
    res <- session.data$de$meta;
    if (identical(NULL, res)) h4(HTML(geex.html.msg("Run meta-analysis to get results."))) else ''
  });
  
  observeEvent(input$demeta.result.n, {
    res <- session.data$de$meta;
    if (!identical(NULL, res)) {
      sel <- input$demeta.result.option; 
      tbl <- res[[as.integer(sel)]];
      n   <- as.integer(input$demeta.result.n);
      output$demeta.result.table <- DT::renderDataTable({ FormatNumeric(tbl[tbl$N_Comp>=n, , drop=FALSE]);
      }, options = dt.options6, rownames=FALSE, server=TRUE, escape = FALSE, selection = 'none');
    }
  });
  
  observeEvent(input$demeta.run, {
    withProgress( {
      cll <- session.data$loaded; 
      cmp <- session.data$de$comparison;
      res <- session.data$de$result; 
      md1 <- input$demeta.method.p;
      md2 <- input$demeta.method.m;
      rid <- input$demeta.comparisons_rows_selected; 
      
      if (!input$demeta.select.all) cmp <- cmp[rid, , drop=FALSE]; 
      
      session.data$de$meta <- tbl <- NULL;
      if (identical(NULL, cll)) output$demeta.run.msg <- renderUI({ h6(HTML(msg.nocollection)); }) else 
        if (identical(NULL, cmp)) output$demeta.run.msg <- renderUI({ h6(HTML("No comparisons available")); }) else
          if (identical(NULL, res)) output$demeta.run.msg <- renderUI({ h6(HTML("No DE results available")); }) else
            if (md1=='') output$demeta.run.msg <- renderUI({ h6(HTML("No method selected to combine p values")); }) else
              if (md2=='') output$demeta.run.msg <- renderUI({ h6(HTML("No method selected to combine mean differences")); }) else
                if (nrow(cmp) < 2) output$demeta.run.msg <- renderUI({ h6(HTML("Require at least 2 comparisons to run meta-analysis")); }) else {
                  session.data$de$meta <- geex.de.meta(cll, cmp, res, md1, md2); 
                  
                  tbl <- FormatNumeric(session.data$de$meta[[1]][, 1:(7+nrow(cmp)), drop=FALSE]); 
                  updateRadioButtons(session, 'demeta.result.option', selected = 1); 
                  updateSelectizeInput(session, 'demeta.result.n', NULL, choices=1:max(tbl$N_Comp), selected=1);
                  updateCheckboxInput(session, 'demeta1', value=TRUE);
                  output$demeta.run.msg <- renderUI({ h6(HTML("Analysis submitted")); }) 
                  if (identical(NULL, tbl)) output$demeta.result.msg <- renderUI({ "No meta-analysis result" })
                }
                
                output$demeta.result.table <- DT::renderDataTable({ FormatNumeric(tbl);
                }, options = dt.options6, rownames=FALSE, server=TRUE, escape = FALSE, selection = 'none');
    }, message = "Running meta-analysis ...", detail = "Please wait.");
  }); 
  
  observeEvent(input$demeta.result.option, {
    sel <- input$demeta.result.option; 
    res <- session.data$de$meta;
    tbl <- NULL;
    msg <- "";
    if (!identical(NULL, res)) {
      if (identical(NULL, sel)) msg <- "No table selected" else {
        tbl <- res[[as.integer(sel)]];
        tbl <- tbl[, 1:rev(grep('_Comparison_', colnames(tbl)))[1], drop=FALSE]; 
      }
      
      output$demeta.result.msg <- renderUI({ msg; });
      output$demeta.result.table <- DT::renderDataTable({ FormatNumeric(tbl);
      }, options = dt.options6, rownames=FALSE, server=TRUE, escape = FALSE, selection = 'none'); 
    }
  }); 
  
  output$demeta.download.table <- downloadHandler(
    filename = function() {
      sel <- input$demeta.result.option; 
      if (sel=='1') fn <- 'p_value' else if (sel=='2') fn <- 'mean_diff';
      paste('metaanalysis_', fn, '.txt', sep=''); 
    },  
    content  = function(file) { 
      tbl <- session.data$de$meta[[as.integer(input$demeta.result.option)]]; 
      for (j in 1:ncol(tbl)) tbl[[j]] <- CleanHtmlTags(tbl[[j]], remove.empty = FALSE); 
      write.table(tbl, file, sep='\t', row.names = FALSE, quote = FALSE)
    } 
  );
  
  output$demeta.download.column <- downloadHandler(
    filename = function() { 'column_description.txt' },
    content  = function(file) {
      sel <- input$demeta.result.option;
      if (sel=='1') tbl <- readRDS('data/meta_p_column.rds') else 
        if (sel=='2') tbl <- readRDS('data/meta_m_column.rds') else 
          tbl <- NULL; 
        write.table(tbl, file, sep='\t', row.names = FALSE, quote = FALSE);
    }
  );
  
  session.data;
}

  