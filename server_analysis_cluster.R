server_analysis_cluster <- function(input, output, session, session.data) {
  output$cl.run.msg <- renderUI({ h6(HTML(geex.html.msg("Click to run analysis"))); });
  
  observeEvent(input$cl.run, {
    withProgress({
      cll <- session.data$loaded;
      
      if (!input$cluster1) {
        output$cl.run.msg <- renderUI({ h6(HTML("Click to run analysis")); });
        updateActionButton(session, 'cl.run', 'Run analysis');
        updateCheckboxInput(session, 'cluster1', value=TRUE);
        updateSelectInput(session, 'cl.select.table', choices = as.character(), selected = NULL); 
        isolate(session.data$coex$cluster <- NULL);
      } else {
        dat <- input$cl.dataset;
        prm <- c(input$cl.top.mean, input$cl.top.sd0, input$cl.top.sd1, input$cl.option.size,
                 input$cl.option.corr, input$cl.re.time, input$cl.re.corr);
        
        cl <- NULL;
        if (identical(NULL, cll)) {
          output$cl.run.msg <- renderUI({ h6(HTML(geex.html.msg(msg.nocollection))) });
        } else if (dat=='') {
          output$cl.run.msg <- renderUI({ h6(HTML(geex.html.msg(msg.nodataset))) });
        } else {
          tryCatch({
            setProgress(value=10);
            cl <- geex.gene.cluster(cll, dat, prm);
            output$cl.run.msg <- renderUI({ h6(HTML(geex.html.msg("Analysis done; click to start over"))) });
            setProgress(value=90);
          }, error = function(e) {
            output$cl.run.msg <- renderUI({ h6(HTML(geex.html.msg("No clusters found, ajdust parameter cutoffs to re-run."))) });
          })
        };
        updateRadioButtons(session, 'cl.option.table', selected=1);
        updateSelectInput(session, 'cl.select.table', choices = names(cl$cluster));
        updateActionButton(session, 'cl.run', 'Start over');
        
        if (!identical(NULL, cl)) {
          tbl <- data.frame(cl$summary);
          colnames(tbl) <- names(cl$group);
          tbl <- FormatNumeric(cbind('Cluster'=rownames(tbl), tbl, cl$mean));
          output$cl.result.table <- DT::renderDataTable({ tbl; 
          }, options = dt.options6, rownames=FALSE, server=TRUE, escape=FALSE, selection='none');
          updateCheckboxInput(session, 'cluster1', value=FALSE);
        };
        
        isolate(session.data$coex$cluster <- cl);
      }
    }, message = "Running clustering analysis ......", detail = "please wait.");
  });

  observeEvent(input$cl.option.table, {
    op <- input$cl.option.table;
    cl <- session.data$coex$cluster;
    if (identical(NULL, cl)) tbl <- NULL else if (op=='1') {
      tbl <- data.frame(cl$summary);
      colnames(tbl) <- names(cl$group);
      tbl <- FormatNumeric(cbind('Cluster'=rownames(tbl), tbl, cl$mean));
    } else {
      sel <- input$cl.select.table;
      if (sel=='') tbl <- NULL else {
        tbl <- data.frame(sapply(cl$group, function(g) rowMeans(cl$data[cl$cluster[[sel]], g, drop=FALSE])));
        colnames(tbl) <- names(cl$group);
        tbl <- FormatNumeric(cbind(tbl, cl$data[rownames(tbl), -1]));
        cll <- session.data$loaded;
        if (!identical(NULL, cll)) tbl <- cbind(cll$browse_table$Gene[rownames(tbl), c(1, 3)], tbl);
      }
    };
    output$cl.result.table <- DT::renderDataTable({ tbl; }, options = dt.options6, rownames=FALSE, server=TRUE, escape = FALSE);
  });

  observeEvent(input$cl.select.table, {
    sel <- input$cl.select.table;
    cl  <- session.data$coex$cluster;
    if ((sel=='' | is.na(sel)) & !identical(NULL, cl))
      updateSelectInput(session, 'cl.select.table', choices = names(cl$cluster));
  });

  observeEvent(input$cl.select.table, {
    sel <- input$cl.select.table;
    cll <- session.data$loaded;
    cl  <- session.data$coex$cluster;
    op  <- input$cl.option.table;

    if (!identical(NULL, cll) & !identical(NULL, cl) & sel!='' & op!='1') {
      tbl <- data.frame(sapply(cl$group, function(g) rowMeans(cl$data[cl$cluster[[sel]], g, drop=FALSE])));
      colnames(tbl) <- names(cl$group);
      tbl <- FormatNumeric(cbind(tbl, cl$data[rownames(tbl), -1]));
      cll <- session.data$loaded;
      if (!identical(NULL, cll)) tbl <- cbind(cll$browse_table$Gene[rownames(tbl), c(1, 3)], tbl);
      output$cl.result.table <- DT::renderDataTable({ tbl; }, options = dt.options6, rownames=FALSE, server=TRUE, escape = FALSE);
    }
  });

  output$cl.download.summary <- downloadHandler(
    filename = function() { "clustering_summary.txt"  },
    content  = function(file) {
      cl <- session.data$coex$cluster;
      if (identical(NULL, cl)) tbl <- NULL else {
        tbl <- data.frame(cl$summary);
        colnames(tbl) <- names(cl$group);
        tbl <- FormatNumeric(cbind('Cluster'=rownames(tbl), tbl, cl$mean));
      }
      write.table(tbl, file, sep='\t', row.names = FALSE, quote = FALSE)
    }
  );

  output$cl.download.gene <- downloadHandler(
    filename = function() { "clustering_gene.txt"  },
    content  = function(file) {
      cll <- session.data$loaded;
      cl  <- session.data$coex$cluster;

      if (identical(NULL, cll) | identical(NULL, cl)) tbl <- NULL else {
        tbl <- data.frame(cl$data);
        mns <- data.frame(sapply(cl$group, function(g) rowMeans(tbl[, g, drop=FALSE])));
        colnames(mns) <- names(cl$group);
        ann <- cll$browse_table$Gene[rownames(tbl), , drop=FALSE];
        ann <- ann[, c(1, 3, 4, 5, 2, 6:ncol(ann)), drop=FALSE];
        for (i in 1:ncol(ann)) ann[[i]] <- CleanHtmlTags(ann[[i]], remove.empty = FALSE);
        tbl <- cbind('Cluster'=tbl[, 1], ann[, 1:2], mns, tbl[, -1], ann[, 3:ncol(ann)]);
      }
      write.table(tbl, file, sep='\t', row.names = FALSE, quote = FALSE)
    }
  );

  output$cl.plot <- renderPlotly({
    cl <- session.data$coex$cluster;
    op <- input$cl.option.table;

    if (identical(NULL, cl)) {
      plotly_empty();
    } else if (op=='1') {
      smm <- cl$summary;
      p <- PlotlyMatrixGroupHeatmap(t(smm), as.list(rownames(smm)), color='rainbow', key='Average expression (normalized)', clustering = TRUE);
      layout(p, title='Cluster-group average');
    } else {
      s <- input$cl.select.table;
      d <- cl$mean[s, unlist(cl$group, use.names = FALSE)];
      g <- rep(names(cl$group), sapply(cl$group, length));
      b <- min(150, 7.5*max(nchar(names(cl$group))));
      plot_ly(y=d, color=g, type='box', showlegend=FALSE, boxpoints = "all", jitter = 0.2, pointpos = 0, marker=list(symbol=18)) %>%
        layout(xaxis=list(tickangle = -45), yaxis=list(title='Average gene expression (normalized)'),
               margin=list(b=b, t=40), title=s);
    }
  });
  
  session.data;
}