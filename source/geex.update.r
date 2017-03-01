geex.update <- function(input, output, session, session.data) {
  output$meta.message <- renderUI({''});
  output$meta.detail <- renderUI({''});
  output$meta.table <- DT::renderDataTable({NULL});
  output$gene.message <- renderUI({''});
  output$gene.stat.table <- DT::renderDataTable({NULL});
  output$expr.message <- renderUI({''});
  output$expr.table <- DT::renderDataTable({NULL});
  output$comb.message <- renderUI({''});
  output$comb.table <- DT::renderDataTable({NULL});
  
  output$pca.plot <- renderPlotly({plotly_empty();}); 
  output$bar.message <- renderUI({''});
  output$bar.plot <- renderPlotly({plotly_empty();});
  output$bar.stat <- DT::renderDataTable({NULL});
  output$geneset.message <- renderUI({''});
  output$geneset.plot <- renderPlotly({plotly_empty();});
  output$geneset.stat <- DT::renderDataTable({NULL});
  output$two.message <- renderUI({''});
  output$two.plot <- renderPlotly({plotly_empty();});
  output$two.geneset.stat <- DT::renderDataTable({NULL});
  
  loaded <- session.data$loaded; 
  
  updateSelectizeInput(session, 'meta.options', choices=names(loaded$browse_table));
  updateSelectizeInput(session, 'expr.dataset', choices=loaded$extra$longname$dataset);
  updateSelectizeInput(session, 'geneset.dataset', choices=loaded$extra$longname$dataset);
  updateSelectizeInput(session, 'comb.species', choices=unique(loaded$gene$Species));
  updateSelectizeInput(session, 'pca.dataset', choices=loaded$extra$longname$dataset);
  updateSelectizeInput(session, 'bar.dataset', choices=loaded$extra$longname$dataset);
  updateSelectizeInput(session, 'x.dataset', choices=loaded$extra$longname$dataset);
  updateSelectizeInput(session, 'y.dataset', choices=loaded$extra$longname$dataset);
  updateSelectizeInput(session, 'de.dataset1', choices=loaded$extra$longname$dataset);
  updateSelectizeInput(session, 'de.dataset2', choices=loaded$extra$longname$dataset);
  updateSelectizeInput(session, 'cl.dataset', choices=loaded$extra$longname$dataset);
  updateCheckboxGroupInput(session, 'comb.dataset', choices=rownames(loaded$metadata$Dataset), selected=rownames(loaded$metadata$Dataset), inline=TRUE);
  updateSelectizeInput(session, 'de.remove.index', choices = ''); 
  updateSelectizeInput(session, 'de.result.index', choices = ''); 
  updateActionButton(session, 'de.run', 'Run analysis'); 
  updateCheckboxInput(session, 'de1', value=TRUE);
  updateCheckboxInput(session, 'demeta1', value=FALSE);
  
  # Co-expression analysis
  # updateCheckboxInput(session, 'coex.gene.all', NULL, TRUE);
  # updateTextInput(session, 'coex.gene.text', NULL, 'Enter keyword ...');
  output$coex.gene.table <- DT::renderDataTable({ 
    session.data$loaded$browse_table$Gene[, 1:5, drop=FALSE];
  }, options = dt.options8, rownames=FALSE, server=TRUE, escape = FALSE, selection = 'single');
  updateRadioButtons(session, 'coex.dataset.all', selected=1); 
  updateCheckboxInput(session, 'coex1', value=TRUE); 
  updateCheckboxInput(session, 'coex2', value=FALSE); 
  updateCheckboxInput(session, 'coex3', value=TRUE); 
  updateActionButton(session, 'coex.run', 'Run analysis');
  output$coex.run.msg <- renderUI({ h6(HTML("Click to run analysis")); });
  output$coex.result <- DT::renderDataTable(NULL, options = dt.options7);
  output$coex.dataset.table <- DT::renderDataTable(NULL, options = dt.options7);
  
  # Clustering analysis
  output$cl.run.msg <- renderUI({ h6(HTML("Click to run analysis")); });
  updateActionButton(session, 'cl.run', 'Run analysis');
  updateCheckboxInput(session, 'cluster1', value=TRUE);
  updateSelectInput(session, 'cl.select.table', choices = as.character(), selected = NULL); 
  
  # DE analysis
  output$de.comparisons <- DT::renderDataTable({ NULL;
  }, options = dt.options3, rownames=FALSE, server=TRUE, escape = FALSE);
  output$de.run.msg <- renderUI({ h6(HTML("Click to run DE analysis")); });
  output$de.add.msg <- renderUI({ h6(HTML("Click to add a new comparison")); });
  output$de.remove.msg <- renderUI({ h6(HTML("Click to remove comparison")); }); 
  output$demeta.result.table <- DT::renderDataTable({ NULL });
  output$demeta.result.msg <- renderUI({ ""; });

}