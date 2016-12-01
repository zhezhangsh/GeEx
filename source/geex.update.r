geex.update <- function(input, output, session, session.data) {
  loaded <- session.data$loaded; 
  if (!is.null(loaded)) {  
    output$meta.message <- renderUI({h3(
      HTML(paste('Data collection: <font color="blue">', loaded$name, '</font>', sep=''))) });
    
    updateSelectizeInput(session, 'meta.options', choices=names(loaded$browse_table));
    updateSelectInput(session, 'expr.dataset', choices=loaded$extra$longname$dataset);
    updateSelectInput(session, 'geneset.dataset', choices=loaded$extra$longname$dataset);
    
    # updateSelectInput(session, 'pca.dataset', choices=coll.loaded$extra$longname$dataset);
    # updateSelectInput(session, 'bar.dataset', choices=coll.loaded$extra$longname$dataset);
    # updateSelectInput(session, 'geneset.dataset', choices=coll.loaded$extra$longname$dataset);
    # updateSelectInput(session, 'filter.dataset', choices=coll.loaded$extra$longname$dataset);
    # updateSelectInput(session, 'x.dataset', choices=coll.loaded$extra$longname$dataset);
    # updateSelectInput(session, 'y.dataset', choices=coll.loaded$extra$longname$dataset);      
    # updateSelectInput(session, 'comb.species', choices=unique(coll.loaded$gene$Species));
  }
}