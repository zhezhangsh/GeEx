geex.summarize.dataset <- function(session.data, input) {  
  cll <- session.data$loaded; 
  if (!identical(NULL, cll)) {      
    ds.selected <- input$expr.dataset;
    if (ds.selected == '') NULL else {
      ds  <- geex.load.dataset(cll, ds.selected);
      grp <- input$expr.group;
      grp <- grp[grp %in% names(ds$group)];
      spe <- sub(' homolog$', '', input$expr.species);
      spe <- spe[spe %in% ds$anno$Species];
      
      if (length(grp) == 0) grp <- names(ds$group); 
      
      if (length(spe) == 0) NULL else {
        withProgress(
          geex.dataset.table(cll, ds.selected, grp, spe, input$expr.scale, expr_columns, GetRowStatTypes(), expr_anno_columns, c()), 
          message="Calculating column values ...", 
          detail="\nPlease wait for the results."
        );            
      }          
    }
  } else NULL; 
};
