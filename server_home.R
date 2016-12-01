server_home <- function(input, output, session, session.data) {
  load.coll0<-reactive({
    rid1<-input$home.table_rows_selected;
    rid2<-input$home.table_row_last_clicked;
    
    if(length(rid1)==0 | length(rid2)==0) NA else if (!(rid2 %in% rid1)) NA else {
      withProgress(geex.load.collection(rid2, GEX_HOME), 
                   message=paste("Loading data collection", rid2, '...'), 
                   detail="\nPlease wait until data is loaded to use GeEx.");
    } 
  });
  
  load.coll<-reactive({ 
    coll.loaded<-load.coll0();
    if (identical(coll.loaded, NA)) NA else {
      if(input$regroup.check) {
        withProgress(
          coll.loaded<-geex.regroup(coll.loaded, input$regroup.dataset_rows_selected, input$regroup.group_rows_selected), 
          message="Re-grouping samples in collection ...", detail="\nPlease wait until the re-grouping is done.");
        updateCheckboxInput(session, 'regroup.check', 'Uncheck box to restore default grouping');
      } else {
        coll.loaded$extra$regrouped<-FALSE;
        updateCheckboxInput(session, 'regroup.check', 'Check box to activate sample re-grouping');          
      }
      
      updateSelectInput(session, 'meta.options', choices=names(coll.loaded$browse_table));
      updateSelectInput(session, 'expr.dataset', choices=coll.loaded$extra$longname$dataset);
      updateSelectInput(session, 'pca.dataset', choices=coll.loaded$extra$longname$dataset);
      updateSelectInput(session, 'bar.dataset', choices=coll.loaded$extra$longname$dataset);
      updateSelectInput(session, 'geneset.dataset', choices=coll.loaded$extra$longname$dataset);
      updateSelectInput(session, 'filter.dataset', choices=coll.loaded$extra$longname$dataset);
      updateSelectInput(session, 'x.dataset', choices=coll.loaded$extra$longname$dataset);
      updateSelectInput(session, 'y.dataset', choices=coll.loaded$extra$longname$dataset);      
      updateSelectInput(session, 'comb.species', choices=unique(coll.loaded$gene$Species));
      
      output$bar.table <- DT::renderDataTable({ 
        geex.load.dataset(coll.loaded, coll.loaded$extra$longname$dataset[1])$anno; 
      }, options = dt.options3, rownames=FALSE, server=TRUE, escape = FALSE);            
      output$coex.gene1 <- DT::renderDataTable({ 
        coll.loaded$browse_table$Gene[, 1:5]
      }, options = dt.options2, selection='single', rownames=FALSE, server=TRUE, escape = FALSE);   
      output$coex.gene2 <- DT::renderDataTable({ 
        coll.loaded$browse_table$Gene[, 1:5]
      }, options = dt.options2, selection='single', rownames=FALSE, server=TRUE, escape = FALSE);   
      
      coll.loaded;
    }
  });
  
  output$home.title<-renderUI({ 
    list(h1('Welcome to Awsomics - GeEx'), 
         h4(HTML("<u>G</u>ene <u>e</u>xpression <u>Ex</u>plorer, beta version"))); });
  
  # Search table message
  output$home.message<-renderUI({
    if (identical(NA, load.coll())) list(h3("Click on a row to load the data collection")) else 
      list(h3(HTML(paste("Loaded data collection", geex.html.msg(load.coll()$selection)))))
  });
  
  output$home.table <- DT::renderDataTable({
    data.frame(ID=rownames(coll), coll, stringsAsFactors=FALSE)
  }, options=list(dom = 't'), selection='single', rownames=FALSE, escape=FALSE);
  
  session.data;
}