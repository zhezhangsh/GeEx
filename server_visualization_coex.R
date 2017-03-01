server_visualization_coex <- function(input, output, session, session.data) {
  # ########################################################################################
  # ########################################## "Coexpression" tab ##########################      
  # {
  #   coex.results<-reactive({ 
  #     cll<-load.coll();
  #     
  #     rid1s<-input$coex.gene1_rows_selected; 
  #     rid1<-input$coex.gene1_row_last_clicked;
  #     if (length(rid1)>0 & length(rid1s)>0) if (rid1 %in% rid1s) id1<-CleanHtmlTags(rid1) else id1<-c() else id1<-c();
  #     
  #     rid2s<-input$coex.gene2_rows_selected; 
  #     rid2<-input$coex.gene2_row_last_clicked;
  #     if (length(rid2)>0 & length(rid2s)>0) if(rid2 %in% rid2s) id2<-CleanHtmlTags(rid2) else id2<-c() else id2<-c()
  #     
  #     coex<-geex.coex.data(cll, id1, id2, input$coex.scale, input$coex.color);
  #     
  #     coex;
  #   });
  #   
  #   observeEvent(input$coex.lookup.option, {
  #     cll<-load.coll();
  #     if (input$coex.lookup.option & !identical(NA, cll)) {
  #       output$coex.lookup.table<-DT::renderDataTable({ geex.empty.matrix('No search key'); }, options=list(dom='t'))
  #       output$coex.lookup.key.ui<-renderUI({ textInput("coex.lookup.key", "", '') });
  #       output$coex.lookup.species.ui<-renderUI({ 
  #         selectizeInput("coex.lookup.species", '', choices=unique(cll$gene$Species), selected='human') });
  #       output$coex.lookup.table.ui<-renderUI({ DT::dataTableOutput('coex.lookup.table') });
  #     } else { 
  #       updateTextInput(session, "coex.lookup.key", NULL, NULL);
  #       updateSelectizeInput(session, "coex.lookup.species", NULL, NULL, NULL);
  #       output$coex.lookup.key.ui <- renderUI({br()});
  #       output$coex.lookup.species.ui <- renderUI({br()}); 
  #       output$coex.lookup.table.ui <- renderUI({br()}); 
  #     }
  #   }); 
  #   observeEvent(input$coex.lookup.key, {
  #     ky<-input$coex.lookup.key;
  #     output$coex.lookup.table <- DT::renderDataTable({
  #       if (ky!='' & !is.null(ky)) geex.lookup.gene(geex.load.dataset(load.coll(), input$bar.dataset)$anno, input$coex.lookup.species, ky, GENE_HOME) else 
  #         geex.empty.matrix('No search key');
  #     }, options = dt.options4, selection='none', rownames=FALSE, server=TRUE, escape = FALSE)   
  #   });
  #   
  #   observeEvent(input$coex.clear.ds, {
  #     output$coex.dataset <- DT::renderDataTable({  
  #       coex<-coex.results(); 
  #       if (!coex$plot) geex.empty.matrix(coex$status) else coex$stat;
  #     }, options = dt.options3, rownames=FALSE, server=TRUE, escape = FALSE);   
  #   });
  #   
  #   output$coex.title<-renderUI({ 
  #     list(h2("Gene-gene coexpression"), 
  #          HTML(c('Coexpression of two selected genes within and across data sets.', 
  #                 'The APP <strong>CoEx</strong> will provide more functions for coexpression analysis.'))) 
  #   });
  #   
  #   output$coex.message<-renderUI({ 
  #     cll<-load.coll();
  #     
  #     rid1s<-input$coex.gene1_rows_selected; 
  #     rid1<-input$coex.gene1_row_last_clicked;
  #     if (length(rid1)>0 & length(rid1s)>0) if (rid1 %in% rid1s) id1<-CleanHtmlTags(rid1) else id1<-c() else id1<-c();
  #     
  #     rid2s<-input$coex.gene2_rows_selected; 
  #     rid2<-input$coex.gene2_row_last_clicked;
  #     if (length(rid2)>0 & length(rid2s)>0) if(rid2 %in% rid2s) id2<-CleanHtmlTags(rid2) else id2<-c() else id2<-c()
  #     
  #     if (length(id1)>0) nm1<-as.vector(cll$gene[id1, 'Symbol']) else nm1<-'';
  #     if (length(id2)>0) nm2<-as.vector(cll$gene[id2, 'Symbol']) else nm2<-'';
  #     
  #     if (nm1=='' & nm2=='') h3(HTML(geex.html.msg(msg.nogene))) else 
  #       h3(HTML(paste(geex.html.msg(nm1), 'vs.', geex.html.msg(nm2))));      
  #   });
  #   
  #   output$coex.dataset <- DT::renderDataTable({  
  #     coex<-coex.results(); 
  #     if (!coex$plot) geex.empty.matrix(coex$status) else coex$stat;
  #   }, options = dt.options3, rownames=FALSE, server=TRUE, escape = FALSE);   
  #   
  #   output$coex.plot <- renderPlot({  
  #     coex<-coex.results();
  #     geex.coex.plot(coex, type=which(data.level.options==input$coex.type), input$coex.dataset_rows_selected);
  #   })
  # }
  # 
  session.data;
}