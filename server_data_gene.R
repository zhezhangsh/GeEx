server_data_gene <- function(input, output, session, session.data) {
  observeEvent(input$gene.gs.source, { 
    updateSelectizeInput(session, 'gene.gs.coll', choices=names(geneset[[input$gene.gs.source]]));
    output$gene.message <- renderUI({''});
    output$gene.stat.table <- DT::renderDataTable({NULL});
  });  
  observeEvent(input$gene.gs.coll, { 
    cll <- session.data$loaded;
    sp <- names(geneset[[input$gene.gs.source]][[input$gene.gs.coll]]);
    if (!identical(NULL, cll)) {
      sp<-union(sp[sp %in% as.vector(cll$gene$Species)], 'human');
      updateSelectizeInput(session, 'gene.gs.species', choices=sp, selected='human'); 
    };
    output$gene.message <- renderUI({''});
    output$gene.stat.table <- DT::renderDataTable({NULL});
  });
  observeEvent(input$gene.gs.species, { 
    output$gene.gs.table <- DT::renderDataTable({ 
      geex.geneset.table(geneset, input$gene.gs.source, input$gene.gs.coll, input$gene.gs.species);  
    }, options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE);
    output$gene.message <- renderUI({''});
    output$gene.stat.table <- DT::renderDataTable({NULL});
  });
  observeEvent(input$gene.clear, { 
    output$gene.gs.table <- DT::renderDataTable({ 
      geex.geneset.table(geneset, input$gene.gs.source, input$gene.gs.coll, input$gene.gs.species);  
    }, options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE);
    output$gene.message <- renderUI({''});
    output$gene.stat.table <- DT::renderDataTable({NULL});
  });
  observeEvent(input$gene.table_rows_selected, {
    output$gene.message <- renderUI({''});
    output$gene.stat.table <- DT::renderDataTable({NULL});
  });
  
  output$gene.table <- DT::renderDataTable({
    cll <- session.data$loaded;
    if (!identical(cll, NULL)) { 
      anno <- cll$gene;
      if (input$gene.filter) {
        rid <- CleanHtmlTags(input$gene.gs.table_rows_selected);
        if (length(rid)>0) { 
          rid <- rownames(geneset[[input$gene.gs.source]][[input$gene.gs.coll]][[input$gene.gs.species]])[as.integer(rid)]; 
          gs<-readRDS(paste(GENESET_HOME, '/', tolower(input$gene.gs.source), '_list.rds', sep=''))[rid];  
          gn<-unique(unlist(gs, use.names=FALSE));
          anno<-anno[rownames(anno) %in% gn, , drop=FALSE];
        }
      }
      if (nrow(anno) == 0) { 
        if (length(rid)>0) tbl <- geex.empty.matrix("No more genes after filtering") else 
          tbl <- geex.empty.matrix("No gene information in data set")
      } else {
        tbl <- data.frame(
          ID=AddHref(rownames(anno), UrlEntrezGene(rownames(anno))), 
          Name=as.vector(anno$Symbol), Species=as.vector(anno$Species),
          Type=sub('^protein-coding', 'coding', as.vector(anno$type_of_gene)), 
          N_Set=anno$Num_Dataset, Synonyms=as.vector(anno$Synonyms), 
          Description=as.vector(anno$description), stringsAsFactors=FALSE
        );
      }
    } else tbl <- geex.empty.matrix("Empty table");
    tbl;
  }, options = dt.options3, filter='bottom', selection='single', rownames=FALSE, escape = FALSE);
  
  observeEvent(input$gene.table.button, {
    cll <- isolate(session.data$loaded);
    
    output$gene.message<-renderUI({ 
      if (identical(NULL, cll)) list(h4(msg.nocollection)) else {
        rid0 <- isolate(input$gene.table_row_last_clicked); 
        rid1 <- isolate(input$gene.table_rows_selected);
        if (length(rid1)>0 & length(rid0)>0)  
          if (rid0 %in% rid1) geex.html.gene.name(rid0, TRUE, cll) else list(h4(msg.nogene)) else
            list(h4(msg.nogene))
      }
    });
    
    output$gene.stat.table <- DT::renderDataTable({ 
      rid0 <- isolate(input$gene.table_row_last_clicked); 
      rid1 <- isolate(input$gene.table_rows_selected); 
      if (length(rid1)>0 & length(rid0)>0) { 
        if (rid0[1] %in% rid1) {
          withProgress(
            {tbl <- geex.summarize.gene(cll, rid0)}, 
            message="Summarizing gene statistics ...", 
            detail="\nPlease wait for the results."
          );  
        } else NULL
      } else tbl <- NULL; 
      isolate(session.data$data$gene <- tbl);
      tbl;
    }, options = dt.options1, filter='bottom', selection='single', rownames=FALSE, escape = FALSE);  
  });

  output$gene.download <- downloadHandler(
    filename = function() { 'gene.txt' }, 
    content = function(file) { 
      tbl <- session.data$data$gene;
      if (!is.null(tbl)) {
        for (i in 1:ncol(tbl)) tbl[[i]] <- CleanHtmlTags(tbl[[i]], remove.empty = FALSE); 
        write.table(tbl, file, row.names = FALSE, col.names = TRUE, quote = FALSE, sep='\t'); 
      }
    }
  );
  
  session.data;
}