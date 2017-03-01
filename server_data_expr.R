server_data_expr <- function(input, output, session, session.data) {
  observeEvent(input$expr.dataset, {
    ds <- geex.load.dataset(session.data$loaded, input$expr.dataset);
    if (!identical(NA, ds)) {
      updateCheckboxGroupInput(session, 'expr.group', choices=names(ds$group), selected=names(ds$group), inline=TRUE);

      sp<-tolower(unique(as.vector(ds$anno$Species)));
      sp<-sp[sp!='human'];
      if (length(sp)>0) sp<-c(sp, 'human homolog') else sp<-'human'; 
      updateSelectizeInput(session, 'expr.species', choices=sp);
      sp0<-names(geneset[[input$expr.gs.source]][[input$expr.gs.coll]]);
      if (length(sp) == 1) sp0<-'human' else sp0<-unique(c(sp0[sp0 %in% sp], 'human'));
      updateSelectizeInput(session, 'expr.gs.species', choices=sp0, selected=sp0[1]);
    };
    output$expr.message <- renderUI({''});
    output$expr.table <- DT::renderDataTable({NULL});
  });
  observeEvent(input$expr.group, {
    output$expr.message <- renderUI({''}); 
    output$expr.table <- DT::renderDataTable({NULL}); 
  }); 
  observeEvent(input$expr.species, {
    output$expr.message <- renderUI({''}); 
    output$expr.table <- DT::renderDataTable({NULL}); 
  }); 
  observeEvent(input$expr.scale, {
    output$expr.message <- renderUI({''}); 
    output$expr.table <- DT::renderDataTable({NULL}); 
  }); 
  observeEvent(input$expr.column, {
    output$expr.message <- renderUI({''}); 
    output$expr.table <- DT::renderDataTable({NULL}); 
  }); 
  observeEvent(input$expr.desc, {
    output$expr.message <- renderUI({''}); 
    output$expr.table <- DT::renderDataTable({NULL}); 
  }); 
  observeEvent(input$expr.anno, {
    output$expr.message <- renderUI({''}); 
    output$expr.table <- DT::renderDataTable({NULL}); 
  }); 
  observeEvent(input$expr.select.all, { 
    if (!input$expr.select.all) {
      updateCheckboxGroupInput(session, 'expr.column', NULL, choice=expr_columns, selected=expr_columns[1], inline=TRUE);
      updateCheckboxGroupInput(session, 'expr.desc', choice=GetRowStatTypes(), selected=GetRowStatTypes(), inline=TRUE);
      updateCheckboxGroupInput(
        session, 'expr.anno', choice=expr_anno_columns[-1], selected=expr_anno_columns[length(expr_anno_columns)], inline=TRUE)
    } else {
      updateCheckboxGroupInput(session, 'expr.column', NULL, choice=expr_columns, selected=expr_columns, inline=TRUE);
      updateCheckboxGroupInput(session, 'expr.desc', NULL, choice=GetRowStatTypes(), selected=GetRowStatTypes(), inline=TRUE);
      updateCheckboxGroupInput(session, 'expr.anno', NULL, choice=expr_anno_columns[-1], selected=expr_anno_columns, inline=TRUE)
    };
    output$expr.message <- renderUI({''}); 
    output$expr.table <- DT::renderDataTable({NULL}); 
  });
  
  observeEvent(input$expr.gs.table_rows_selected, { 
    output$expr.message <- renderUI({''}); 
    output$expr.table <- DT::renderDataTable({NULL}); 
  });
  
  observeEvent(input$expr.gs.source, { 
    updateSelectizeInput(session, 'expr.gs.coll', choices=names(geneset[[input$expr.gs.source]]));
    output$expr.message <- renderUI({''}); 
    output$expr.table <- DT::renderDataTable({NULL}); 
  });
  
  observeEvent(input$expr.gs.coll, {
    coll <- session.data$loaded;
    sp <- names(geneset[[input$expr.gs.source]][[input$expr.gs.coll]]);
    
    if (!identical(NULL, coll) & input$expr.dataset!='') { 
      ds <- geex.load.dataset(coll, input$expr.dataset); 
      ds.sp <- unique(as.vector(ds$anno$Species));
      sp <- sp[sp %in% ds.sp];
      sp <- c(sp[tolower(sp)!='human'], 'human');
    };
    updateSelectizeInput(session, 'expr.gs.species', choices=sp);
    output$expr.message <- renderUI({''}); 
    output$expr.table <- DT::renderDataTable({NULL}); 
  });
  
  observeEvent(input$expr.gs.species, {
    output$expr.gs.table <- DT::renderDataTable({
      geex.geneset.table(geneset, input$expr.gs.source, input$expr.gs.coll, input$expr.gs.species);
    }, options = dt.options3, rownames=FALSE, server=TRUE, escape = FALSE); 
    output$expr.message <- renderUI({''}); 
    output$expr.table <- DT::renderDataTable({NULL}); 
  });
  
  observeEvent(input$expr.gs.clear, {
    output$expr.gs.table <- DT::renderDataTable({
      geex.geneset.table(geneset, input$expr.gs.source, input$expr.gs.coll, input$expr.gs.species);
    }, options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE)
    output$expr.message <- renderUI({''}); 
    output$expr.table <- DT::renderDataTable({NULL}); 
  });
  
  observeEvent(input$expr.table.button, {
    output$expr.message<-renderUI({
      list(h4(HTML(paste('Table -', geex.html.msg(isolate(input$expr.dataset))))));
    });

    output$expr.table <- DT::renderDataTable({
      cll <- isolate(session.data$loaded);
      tbl <- isolate(geex.summarize.dataset(session.data, input)); 
      if (!identical(NULL, cll) & !identical(NULL, tbl)) {
        if (colnames(tbl)[1] != 'ID') tbl0 <- tbl else {
          cmn<-c('Name', isolate(input$expr.anno), isolate(input$expr.desc));
          cs<-colnames(tbl);
          cs<-cs[!(cs %in% c('ID', expr_anno_columns, GetRowStatTypes()))];
          cnm.grp<-cs[grep('^G[0-9]', cs)];
          cnm.smp<-cs[grep('^S[0-9]', cs)];
          col<-isolate(input$expr.column);
          if (expr_columns[1] %in% col) cmn<-c(cmn, cnm.grp);
          if (expr_columns[2] %in% col) cmn<-c(cmn, cnm.smp);
          tbl <- tbl[, c('ID', cmn), drop=FALSE];
          rid <- isolate(CleanHtmlTags(input$expr.gs.table_rows_selected));  
          gs1 <- isolate(input$expr.gs.source); 
          gs2 <- isolate(input$expr.gs.coll); 
          gs3 <- isolate(input$expr.gs.species); 
          rid <- rownames(geneset[[gs1]][[gs2]][[gs3]])[as.integer(rid)]; 
          if (length(rid) == 0) tbl0 <- tbl else {
            gs <- readRDS(paste(GENESET_HOME, '/', tolower(isolate(gs1)), '_list.rds', sep=''))[rid];
            gn <- unique(unlist(gs, use.names=FALSE));
            gn <- gn[gn %in% rownames(tbl)]; 
            if (length(gn) == 0) tbl0 <- geex.empty.matrix("No gene of selected gene set found in data set") else 
              tbl0 <- tbl[gn, , drop=FALSE];
          }         
        }
      } else tbl0 <- NULL;
      isolate({session.data$data$expr <- tbl0}); 
      tbl0;
    }, options = dt.options1, filter='bottom', selection='none', rownames=FALSE, server=TRUE, escape = FALSE);
  }); 
  
  output$expr.message <- renderUI({ 
    if (identical(NULL, isolate(session.data$loaded))) list(h4(msg.nocollection)) else 
      if (isolate(input$expr.dataset=='')) list(h4(msg.nodataset)) else list(h4('')); 
  });
  
  output$expr.download <- downloadHandler(
    filename = function() { paste(gsub(' ', '_', input$expr.dataset), "dataset.txt", sep='_'); }, 
    content = function(file) {
      tbl <- session.data$data$expr;
      if (!is.null(tbl)) {
        for (i in 1:ncol(tbl)) tbl[[i]] <- CleanHtmlTags(tbl[[i]], remove.empty = FALSE); 
        write.table(tbl, file, row.names = FALSE, col.names = TRUE, quote = FALSE, sep='\t'); 
      }
    }
  );
  
  session.data;
}