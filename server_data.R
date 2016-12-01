server_data <- function(input, output, session, session.data) { 
  ##############################################################################################################
  ######################################## "Data" menu #########################################################
  ##############################################################################################################
  
  ########################################## "Metadata" tab ####################################################      
  output$meta.table <- DT::renderDataTable({
    cll <- session.data$loaded;
    if (!identical(cll, NULL)) {
      tbl.nm <- input$meta.options; 
      if (tbl.nm=='' | is.na(tbl.nm)) cll$browse_table[[1]] else cll$browse_table[[tbl.nm]];    
    } else geex.empty.matrix("Empty table")
  }, options = dt.options1, selection='single', rownames=FALSE, escape = FALSE);
  
  output$meta.detail <- renderUI({
    rid1 <- input$meta.table_rows_selected;
    rid2 <- input$meta.table_row_last_clicked;
    if (length(rid1)>0 & length(rid2)>0)
      if (rid2[1] %in% rid1) {
        cll <- session.data$loaded;
        if (!identical(cll, NULL)) {
          r <- rownames(cll$browse_table[[input$meta.options]])[rid2[1]];
          t <- sapply(cll$metadata_by_id, function(x) r %in% names(x));
          if (length(t[t])>0) HtmlKeyValue(cll$metadata_by_id[[which(t)[1]]][[rid2[1]]], separator2=' ');
        }
      }
  });
  
  ########################################## "Data set" tab ####################################################      
  observeEvent(input$expr.dataset, {
    ds <- geex.load.dataset(session.data$loaded, input$expr.dataset);
    if (!identical(NA, ds)) {
      updateCheckboxGroupInput(session, 'expr.group', choices=names(ds$group), selected=names(ds$group));
      sp<-tolower(unique(as.vector(ds$anno$Species)));
      sp<-sp[sp!='human'];
      if (length(sp)>0) sp<-c(sp, 'human homolog') else sp<-'human';
      updateCheckboxGroupInput(session, 'expr.species
                               ', choices=sp, selected=sp);
      sp0<-names(geneset[[input$expr.gs.source]][[input$expr.gs.coll]]);
      if (length(sp) == 1) sp0<-'human' else sp0<-unique(c(sp0[sp0 %in% sp], 'human'));
      updateSelectizeInput(session, 'expr.gs.species', choices=sp0, selected=sp0[1]);
      output$expr.message<-renderUI({
        list(h3(HTML(paste('Data set', geex.html.msg(ds$longname)))), br(), br()); });
    }
  });

  observeEvent(input$expr.select.all, { 
    if (!input$expr.select.all) {
      updateCheckboxGroupInput(session, 'expr.column', NULL, choice=expr_columns, selected=expr_columns[1], inline=FALSE);
      updateCheckboxGroupInput(session, 'expr.desc', choice=GetRowStatTypes(), selected=GetRowStatTypes(), inline=TRUE);
      updateCheckboxGroupInput(
        session, 'expr.anno', choice=expr_anno_columns, selected=expr_anno_columns[c(1, length(expr_anno_columns))], inline=TRUE)
    } else {
      updateCheckboxGroupInput(session, 'expr.column', NULL, choice=expr_columns, selected=expr_columns, inline=FALSE);
      updateCheckboxGroupInput(session, 'expr.desc', choice=GetRowStatTypes(), selected=GetRowStatTypes(), inline=TRUE);
      updateCheckboxGroupInput(session, 'expr.anno', choice=expr_anno_columns, selected=expr_anno_columns, inline=TRUE)
    }
  });
  
  observeEvent(input$expr.gs.source, { 
    updateSelectizeInput(session, 'expr.gs.coll', choices=names(geneset[[input$expr.gs.source]]))}
  );
  observeEvent(input$expr.gs.coll, {
    coll <- session.data$loaded;
    sp <- names(geneset[[input$expr.gs.source]][[input$expr.gs.coll]]);
    
    if (!identical(NULL, coll) & input$expr.dataset!='') { 
      ds <- geex.load.dataset(coll, input$expr.dataset); 
      ds.sp <- unique(as.vector(ds$anno$Species));
      sp <- sp[sp %in% ds.sp];
      sp <- c(sp[tolower(sp)!='human'], 'human');
    };
    updateSelectizeInput(session, 'expr.gs.species', choices=sp);}
  );
  observeEvent(input$expr.gs.species, {
    output$expr.gs.table <- DT::renderDataTable({
      geex.geneset.table(geneset, input$expr.gs.source, input$expr.gs.coll, input$expr.gs.species);
    }, options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE)}
  );
  observeEvent(input$expr.gs.clear, {
    output$expr.gs.table <- DT::renderDataTable({
      geex.geneset.table(geneset, input$expr.gs.source, input$expr.gs.coll, input$expr.gs.species);
    }, options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE)}
  );
  
  # {
  #   summarize.dataset<-reactive({  
  #     cll<-load.coll(); 
  #     if (identical(NA, cll)) geex.empty.matrix(msg.nocollection) else {      
  #       ds.selected<-input$expr.dataset;
  #       if (ds.selected == '') geex.empty.matrix(msg.nodataset) else {
  #         ds<-geex.load.dataset(cll, ds.selected);
  #         grp<-input$expr.group;
  #         grp<-grp[grp %in% names(ds$group)];
  #         spe<-sub(' homolog$', '', input$expr.species);
  #         spe<-spe[spe %in% ds$anno$Species];
  #         if (length(grp) == 0) geex.empty.matrix("No selected group; must select one") else 
  #           if (length(spe) == 0) geex.empty.matrix("No selected species; must select one") else
  #             withProgress(geex.dataset.table(cll, ds.selected, grp, spe, input$expr.scale, expr_columns, GetRowStatTypes(), expr_anno_columns, c()), 
  #                          message="Calculating descriptive stats ...", detail="\nPlease wait for the full result table.");
  #       }
  #     }
  #   });
  #   



  #   
  #   output$expr.message<-renderUI({ 
  #     if (identical(NA, load.coll())) list(h3(msg.nocollection), br(), br()) else 
  #       if (input$expr.dataset=='') list(h3(msg.nodataset), br(), br()) });
  #   output$expr.table <- DT::renderDataTable({
  #     cll<-load.coll();
  #     if (identical(NA, cll))  geex.empty.matrix(msg.nocollection) else {
  #       tbl<-summarize.dataset();
  #       if (colnames(tbl)[1] != 'ID') tbl else {
  #         cmn<-c(input$expr.anno, input$expr.desc);
  #         cs<-colnames(tbl);
  #         cs<-cs[!(cs %in% c('ID', expr_anno_columns, GetRowStatTypes()))];
  #         cnm.grp<-cs[grep('^G[0-9]', cs)];
  #         cnm.smp<-cs[grep('^S[0-9]', cs)];
  #         col<-input$expr.column;
  #         if (expr_columns[1] %in% col) cmn<-c(cmn, cnm.grp);
  #         if (expr_columns[2] %in% col) cmn<-c(cmn, cnm.smp);
  #         tbl<-tbl[, c('ID', cmn), drop=FALSE];
  #         rid<-CleanHtmlTags(input$expr.gs.table_rows_selected);
  #         rid<-rid[rid %in% rownames(geneset[[input$expr.gs.source]][[input$expr.gs.coll]][[input$expr.gs.species]])]; 
  #         if (length(rid) == 0) tbl else {
  #           gs<-readRDS(paste(GENESET_HOME, '/', tolower(input$expr.gs.source), '_list.rds', sep=''))[rid];
  #           gn<-unique(unlist(gs, use.names=FALSE));
  #           gn<-gn[gn %in% rownames(tbl)];
  #           if (length(gn) == 0) geex.empty.matrix("No gene of selected gene set found in data set") else 
  #             tbl[gn, , drop=FALSE];
  #         }         
  #       }}
  #   }, options = dt.options1, filter='bottom', selection='none', rownames=FALSE, server=TRUE, escape = FALSE);
  #   
  # }
  # 
  # ########################################## "Gene-Data set" tab ###############################################      
  # {
  #   combine.dataset<-reactive({   
  #     cll<-load.coll();
  #     if (identical(NA, cll)) geex.empty.matrix(msg.nocollection) else {
  #       withProgress(geex.combined.table(cll, input$comb.species, input$comb.scale), 
  #                    message="Calculating descriptive stats ...", detail="\nPlease wait for the full result table.")
  #     }
  #   });
  #   
  #   observeEvent(input$comb.filter, {
  #     if (input$comb.filter) {
  #       cll<-load.coll();
  #       sp<-names(geneset[[1]][[1]]);
  #       sp<-union(sp[sp %in% cll$gene$Species], 'human');
  #       output$comb.ui <- renderUI({ wellPanel(fluidRow(
  #         column(3, br(),
  #                selectizeInput("comb.gs.source", "Source", 
  #                               names(geneset), selected=names(geneset)[1], width='80%'), 
  #                selectizeInput("comb.gs.coll", "Collection", 
  #                               names(geneset[[1]]), selected=names(geneset[[1]])[1], width='80%'),
  #                selectizeInput("comb.gs.species", "Species", 
  #                               sp, selected='human', width='80%'), br(),
  #                column(2, h1(""), column(10, actionButton("comb.clear", 'Clear selection')))), 
  #         column(9, DT::dataTableOutput('comb.gs.table')))) }) 
  #     } else { output$comb.ui <- renderUI({br()}) }
  #   });
  #   
  #   observeEvent(input$comb.gs.source, { 
  #     updateSelectizeInput(session, 'comb.gs.coll', choices=names(geneset[[input$comb.gs.source]]))});  
  #   observeEvent(input$comb.gs.coll, { 
  #     cll<-load.coll();
  #     sp<-names(geneset[[input$comb.gs.source]][[input$comb.gs.coll]]);
  #     if (!identical(NA, cll)) sp<-union(sp[sp %in% as.vector(cll$gene$Species)], 'human');
  #     updateSelectizeInput(session, 'comb.gs.species', choices=sp, selected='human'); 
  #   });
  #   observeEvent(input$comb.gs.species, { 
  #     output$comb.gs.table <- DT::renderDataTable({ 
  #       geex.geneset.table(geneset, input$comb.gs.source, input$comb.gs.coll, input$comb.gs.species);  
  #     }, options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE)});
  #   observeEvent(input$comb.gs.clear, { 
  #     output$comb.gs.table <- DT::renderDataTable({ 
  #       geex.geneset.table(geneset, input$comb.gs.source, input$comb.gs.coll, input$comb.gs.species);  
  #     }, options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE)});
  #   
  #   output$comb.title<-renderUI({ 
  #     list(h2("Gene-Data set"), HTML('Summary of gene expression level across all data sets. Using relative expression percentiles is preferred if data sets were from different studies.')) });   
  #   output$comb.table <- DT::renderDataTable({
  #     cll<-load.coll();
  #     if (identical(NA, cll))  geex.empty.matrix(msg.nocollection) else { 
  #       tbl<-combine.dataset(); 
  #       if (colnames(tbl)[1] != 'ID') tbl else {
  #         cmn<-c('ID', input$comb.anno, input$comb.desc, 'Mx-Mn', rownames(cll$metadata$Dataset));
  #         cmn<-cmn[cmn %in% colnames(tbl)]; 
  #         tbl<-tbl[, cmn, drop=FALSE]; 
  #         
  #         rid<-CleanHtmlTags(input$comb.gs.table_rows_selected);
  #         if (length(rid)==0)  tbl else {
  #           rid<-rid[rid %in% rownames(geneset[[input$comb.gs.source]][[input$comb.gs.coll]][[input$comb.gs.species]])]; 
  #           gs<-readRDS(paste(GENESET_HOME, '/', tolower(input$comb.gs.source), '_list.rds', sep=''))[rid];  
  #           gn<-unique(unlist(gs, use.names=FALSE));
  #           gn<-gn[gn %in% rownames(tbl)];
  #           if (length(gn) == 0) geex.empty.matrix("No more genes after filtering") else tbl[gn, , drop=FALSE];
  #         }}}}, options = dt.options1, filter='bottom', selection='none', rownames=FALSE, server=TRUE, escape = FALSE);
  # }
  # 
  # ########################################## "Gene summary" tab ################################################      
  # {  
  #   observeEvent(input$gene.filter, {
  #     if (input$gene.filter) {
  #       cll<-load.coll();
  #       sp<-names(geneset[[1]][[1]]);
  #       sp<-union(sp[sp %in% cll$gene$Species], 'human');
  #       output$gene.ui <- renderUI({ wellPanel(fluidRow(
  #         column(3, br(),
  #                selectizeInput("gene.gs.source", "Source", 
  #                               names(geneset), selected=names(geneset)[1], width='80%'), 
  #                selectizeInput("gene.gs.coll", "Collection", 
  #                               names(geneset[[1]]), selected=names(geneset[[1]])[1], width='80%'),
  #                selectizeInput("gene.gs.species", "Species", sp, selected='human', width='80%'), br(),
  #                column(2, h1(""), column(10, actionButton("gene.clear", 'Clear selection')))), 
  #         column(9, DT::dataTableOutput('gene.gs.table')))) }) 
  #     } else { output$gene.ui <- renderUI({br()}) }
  #   });
  #   
  #   observeEvent(input$gene.gs.source, { 
  #     updateSelectizeInput(session, 'gene.gs.coll', choices=names(geneset[[input$gene.gs.source]]))});  
  #   observeEvent(input$gene.gs.coll, { 
  #     cll<-load.coll();
  #     sp<-names(geneset[[input$gene.gs.source]][[input$gene.gs.coll]]);
  #     if (!identical(NA, cll)) sp<-union(sp[sp %in% as.vector(cll$gene$Species)], 'human');
  #     updateSelectizeInput(session, 'gene.gs.species', choices=sp, selected='human'); 
  #   });
  #   observeEvent(input$gene.gs.species, { 
  #     output$gene.gs.table <- DT::renderDataTable({ 
  #       geex.geneset.table(geneset, input$gene.gs.source, input$gene.gs.coll, input$gene.gs.species);  
  #     }, options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE)});
  #   observeEvent(input$gene.clear, { 
  #     output$gene.gs.table <- DT::renderDataTable({ 
  #       geex.geneset.table(geneset, input$gene.gs.source, input$gene.gs.coll, input$gene.gs.species);  
  #     }, options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE)});
  #   
  #   output$gene.title<-renderUI({ 
  #     list(h2("Single gene information"), 
  #          HTML('Summarize the descriptive statistics of a single gene across data sets.')) }); 
  #   
  #   output$gene.message<-renderUI({
  #     cll<-load.coll();
  #     if (identical(NA, cll)) list(h3(msg.nocollection), br()) else {
  #       rid0<-input$gene.table_row_last_clicked; 
  #       rid1<-input$gene.table_rows_selected; 
  #       if (length(rid1)>0 & length(rid0)>0)  
  #         if (rid0 %in% rid1) geex.html.gene.name(rid0, TRUE, cll) else list(h3(msg.nogene), br()) else
  #           list(h3(msg.nogene), br())
  #     }});
  #   
  #   output$gene.table <- DT::renderDataTable({
  #     cll<-load.coll();
  #     if (!identical(cll, NA)) { 
  #       anno<-cll$gene;
  #       if (input$gene.filter) {
  #         rid<-CleanHtmlTags(input$gene.gs.table_rows_selected);
  #         if (length(rid)>0) {
  #           rid<-rid[rid %in% rownames(geneset[[input$gene.gs.source]][[input$gene.gs.coll]][[input$gene.gs.species]])]; 
  #           gs<-readRDS(paste(GENESET_HOME, '/', tolower(input$gene.gs.source), '_list.rds', sep=''))[rid];  
  #           gn<-unique(unlist(gs, use.names=FALSE));
  #           anno<-anno[rownames(anno) %in% gn, , drop=FALSE];
  #         }
  #       }
  #       if (nrow(anno) == 0) { 
  #         if (length(rid)>0) geex.empty.matrix("No more genes after filtering") else 
  #           geex.empty.matrix("No gene information in data set")
  #       } else {
  #         data.frame(ID=AddHref(rownames(anno), UrlEntrezGene(rownames(anno))), 
  #                    Name=as.vector(anno$Symbol), Species=as.vector(anno$Species),
  #                    Type=sub('^protein-coding', 'coding', as.vector(anno$type_of_gene)), 
  #                    N_Set=anno$Num_Dataset, Synonyms=as.vector(anno$Synonyms), 
  #                    Description=as.vector(anno$description), stringsAsFactors=FALSE);
  #       }
  #     } else geex.empty.matrix("Empty table")
  #   }, options = dt.options2, filter='bottom', selection='single', rownames=FALSE, escape = FALSE);
  #   
  #   output$gene.stat.table <- DT::renderDataTable({
  #     cll<-load.coll();
  #     rid0<-input$gene.table_row_last_clicked; 
  #     rid1<-input$gene.table_rows_selected; 
  #     if (length(rid1)>0 & length(rid0)>0)  if (rid0[1] %in% rid1) geex.summarize.gene(cll, rid0) else 
  #       geex.empty.matrix(msg.nogene) else
  #         geex.empty.matrix(msg.nogene); 
  #   }, options = dt.options1, filter='bottom', selection='single', rownames=FALSE, escape = FALSE);  
  # }
  # 

  session.data;
}