server_visualization <- function(input, output, session, session.data) {
  
  ###################################################################################################
  ######################################## "Visualize" menu #########################################
  ###################################################################################################
  
  ########################################################################################
  ######################################## "PCA Plot" tab ################################
  {
    observeEvent(input$pca.dataset, {
      ds<-geex.load.dataset(load.coll(), input$pca.dataset);
      if (!identical(NA, ds)) {
        updateCheckboxGroupInput(session, 'pca.group', choices=names(ds$group), selected=names(ds$group));
        updateSelectizeInput(session, 'pca.x', choices=paste('PC', 1:ncol(ds$data[[1]]), sep=''), selected='PC1');
        updateSelectizeInput(session, 'pca.y', choices=paste('PC', 1:ncol(ds$data[[1]]), sep=''), selected='PC2');
        
        sp0<-input$pca.geneset.species;
        sp<-names(geneset[[input$pca.geneset.source]][[input$pca.geneset.coll]]);
        ds<-geex.load.dataset(load.coll(), input$pca.dataset);
        ds.sp<-unique(as.vector(ds$anno$Species));
        sp<-sp[sp %in% ds.sp];
        sp<-c(sp[tolower(sp)!='human'], 'human'); 
        if (sp0 %in% sp) sp1<-sp0 else sp1<-sp[1]; 
        updateSelectizeInput(session, 'pca.geneset.species', choices=sp, selected=sp1);      
      }
    });
    observeEvent(input$pca.clear, {
      if (!identical(NA, cll<-load.coll()))
        output$pca.table <- DT::renderDataTable({
          cll$metadata$Sample[cll$metadata$Sample$Dataset == strsplit(input$pca.dataset, ': ')[[1]][1], 
                              c('Name', 'Group'), drop=FALSE];
        }, options = dt.options3, escape = FALSE);
    });
    
    observeEvent(input$pca.geneset.source, { 
      updateSelectizeInput(session, 'pca.geneset.coll', choices=names(geneset[[input$pca.geneset.source]])); 
    });
    observeEvent(input$pca.geneset.coll, { 
      sp<-names(geneset[[input$pca.geneset.source]][[input$pca.geneset.coll]]);
      if (!identical(NA, load.coll())) {
        ds<-geex.load.dataset(load.coll(), input$pca.dataset);
        ds.sp<-unique(as.vector(ds$anno$Species));
        sp<-sp[sp %in% ds.sp];
        sp<-c(sp[tolower(sp)!='human'], 'human');
      };
      updateSelectizeInput(session, 'pca.geneset.species', choices=sp); 
    });
    observeEvent(input$pca.geneset.species, { 
      output$pca.geneset.table <- DT::renderDataTable({ 
        geex.geneset.table(geneset, input$pca.geneset.source, input$pca.geneset.coll, input$pca.geneset.species);  
      }, options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE)
    });
    observeEvent(input$pca.geneset.clear, { 
      output$pca.geneset.table <- DT::renderDataTable({ 
        geex.geneset.table(geneset, input$pca.geneset.source, input$pca.geneset.coll, input$pca.geneset.species);  
      }, options = dt.options2, rownames=FALSE, selection='single', server=TRUE, escape = FALSE)
    });
    
    output$pca.title<-renderUI({list(
      h2("Principal Components Analysis"), 
      HTML(paste('Unsupervised clustering of samples in the same data set by',  
                 '<a href="https://en.wikipedia.org/wiki/Principal_component_analysis" target="_blank">PCA</a>'))) });  
    output$pca.message<-renderUI({
      if (identical(NA, load.coll())) list(h3(HTML(msg.nocollection)), br(), br())  else 
        list(h3(HTML(paste("Data set", geex.html.msg(input$pca.dataset)))), br(), br()) })
    output$pca.plot <- renderPlot({
      if (length(input$pca.geneset.table_rows_selected) > 0) {
        src<-input$pca.geneset.source;
        cll<-input$pca.geneset.coll;
        spe<-input$pca.geneset.species;
        rid<-CleanHtmlTags(input$pca.geneset.table_rows_selected);
        rid<-rid[rid %in% rownames(geneset[[src]][[cll]][[spe]])]; 
        if (length(rid) > 0) gn.subset<-unique(unlist(readRDS(paste(GENESET_HOME, '/', tolower(src), '_list.rds', sep=''))[rid], use.names=FALSE)) else 
          gn.subset<-c();
      } else gn.subset<-NA;
      geex.plot.pca(load.coll(), input$pca.x, input$pca.y, input$pca.color, input$pca.dataset, input$pca.group, 
                    input$pca.table_rows_selected, gn.subset); }, height = 720, width = 960);
    
    output$pca.table <- DT::renderDataTable({
      cll<-load.coll();
      if (identical(cll, NA)) geex.empty.matrix("Empty table") else 
        cll$metadata$Sample[cll$metadata$Sample$Dataset == strsplit(input$pca.dataset, ': ')[[1]][1], 
                            c('Name', 'Group'), drop=FALSE];
    }, options = dt.options2, escape = FALSE);
  }
  
  ########################################################################################
  ######################################## "Bar Plot" tab ################################ 
  {
    select.gene <- reactive({
      cll<-load.coll();
      
      ds<-input$bar.dataset;
      rid<-input$bar.table_rows_selected;
      selected<-geex.select.gene(cll, ds, input$bar.group, rid);
      
      updateSelectizeInput(session, 'bar.dataset', choices=selected$dataset, selected=ds);
      
      if (length(rid)==0 | identical(cll, NA)) output$bar.dataset.message<-renderUI({ '' }) else {
        output$bar.dataset.message<-renderUI({
          if (setequal(rownames(cll$metadata$Dataset), selected$dataset)) '' else 
            list(HTML(geex.html.msg("Only data sets including all selected genes are listed;")), br(), HTML(geex.html.msg("Clear gene selection to list all data sets.")))
        }) 
      }     
      selected;
    });
    
    observeEvent(input$bar.dataset, {
      ds<-geex.load.dataset(load.coll(), input$bar.dataset);
      if (!identical(NA, ds)) {
        updateCheckboxGroupInput(session, 'bar.group', choices=names(ds$group), selected=names(ds$group));   
        if (length(input$bar.table_rows_selected) == 0) output$bar.table <- DT::renderDataTable({ ds$anno; }, options = dt.options3, rownames=FALSE, server=TRUE, escape = FALSE);
      } else {
        output$bar.table <- DT::renderDataTable({ geex.empty.matrix(msg.nodataset) }, options = dt.options3, rownames=FALSE, server=TRUE, escape = FALSE);
      }
    });
    
    observeEvent(input$bar.clear, {
      cll<-load.coll();
      ds<-input$bar.dataset;
      output$bar.table <- DT::renderDataTable({ 
        if (!identical(NA, cll)) geex.load.dataset(cll, ds)$anno else
          geex.empty.matrix(msg.nocollection);
      }, options = dt.options3, rownames=FALSE, server=TRUE, escape = FALSE); 
      updateSelectInput(session, 'bar.dataset', choices=cll$extra$longname$dataset, selected=ds);
    });
    
    observeEvent(input$bar.table_row_last_clicked, {
      ds<-geex.load.dataset(load.coll(), input$bar.dataset);
      rid<-CleanHtmlTags(input$bar.table_rows_selected);
      rid0<-CleanHtmlTags(input$bar.table_row_last_clicked);
      output$bar.table.message<-renderUI({ '' })
      if (length(rid)>0) if (rid[length(rid)]==rid0) if (!(rid0 %in% rownames(ds$anno))) {
        output$bar.table.message<-renderUI({
          list(HTML(geex.html.msg(paste("Gene <strong>", rid0, "</strong> is not included in data set ", " <strong>", ds$longname, '</strong>', sep=''))), br(),
               HTML(geex.html.msg("Clear gene selction to show all genes available in current data set")))
        });
      }
    })
    
    output$bar.title<-renderUI({ list(
      h2("Expression level of individual gene(s)"), 
      HTML("Make bar plot of individual genes to compare their expression levels to each other or between samples.")); });
    
    output$bar.message<-renderUI({ h3(select.gene()$message) });
    
    output$bar.stat <- DT::renderDataTable({ select.gene()$table; }, options = dt.options5, rownames=FALSE, server=TRUE, escape = FALSE);
    
    output$bar.plot <- renderPlot({ 
      ds<-geex.load.dataset(load.coll(), input$bar.dataset); 
      geex.plot.bar(ds, input$bar.stat_rows_all, input$bar.scale, input$bar.color, input$bar.group, input$bar.mean);  
    });
    
    ########################## Gene dictionary 
    observeEvent(input$lookup.option, {
      cll<-load.coll();
      if (input$lookup.option & !identical(NA, cll)) {
        output$lookup.table<-DT::renderDataTable({ geex.empty.matrix('No search key'); }, options=list(dom='t'))
        output$lookup.key.ui<-renderUI({ textInput("bar.lookup.key", "", '') });
        output$lookup.species.ui<-renderUI({ 
          selectizeInput("bar.lookup.species", '', choices=unique(cll$gene$Species), selected='human') });
        output$lookup.table.ui<-renderUI({ DT::dataTableOutput('bar.lookup.table') });
      } else { 
        updateTextInput(session, "bar.lookup.key", NULL, NULL);
        updateSelectizeInput(session, "bar.lookup.species", NULL, NULL, NULL);
        output$lookup.key.ui <- renderUI({br()});
        output$lookup.species.ui <- renderUI({br()}); 
        output$lookup.table.ui <- renderUI({br()}); 
      }
    });
    observeEvent(input$bar.lookup.key, {
      ky<-input$bar.lookup.key;
      output$bar.lookup.table <- DT::renderDataTable({
        if (ky!='' & !is.null(ky)) geex.lookup.gene(geex.load.dataset(load.coll(), input$bar.dataset)$anno, input$bar.lookup.species, ky, GENE_HOME) else 
          geex.empty.matrix('No search key');
      }, options = dt.options4, selection='none', rownames=FALSE, server=TRUE, escape = FALSE);
    });
    
    ########################## 
    
  } # end of menu tab
  
  ########################################################################################
  ######################################## "Geneset" tab #################################
  {
    observeEvent(input$geneset.dataset, {
      ds<-geex.load.dataset(load.coll(), input$geneset.dataset);
      if (!identical(NA, ds)) {
        updateCheckboxGroupInput(session, 'geneset.group', choices=names(ds$group), selected=names(ds$group));
        sp0<-input$geneset.species;
        sp<-names(geneset[[input$geneset.source]][[input$geneset.coll]]);
        ds<-geex.load.dataset(load.coll(), input$geneset.dataset);
        ds.sp<-unique(as.vector(ds$anno$Species));
        sp<-sp[sp %in% ds.sp];
        sp<-c(sp[tolower(sp)!='human'], 'human');
        if (sp0 %in% sp) sp1<-sp0 else sp1<-sp[1];
        updateSelectizeInput(session, 'geneset.species', choices=sp, selected=sp1);       
      }  
    });
    observeEvent(input$geneset.source, { 
      updateSelectizeInput(session, 'geneset.coll', choices=names(geneset[[input$geneset.source]])); 
    });
    observeEvent(input$geneset.coll, { 
      sp<-names(geneset[[input$geneset.source]][[input$geneset.coll]]);
      if (!identical(NA, load.coll())) {
        ds<-geex.load.dataset(load.coll(), input$geneset.dataset);
        ds.sp<-unique(as.vector(ds$anno$Species));
        sp<-sp[sp %in% ds.sp];
        sp<-c(sp[tolower(sp)!='human'], 'human');
      };
      updateSelectizeInput(session, 'geneset.species', choices=sp); 
    });
    observeEvent(input$geneset.species, { 
      output$geneset.table <- DT::renderDataTable({ 
        geex.geneset.table(geneset, input$geneset.source, input$geneset.coll, input$geneset.species);  
      }, options = dt.options3, rownames=FALSE, selection='single', server=TRUE, escape = FALSE) 
      output$geneset.stat <- DT::renderDataTable({ 
        geex.empty.matrix(msg.nogeneset); }, options=list(dom='t'), selection='none');
    });
    observeEvent(input$geneset.clear, { 
      output$geneset.table <- DT::renderDataTable({ 
        geex.geneset.table(geneset, input$geneset.source, input$geneset.coll, input$geneset.species);  
      }, options = dt.options3, rownames=FALSE, selection='single', server=TRUE, escape = FALSE) 
      output$geneset.stat <- DT::renderDataTable({ 
        geex.empty.matrix(msg.nogeneset); }, options=list(dom='t'), selection='none');
    });
    
    output$geneset.title<-renderUI({ list(
      h2("Expression pattern of a gene set"),
      HTML('Compare and visualize overall expression levels of a gene set between sample groups.')) });
    output$geneset.message<-renderUI({
      if (identical(NA, load.coll())) list(h3(msg.nocollection), br(), br()) else {
        rid0<-input$geneset.table_rows_selected;
        rid<-input$geneset.table_row_last_clicked;
        if (length(rid0)>0 & length(rid)>0) {
          if (rid0[length(rid0)]==rid) {
            rid<-CleanHtmlTags(input$geneset.table_rows_selected);
            nm<-as.vector(geneset[[input$geneset.source]][[input$geneset.coll]][[input$geneset.species]][rid[length(rid)], 'Name'])
            list(h3(HTML(paste('Gene set', geex.html.msg(nm)))), br(), br());
          } else list(h3(msg.nogeneset), br(), br());
        } else list(h3(msg.nogeneset), br(), br());
      }
    });
    
    output$geneset.plot <- renderPlot({
      rid0<-input$geneset.table_rows_selected;
      rid<-input$geneset.table_row_last_clicked;
      if (length(rid0)>0 & length(rid)>0) if (rid0[length(rid0)]==rid) {
        src<-input$geneset.source;
        cll<-input$geneset.coll;
        spe<-input$geneset.species;
        grp<-input$geneset.group;
        rid<-CleanHtmlTags(input$geneset.table_rows_selected);
        rid<-rid[rid %in% rownames(geneset[[src]][[cll]][[spe]])]; 
        rid<-rid[length(rid)];
        gs<-readRDS(paste(GENESET_HOME, '/', tolower(src), '_list.rds', sep=''))[rid];
        if (length(gs) > 0) {
          names(gs)<-as.vector(geneset[[src]][[cll]][[spe]][names(gs), 'Name']);  
          out<-geex.plot.geneset(load.coll(), grp, input$geneset.type, input$geneset.scale, 
                                 input$geneset.color, input$geneset.normalize, gs[[1]]);  
        }
      }
    });
    
  }
  
  ########################################################################################
  ######################################## "Compare two tab" tab #########################
  {
    observeEvent(input$x.dataset, {
      ch.x<-geex.longname.set2groups(load.coll(), input$x.dataset);
      updateSelectizeInput(session, 'x.group', choices=ch.x);
    });
    observeEvent(input$y.dataset, {
      ch.y<-geex.longname.set2groups(load.coll(), input$y.dataset);
      updateSelectizeInput(session, 'y.group', choices=ch.y, selected=ch.y[length(ch.y)]);
    });
    
    observeEvent(input$x.group, {
      output$two.message<-renderUI({ 
        list(h3(HTML(paste(geex.html.msg(input$x.group), 'vs.', geex.html.msg(input$y.group)))), br()); });
      sp<-geex.get2group.species(load.coll(), input$x.group, input$y.group); 
      output$two.table<-DT::renderDataTable({ 
        geex.get2group(load.coll(), input$x.group, input$y.group, sp[1])
      }, options=dt.options2, rownames=FALSE, filter='bottom', server=TRUE, escape = FALSE); 
      updateSelectizeInput(session, "two.select.species", choices=sp);
      updateSelectizeInput(session, 'x.sample', 
                           choices=c('', geex.longname.group2samples(load.coll(), input$x.group)));
    });
    observeEvent(input$y.group, { 
      output$two.message<-renderUI({ 
        list(h3(HTML(paste(geex.html.msg(input$x.group), 'vs.', geex.html.msg(input$y.group)))), br()); });    
      sp<-geex.get2group.species(load.coll(), input$x.group, input$y.group); 
      output$two.table<-DT::renderDataTable({ 
        geex.get2group(load.coll(), input$x.group, input$y.group, sp[1])
      }, options=dt.options2, rownames=FALSE, filter='bottom', server=TRUE, escape = FALSE); 
      updateSelectizeInput(session, "two.select.species", choices=sp);
      updateSelectizeInput(session, 'y.sample', 
                           choices=c('', geex.longname.group2samples(load.coll(), input$y.group)));
    });
    observeEvent(input$x.sample, {
      if (input$x.sample != '') lab.x<-input$x.sample else lab.x<-input$x.group;
      if (input$y.sample != '') lab.y<-input$y.sample else lab.y<-input$y.group;
      output$two.message<-renderUI({ 
        list(h3(HTML(paste(geex.html.msg(lab.x), 'vs.', geex.html.msg(lab.y)))), br()); });
    });
    observeEvent(input$y.sample, {
      if (input$x.sample != '') lab.x<-input$x.sample else lab.x<-input$x.group;
      if (input$y.sample != '') lab.y<-input$y.sample else lab.y<-input$y.group;
      output$two.message<-renderUI({ 
        list(h3(HTML(paste(geex.html.msg(lab.x), 'vs.', geex.html.msg(lab.y)))), br()); });
    });
    
    observeEvent(input$two.source, { 
      updateSelectizeInput(session, 'two.coll', choices=names(geneset[[input$two.source]])); 
    });
    observeEvent(input$two.coll, { 
      sp<-names(geneset[[input$two.source]][[input$two.coll]]);
      if (!identical(NA, load.coll())) 
        sp<-unique(c(sp[sp %in% geex.get2group.species(load.coll(), input$x.group, input$y.group)], 'human'));
      updateSelectizeInput(session, 'two.species', choices=sp); 
    });
    observeEvent(input$two.species, { 
      output$two.table.geneset <- DT::renderDataTable({  geex.geneset.table(geneset, input$two.source, input$two.coll, input$two.species);  
      }, options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE) 
      output$two.geneset.stat <- DT::renderDataTable({ geex.empty.matrix(msg.nogeneset); }, options=list(dom='t'), selection='none');
    });
    observeEvent(input$two.clear.geneset, { 
      output$two.table.geneset <- DT::renderDataTable({ geex.geneset.table(geneset, input$two.source, input$two.coll, input$two.species);  
      }, options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE) 
      output$two.geneset.stat <- DT::renderDataTable({ geex.empty.matrix(msg.nogeneset); }, options=list(dom='t'), selection='none');
    });
    observeEvent(input$two.clear, { 
      output$two.table <- DT::renderDataTable({ 
        geex.get2group(load.coll(), input$x.group, input$y.group, input$two.select.species)
      }, options=dt.options2, rownames=FALSE, filter='bottom', server=TRUE, escape = FALSE); 
    });
    observeEvent(input$two.select.species, { 
      sp<-geex.get2group.species(load.coll(), input$x.group, input$y.group);
      sp1<-names(geneset[[input$two.source]][[input$two.coll]]);
      if (!setequal(sp, sp1) | input$two.select.species!=input$two.species) {
        sp2<-unique(c(sp1[sp1 %in% sp], 'human'));
        sp3<-input$two.select.species;
        if (!(sp3 %in% sp2)) sp3<-'human'
        updateSelectizeInput(session, 'two.species', sp2, sp3); 
      }
      output$two.table<-DT::renderDataTable({ 
        geex.get2group(load.coll(), input$x.group, input$y.group, sp<-input$two.select.species)
      }, options=dt.options2, rownames=FALSE, filter='bottom', server=TRUE, escape = FALSE); 
    });
    
    output$two.title<-renderUI({list(
      h2("Differential expression between two groups/samples"),
      HTML('Compare expression levels of any 2 groups (default) or samples at 3 levels: global, gene set, and single gene.'))});
    output$two.message<-renderUI({if (identical(NA, load.coll())) list(h3(msg.nocollection), br(), br()); });
    output$two.table <- DT::renderDataTable({ geex.empty.matrix('Empty table'); });
    
    output$two.plot <- renderPlot({
      src<-input$two.source;
      cll<-input$two.coll;
      spe<-input$two.species; 
      rid<-CleanHtmlTags(input$two.table.geneset_rows_selected);
      rid<-rid[rid %in% rownames(geneset[[src]][[cll]][[spe]])]; 
      gs<-list();
      if (length(rid)==0) spe<-input$two.select.species else {
        gs<-readRDS(paste(GENESET_HOME, '/', tolower(src), '_list.rds', sep=''))[rid];
        if (length(gs) > 0) names(gs)<-as.vector(geneset[[src]][[cll]][[spe]][names(gs), 'Name']);  
      }
      cnm.x<-input$x.group;
      cnm.y<-input$y.group;
      if (input$x.sample != '') cnm.x<-input$x.sample;
      if (input$y.sample != '') cnm.y<-input$y.sample; 
      if (cnm.x!='' & cnm.y!='') 
        out<-geex.plot.two(load.coll(), cnm.x, cnm.y, input$two.type, input$two.scale, input$two.color, 
                           spe, input$two.table_rows_selected, gs);  
      if (length(gs) == 0) output$two.geneset.stat<-DT::renderDataTable({ 
        geex.empty.matrix(msg.nogeneset); 
      }, options=list(dom='t'), selection='none') else
        output$two.geneset.stat<-DT::renderDataTable({ 
          out$geneset.stat 
        }, options=dt.options5, rownames=FALSE, selection='none', server=TRUE, escape = FALSE); 
    });
  }
  
  
  ########################################################################################
  ########################################## "Coexpression" tab ##########################      
  {
    coex.results<-reactive({ 
      cll<-load.coll();
      
      rid1s<-input$coex.gene1_rows_selected; 
      rid1<-input$coex.gene1_row_last_clicked;
      if (length(rid1)>0 & length(rid1s)>0) if (rid1 %in% rid1s) id1<-CleanHtmlTags(rid1) else id1<-c() else id1<-c();
      
      rid2s<-input$coex.gene2_rows_selected; 
      rid2<-input$coex.gene2_row_last_clicked;
      if (length(rid2)>0 & length(rid2s)>0) if(rid2 %in% rid2s) id2<-CleanHtmlTags(rid2) else id2<-c() else id2<-c()
      
      coex<-geex.coex.data(cll, id1, id2, input$coex.scale, input$coex.color);
      
      coex;
    });
    
    observeEvent(input$coex.lookup.option, {
      cll<-load.coll();
      if (input$coex.lookup.option & !identical(NA, cll)) {
        output$coex.lookup.table<-DT::renderDataTable({ geex.empty.matrix('No search key'); }, options=list(dom='t'))
        output$coex.lookup.key.ui<-renderUI({ textInput("coex.lookup.key", "", '') });
        output$coex.lookup.species.ui<-renderUI({ 
          selectizeInput("coex.lookup.species", '', choices=unique(cll$gene$Species), selected='human') });
        output$coex.lookup.table.ui<-renderUI({ DT::dataTableOutput('coex.lookup.table') });
      } else { 
        updateTextInput(session, "coex.lookup.key", NULL, NULL);
        updateSelectizeInput(session, "coex.lookup.species", NULL, NULL, NULL);
        output$coex.lookup.key.ui <- renderUI({br()});
        output$coex.lookup.species.ui <- renderUI({br()}); 
        output$coex.lookup.table.ui <- renderUI({br()}); 
      }
    }); 
    observeEvent(input$coex.lookup.key, {
      ky<-input$coex.lookup.key;
      output$coex.lookup.table <- DT::renderDataTable({
        if (ky!='' & !is.null(ky)) geex.lookup.gene(geex.load.dataset(load.coll(), input$bar.dataset)$anno, input$coex.lookup.species, ky, GENE_HOME) else 
          geex.empty.matrix('No search key');
      }, options = dt.options4, selection='none', rownames=FALSE, server=TRUE, escape = FALSE)   
    });
    
    observeEvent(input$coex.clear.ds, {
      output$coex.dataset <- DT::renderDataTable({  
        coex<-coex.results(); 
        if (!coex$plot) geex.empty.matrix(coex$status) else coex$stat;
      }, options = dt.options3, rownames=FALSE, server=TRUE, escape = FALSE);   
    });
    
    output$coex.title<-renderUI({ 
      list(h2("Gene-gene coexpression"), 
           HTML(c('Coexpression of two selected genes within and across data sets.', 
                  'The APP <strong>CoEx</strong> will provide more functions for coexpression analysis.'))) 
    });
    
    output$coex.message<-renderUI({ 
      cll<-load.coll();
      
      rid1s<-input$coex.gene1_rows_selected; 
      rid1<-input$coex.gene1_row_last_clicked;
      if (length(rid1)>0 & length(rid1s)>0) if (rid1 %in% rid1s) id1<-CleanHtmlTags(rid1) else id1<-c() else id1<-c();
      
      rid2s<-input$coex.gene2_rows_selected; 
      rid2<-input$coex.gene2_row_last_clicked;
      if (length(rid2)>0 & length(rid2s)>0) if(rid2 %in% rid2s) id2<-CleanHtmlTags(rid2) else id2<-c() else id2<-c()
      
      if (length(id1)>0) nm1<-as.vector(cll$gene[id1, 'Symbol']) else nm1<-'';
      if (length(id2)>0) nm2<-as.vector(cll$gene[id2, 'Symbol']) else nm2<-'';
      
      if (nm1=='' & nm2=='') h3(HTML(geex.html.msg(msg.nogene))) else 
        h3(HTML(paste(geex.html.msg(nm1), 'vs.', geex.html.msg(nm2))));      
    });
    
    output$coex.dataset <- DT::renderDataTable({  
      coex<-coex.results(); 
      if (!coex$plot) geex.empty.matrix(coex$status) else coex$stat;
    }, options = dt.options3, rownames=FALSE, server=TRUE, escape = FALSE);   
    
    output$coex.plot <- renderPlot({  
      coex<-coex.results();
      geex.coex.plot(coex, type=which(data.level.options==input$coex.type), input$coex.dataset_rows_selected);
    })
  }
  
  session.data;
}