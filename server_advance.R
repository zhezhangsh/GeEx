server_advance <- function(input, output, session, session.data) {
  ##############################################################################################################
  ######################################## "Advance" menu ######################################################
  ##############################################################################################################
  
  ########################################## "filter gene" tab ######################################################
  {
    filter.table <- reactive({ 
      withProgress(tbl<-geex.filter.table(load.coll(), input$filter.dataset, input$filter.group, input$filter.sample), 
                   message='Preparing gene filtering table ...', detail='\nPlease wait until full table appears below.');
      updateSliderInput(session, 'filter.absolute', value=c(0, 100));
      updateSliderInput(session, 'filter.relative', value=c(0, 100)); 
      ch<-c('Higher', 'Lower');
      if (tbl$is.empty) updateRadioButtons(session, 'filter.direction', label=ch) else {
        names(ch)<-paste(c('Higher', 'Lower'), 'in', tbl$id); 
        updateRadioButtons(session, 'filter.direction', NULL, choices=ch, selected=ch[1], inline=TRUE);
      }
      
      tbl;
    });
    
    observeEvent(input$filter.dataset, {
      grp<-geex.longname.set2groups(load.coll(), input$filter.dataset);
      updateSelectizeInput(session, 'filter.group', choices=c('', grp));
    });
    observeEvent(input$filter.group, {
      smp<-geex.longname.group2samples(load.coll(), input$filter.group);
      updateSelectizeInput(session, 'filter.sample', choices=c('', smp));
    });
    
    output$filter.title<-renderUI({ 
      list(h2("Filter gene"), 
           HTML(c('Use this page to identify genes whose absolute or relative expression is among the highest or lowest in a data set/group/sample.', 
                  'All values in the tables are percentiles of expression (100 = highest expression).',
                  'The selection could be based on absolute expression level, or relative difference comparing to other data sets/groups/samples',
                  'The APP <strong>DeEx</strong> will provide more functions for analyzing differential expression.',
                  'Use cases of this page include:')), br(),
           HTML(' --- <i>Select genes having the highest/lowest expression level in a data set;</i>'), br(),
           HTML(' --- <i>Find genes activated or silenced in a specific tissue, developmental stage, etc.</i>'), br(),
           HTML(' --- <i>If a sample was identified as an outlier, which genes contribute the most to its difference from other samples.</i>')) 
    });    
    output$filter.msg<-renderUI({ list(h3(HTML(filter.table()$message)), br()) })
    output$filter.table <- DT::renderDataTable({  
      tbl<-filter.table(); 
      saveRDS(tbl, '/zhangz/x.rds');
      geex.empty.matrix('test');
      if (tbl$is.empty) geex.empty.matrix(tbl$message) else {
        g<-tbl$gex;
        range.obs<-input$filter.absolute;
        c<-round(g[, tbl$id]);
        g<-g[c>=min(range.obs) & c<=max(range.obs) & !is.na(c), , drop=FALSE];
        if (nrow(g)>0 & (input$filter.relative[1]>0 | input$filter.relative[2]<100)) {
          if (tolower(input$filter.direction) == 'lower') c<- -1*round(g[, paste(tbl$id, 'Min', sep='_vs_')]) else
            c<-round(g[, paste(tbl$id, 'Max', sep='_vs_')]);
          range.rel<-input$filter.relative;
          g<-g[c>=min(range.rel) & c<=max(range.rel) & !is.na(c), , drop=FALSE];
        }
        if (nrow(g) == 0) geex.empty.matrix("No genes left after filtering") else g;
      }
    }, options = dt.options1, filter='bottom', rownames=FALSE, server=TRUE, escape = FALSE);       
  }
  
  
  ########################################## "Re-group" tab ####################################################      
  # Allow users to regroup samples into datasets and groups.
  {
    observeEvent(input$regroup.clear.ds, { 
      output$regroup.dataset<-DT::renderDataTable({
        cll<-load.coll0();
        if (identical(NA, cll)) geex.empty.matrix(msg.nocollection) else 
          geex.regroup.options(cll$metadata$Sample);
      }, options = dt.options3, filter='none', rownames=FALSE, escape = FALSE);
      updateCheckboxInput(session, "regroup.check", 'Check box to activate sample re-grouping', value=FALSE);
    });
    observeEvent(input$regroup.clear.grp, { 
      output$regroup.group<-DT::renderDataTable({
        cll<-load.coll0();
        if (identical(NA, cll)) geex.empty.matrix(msg.nocollection) else
          geex.regroup.options(cll$metadata$Sample);
      }, options = dt.options3, filter='none', rownames=FALSE, escape = FALSE);
      updateCheckboxInput(session, "regroup.check", 'Check box to activate sample re-grouping', value=FALSE);
    });
    
    output$regroup.title<-renderUI({ 
      list(h2("Re-group samples"), HTML('Re-define data sets and groups based on sample features. Note that grouping samples from different studies into the same data set should always be avoided.')) 
    });
    output$regroup.grp.msg<-renderUI({
      rid<-input$regroup.group_rows_selected;
      if (length(rid)==0) h3(HTML("No feature(s) selected to define sample groups")) else 
        h3(HTML(paste("Define sample groups by", geex.html.msg(paste(rid, collapse=' & ')))));
    }); 
    output$regroup.ds.msg<-renderUI({
      rid<-input$regroup.dataset_rows_selected;
      if (length(rid)==0) h3(HTML("No feature(s) selected to define data sets")) else 
        h3(HTML(paste("Define data sets by", geex.html.msg(paste(rid, collapse=' & ')))));
    }); 
    output$regroup.dataset<-DT::renderDataTable({
      cll<-load.coll0();
      if (identical(NA, cll)) geex.empty.matrix(msg.nocollection) else {
        geex.regroup.options(cll$metadata$Sample);
      }
    }, options = dt.options3, filter='none', rownames=FALSE, escape = FALSE);
    output$regroup.group<-DT::renderDataTable({
      cll<-load.coll0(); 
      if (identical(NA, cll)) geex.empty.matrix(msg.nocollection) else
        geex.regroup.options(cll$metadata$Sample);
    }, options = dt.options3, filter='none', rownames=FALSE, escape = FALSE);
    
    output$regroup.ds.tbl <- DT::renderDataTable({
      rid<-input$regroup.dataset_rows_selected;
      cll<-load.coll();
      if (length(rid)==0) 
        updateCheckboxInput(session, "regroup.check", 'Check box to activate sample re-grouping', value=FALSE);
      if (identical(NA, cll)) geex.empty.matrix(msg.nocollection) else
        geex.regroup.outputs(cll$metadata$Sample, rid, c())$dataset;
    }, options = dt.options3, filter='none', rownames=FALSE, escape = FALSE);
    output$regroup.grp.tbl <- DT::renderDataTable({
      rid0<-input$regroup.dataset_rows_selected;
      rid<-input$regroup.group_rows_selected; 
      cll<-load.coll();
      if (length(rid0)==0 | length(rid)==0) 
        updateCheckboxInput(session, "regroup.check", 'Check box to activate sample re-grouping', value=FALSE);
      if (!identical(NA, cll) & length(rid0)==0) output$regroup.group<-DT::renderDataTable({
        geex.regroup.options(cll$metadata$Sample);
      }, options = dt.options3, filter='none', rownames=FALSE, escape = FALSE);
      if (identical(NA, cll)) geex.empty.matrix(msg.nocollection) else
        geex.regroup.outputs(cll$metadata$Sample, rid0, rid)$group;
    }, options = dt.options3, filter='none', rownames=FALSE, escape = FALSE);  
    
  }
  
  ########################################## "Upload" tab ######################################################
  {
    output$upload.title<-renderUI({ 
      list(h2("Upload user-specific data"), 
           HTML('It is possible to upload your own data collection and data sets for them to be explored in GeEx. However, two issues need to be addressed first:'), br(),
           HTML(' --- <strong>Data format</strong>. GeEx has very specific requirements about how the data collection should be formatted.'), br(),
           HTML(' --- <strong>Data privacy</strong>. If the uploaded data sets were not published yet, they need to be destroyed after each session.'), br(),
           HTML('So, please contact us (zhangz@email.chop.edu) if you have such need, and we will give you further details on how to make it happen.s')) 
    });
  }
  
  session.data;
}