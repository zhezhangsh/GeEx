geex.plot.geneset<-function(cll, grp, type, scale, color, normalize, gs) { 
  out <- list();
  
  scale <- tolower(scale);
  type  <- tolower(type);
  grp   <- as.vector(cll$mapping$longname2id[grp]);
  smp   <- cll$metadata$Sample;
  grp   <- grp[grp %in% rownames(cll$metadata$Group)];
  grp2smp <- split(rownames(smp), as.vector(smp$Group));
  
  if (identical(NULL, cll)) {
    plotly_empty() %>% layout(title='No data collection loaded', margin=list(t=150)); 
  } else if (length(grp)==0) {
    plotly_empty() %>% layout(title='Select at least 1 sample group', margin=list(t=150)); 
  } else {
    grp2smp <- grp2smp[grp];
    gex <- cll$gex_combined;
    if (scale=='unlogged') gex<-exp(gex$logged*log(2)) else if (scale=='percentile') gex<-gex$percentile else gex<-gex$logged;
    gex <- gex[, unlist(grp2smp, use.names=FALSE), drop=FALSE];
    gex <- gex[rownames(gex) %in% gs, , drop=FALSE];
    gex <- gex[!is.na(rowMeans(gex)), , drop=FALSE];
    if (nrow(gex) < 2) {
      plotly_empty() %>% layout(title="Data set include less than 2 genes in the gene set", margin=list(t=150)); 
    } else {  
      colnames(gex)<-paste(as.vector(cll$metadata$Sample[colnames(gex), 'Group']), colnames(gex), sep='_');
      grp2smp <- lapply(grp2smp, function(g) paste(as.vector(cll$metadata$Sample[g, 'Group']), g, sep='_'));
      gid <- rownames(gex);
      gnm <- as.vector(cll$gene[gid, 'Symbol']);
      rownames(gex) <- paste(gid, gnm, sep='_');

      if (normalize) {
        if (scale=='unlogged') key <- 'Re-scaled expression data' else 
          if (scale=='percentile') key <- 'Re-scaled expression percentile' else
            key <- 'Re-scaled and logged expression data';
      } else {
        if (scale=='unlogged') key <- 'Normalized expression data' else 
          if (scale=='percentile') key <- 'Normalized expression percentile' else
            key <- 'Normalized and logged expression data';
      }
      if (type=='heatmap') {
        if (normalize) gex <- t(scale(t(gex))); 
        PlotlyMatrixGroupHeatmap(d=gex, grp=grp2smp, color=color, key=key, clustering = TRUE); 
      } else if (type=='bar plot') { 
        if (normalize) gex <- t(scale(t(gex))); 
        g <- rep(cll$mapping$id2longname[names(grp2smp)], sapply(grp2smp, length));
        PlotlyBar(d=colMeans(gex[, unlist(grp2smp, use.names=FALSE), drop=FALSE]), group=g, col=color, title='Average expression of all genes in each group', ylab=key, o=0); 
      } else if (type=='box plot') { 
        if (normalize) gex <- t(scale(t(gex))); 
        g <- rep(cll$mapping$id2longname[names(grp2smp)], sapply(grp2smp, length));
        m <- lapply(grp2smp, function(g) colMeans(gex[, g, drop=FALSE])); 
        names(m) <- cll$mapping$id2longname[names(grp2smp)];
        c <- substr(GetColors(length(m), color), 1, 7); 
        mk <- list(size=6, color='black', symbol=2); 

        xa <- list(title='', zeroline=FALSE, showgrid=FALSE, showline=FALSE, tickangle = -30, 
                   tickmode='array', tickvals=names(m));
        ya <- list(title=key, zeroline=FALSE, showgrid=TRUE, showline=FALSE);
        mb <- 5+7.5*max(nchar(names(m)));
        
        p <- plot_ly(x=names(m)[1], y=m[[1]], type='box', boxpoints = "all", jitter = .5, pointpos = 0, fillcolor=c[1], 
                     marker=mk, showlegend = FALSE, opacity=.7); 
        if (length(m) > 1) for (i in 2:length(m)) p <- add_trace(p, x=names(m)[i], y=m[[i]], type='box', fillcolor=c[i]);
        p <- layout(p, xaxis=xa, yaxis=ya, margin=list(b=mb), title='Average expression of all genes in each sample');
        p;
      } else if (type == 'density plot') { 
        if (normalize) gex <- t(scale(t(gex))); 
        
        g <- cll$mapping$id2longname[names(grp2smp)];
        m <- lapply(grp2smp, function(g) rowMeans(gex[, g, drop=FALSE])); 
        d <- lapply(m, density); 

        c  <- substr(GetColors(length(m), color), 1, 7); 
        c1 <- apply(col2rgb(c), 2, function(c) paste('rgba(', paste(c, collapse=','), ',0.8)', sep=''));        
        c2 <- apply(col2rgb(c), 2, function(c) paste('rgba(', paste(c, collapse=','), ',0.4)', sep=''));        
        
        xa <- list(title=key, zeroline=TRUE, showgrid=TRUE, showline=TRUE); 
        ya <- list(title='Density', zeroline=FALSE, showgrid=TRUE, showline=TRUE);
        
        setProgress(value = 80); 
        
        p <- plot_ly(x = d[[1]]$x, y = d[[1]]$y, type = 'scatter', mode = 'lines', fill = 'tozeroy', 
                     line = list(color = c1[1]), fillcolor = c2[1], name = g[1], text = g[1]);
        if (length(d) > 1) 
          for (i in 2:length(d)) p <- add_trace(p, x=d[[i]]$x, y=d[[i]]$y, line=list(color=c1[i]), 
                                                fillcolor=c2[i], name=g[i], text = g[i]);
        p <- layout(p, xaxis=xa, yaxis=ya, legend = list(x = 0.02, y = 0.98));
        p; 
      } else if (type == 'cumulative plot') {
        if (normalize) gex <- t(scale(t(gex))); 
        g <- cll$mapping$id2longname[names(grp2smp)];
        
        m  <- lapply(grp2smp, function(g) rowMeans(gex[, g, drop=FALSE])); 
        mn <- min(sapply(m, function(x) min(x, na.rm=TRUE))); 
        mx <- max(sapply(m, function(x) max(x, na.rm=TRUE))); 
        d  <- lapply(m, function(m) knots(ecdf(m)));
        d  <- lapply(d, function(d) c(d[1], d)); 

        c  <- substr(GetColors(length(m), color), 1, 7); 
        c1 <- apply(col2rgb(c), 2, function(c) paste('rgba(', paste(c, collapse=','), ',0.8)', sep=''));        

        y <- 0:nrow(gex);
        
        xa <- list(title=key, zeroline=FALSE, showgrid=FALSE, showline=TRUE, 
                   range=c(mn-0.02*(mx-mn), mx+0.02*(mx-mn))); 
        ya <- list(title='Number of genes', zeroline=FALSE, showgrid=FALSE, showline=TRUE,
                   range=c(0, nrow(gex)));
        
        setProgress(value = 80); 
        
        p <- plot_ly(x = d[[1]], y = y, type = 'scatter', mode = 'lines', 
                     line = list(shape = "hv", color = c1[1]), name = g[1], text = g[1]);
        if (length(d) > 1) 
          for (i in 2:length(d)) 
            p <- add_trace(p, x=d[[i]], y=y, line=list(color=c1[i]), name=g[i], text = g[i]);
        p <- layout(p, xaxis=xa, yaxis=ya, legend = list(x = 0, y = 1));
        p; 
      } else {
        plotly_empty() %>% layout(title=paste("Unknown plot type:", type), margin=list(t=150)); 
      }
    }
  } 
}