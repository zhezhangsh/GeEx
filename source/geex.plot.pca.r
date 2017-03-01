geex.plot.pca<-function(cll, pc.x, pc.y, clr, ds, gp, rid, gn.subset) {
  # cll         A data collection returned by geex.load.collection
  # pc.x, pc.y  The PCs to show
  # clr         The color choice
  # ds          Data set long name as selected by pca.dataset
  # gp          Selected groups to be included
  # rid         Row ID of samples to highlight

  if (identical(NULL, cll) | ds[1]=='') plotly_empty() else { 
    ds.id<-as.vector(cll$mapping$longname2id[ds]);
    d <- geex.load.dataset(cll, ds); 
    e <- d$data$logged;
    e <- e[!is.na(rowSums(e, na.rm=TRUE)), , drop=FALSE]; 
    smp<-cll$metadata$Sample;
    smp<-smp[smp$Dataset==ds.id, , drop=FALSE];
    grp<-cll$metadata$Group[as.vector(smp$Group), , drop=FALSE];
    grp.nm<-sort(paste(as.vector(smp$Group), as.vector(grp$Name), sep=': '));
    e<-e[, rownames(smp), drop=FALSE];
    
    col0<-GetColors(length(unique(smp$Group)), clr);
    names(col0)<-unique(grp.nm);
    col<-col0[grp.nm];
    
    e<-e[, grp.nm %in% gp, drop=FALSE];
    col<-col[grp.nm %in% gp];
    grp.nm<-grp.nm[grp.nm %in% gp];
    
    if (ncol(e) < 2) plotly_empty() else {
      gn<-gn.subset[gn.subset %in% rownames(e)]; 
      if (!identical(NA, gn.subset) & length(gn)<3) plotly_empty() else {
        if (length(gn) > 2) e<-e[gn, ];
        a<-d$anno[rownames(e), , drop=FALSE];
        if (length(setdiff(tolower(a$Species), 'human'))>0)  e<-e[a$Species!='human', , drop=FALSE];
        cex<-max(10, min(20, 240/ncol(e)));
        pr<-prcomp(t(e));
        
        pc<-as.numeric(sub('PC', '', c(pc.x, pc.y), ignore.case=TRUE));
        pt <- round(summary(pr)$importance[2, pc] * 100, 2);
        
        xs <- pr$x[, pc[1]];
        ys <- pr$x[, pc[2]];
        xr <- range(xs); 
        yr <- range(ys); 
        xx <- c(xr[1]-0.05*(xr[2]-xr[1]), xr[2]+0.05*(xr[2]-xr[1])); 
        yy <- c(yr[1]-0.05*(yr[2]-yr[1]), yr[2]+0.05*(yr[2]-yr[1])); 
        lb <- paste('PC', pc, ', ', pt[pc], '%', sep=''); 
        ln <- list(width=rep(1, length(xs)), color='rgba(152, 0, 0, 1)');
        cl <- substr(as.vector(col), 1, 7);
        mk <- list(size = cex, line=ln, color=cl, symbol=13); 
        
        if (length(rid) > 0) mk$line$width[rid] <- 3;
        
        dd <- data.frame(X=xs, Y=ys, text=rownames(pr$x), stringsAsFactors = FALSE); 
        
        plot_ly(data=dd, x=~X, y=~Y, type='scatter', mode='marker', hoverinfo="text", text=~text, color=names(col), marker=mk) %>%
          layout(
            showlegend = TRUE, 
            xaxis = list(title=lb[1], range=xx, zeroline=FALSE, showgrid=TRUE, showline=FALSE, showticklabels=TRUE),
            yaxis = list(title=lb[2], range=yy, zeroline=FALSE, showgrid=TRUE, showline=FALSE, showticklabels=TRUE),
            shapes = list(list(type = 'rect', xref = 'x', x0 = xx[1], x1 = xx[2], yref = 'y', y0 = yy[1], y1 = yy[2],
                               fillcolor = 'rgba(0, 0, 0, 0)', line = list(color = '#000000'), opacity = 1))
          )
        
        
        # PlotPCA(pr, grp.nm, col=col, cex=cex, legend=TRUE, highlight=rid, dimensions=pc, new.window=FALSE);
      }
    }
  }
}