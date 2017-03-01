# make barplot of a single gene
geex.plot.bar<-function(ds, rid, scale, color, selected.group, show.mean=FALSE) { 
  if (identical(NA, ds)) {
    plotly_empty() #%>% layout(title='No data collection loaded', margin=list(t=150)); 
  } else if (length(selected.group)==0) {
    plotly_empty() #%>% layout(title='Select at least 1 sample group', margin=list(t=150)); 
  } else if (length(rid)==0) {
    plotly_empty() #%>% layout(title='Select at least 1 gene', margin=list(t=150)); 
  } else {
    if (scale=='unlogged') d<-exp(ds$data$logged*log(2)) else d<-ds$data[[scale]];
    
    rid <- rownames(d)[as.integer(rid)];
    grp <- ds$group;
    grp <- lapply(grp, function(grp) grp[grp %in% colnames(d)]);
    grp <- grp[sapply(grp, length)>0];

    setProgress(value = 40); 
    
    if (length(rid)==0) plotly_empty() else {
      if (!is.null(selected.group)) selected.group <- selected.group[!is.na(selected.group) & selected.group!=''];
      if (length(selected.group) == 0) plotly_empty() else {
        grp <- grp[names(grp) %in% selected.group];
        grp <- grp[order(names(grp))];
        
        if (ncol(d) > 20) ln <- list() else ln <- list(color = "rgb(8,48,107,.5)", width = 0.5);
        ylab <- paste('Expression level,', scale);
        smp <- unlist(grp, use.names=FALSE);
        d0  <- round(d[rid, smp, drop=FALSE], 3);
        gn  <- ds$anno[rid, 'Name'];
        mgb  <- 5 + 7.5 * max(nchar(names(grp)));
        bgp <- min(0.5, 0.1 + round(1/nrow(d0), 1))/2;
        
        if (length(grp) == 0) plotly_empty() else {
          if (!show.mean) {
            col0 <- GetColors(length(grp), color);
            col0 <- substr(col0, 1, 7);
            names(col0) <- names(grp);
            col1 <- rep(col0[names(grp)], sapply(grp, length));

            setProgress(value = 60); 
            
            p <- plot_ly(x=grp[[1]], y=d0[1, grp[[1]]], type='bar', showlegend=TRUE, name=names(grp)[1], 
                         text=gn[1], marker=list(color=col0[1], line=ln)); 
            
            if (nrow(d0) > 1) {
              for (i in 2:nrow(d0)) {
                p <- add_trace(p, x=grp[[1]], y=d0[i, grp[[1]]], showlegend=FALSE, text=gn[i], 
                               marker=list(color=col0[1], line=ln));
              }
            }
            
            if (length(grp) > 1) {
              for (j in 2:length(grp)) {
                p <- add_trace(p, x=grp[[j]], y=d0[1, grp[[j]]], type='bar', showlegend=TRUE, name=names(grp)[j], 
                               text=gn[j], marker=list(color=col0[j], line=ln));

                if (nrow(d0) > 1) {
                  for (i in 2:nrow(d0)) {
                    p <- add_trace(p, x=grp[[j]], y=d0[i, grp[[j]]], showlegend=FALSE, marker=list(color=col0[j], line=ln));
                  } 
                }
              }
            }
            
            setProgress(value = 80); 
            
            p <- layout(p, barmode = "group", margin = list(b = mgb), yaxis = list(title = ylab), 
                        xaxis = list(autotick = FALSE, tickangle = -45, range=c(-1, ncol(d0))));
            p; 
          } else {
            col0 <- GetColors(length(rid), color); 
            col0 <- substr(col0, 1, 7);
            if (length(col0) == 1) col0[1] <- '#888888'
            names(col0) <- names(rid);
            gn <- ds$anno[rid, 'Name'];
            
            if (length(selected.group) == 1) {
              m  <- round(sapply(rid, function(rid) sapply(grp, function(s) mean(d[rid, s]))), 3);
              sd <- round(sapply(rid, function(rid) sapply(grp, function(s) sd(d[rid, s]))), 3);
              y  <- list(array=sd, type='data', color='#000000', thickness=1, 
                         width = min(5, ceiling(100/length(rid))), opacity = 0.9)
              
              plot_ly(x=gn, y=m, error_y=y, type='bar', showlegend=FALSE, marker=list(line=ln)) %>% 
                layout(p, margin = list(b = mgb), bargap=max(0.2, 0.8/length(rid)), yaxis = list(title = ylab), 
                       xaxis = list(autotick = FALSE, tickangle = -45));
            } else {
              ns <- sapply(grp, length); 
              
              m  <- round(sapply(rid, function(rid) sapply(grp, function(s) mean(d[rid, s]))), 3);
              sd <- round(sapply(rid, function(rid) sapply(grp, function(s) sd(d[rid, s]))), 3);
              mx <- max(m+sd); 
              
              y <- apply(sd, 2, function(s) { 
                s[is.na(s)] <- 0; 
                list(array = s, type = "data", color = "#000000", thickness = 1, 
                     width = min(5, ceiling(100/length(grp))), opacity = 0.9)
              }); 
              
              setProgress(value = 60); 
              
              p <- plot_ly(x=rownames(m), y=m[, 1], error_y=y[[1]], type='bar', showlegend=TRUE, name=gn[1], 
                           text=gn[1], marker=list(color=col0[1], line=ln)); 
              
              if (ncol(m) > 1) {
                for (i in 2:ncol(m)) {
                  p <- add_trace(p, x=rownames(m), y=m[, i], error_y=y[[i]], showlegend=TRUE, text=gn[i], name=gn[i], 
                                 marker=list(color=col0[i], line=ln));
                }
              }
              
              setProgress(value = 80); 
              
              p <- layout(p, barmode = "group", bargap=bgp, margin = list(b = mgb), yaxis = list(title = ylab), 
                          xaxis = list(autotick = FALSE, tickangle = -45),
                          annotations = list(x = rownames(m), y = mx, text = paste('N', ns, sep='='), 
                                             xanchor = 'center', yanchor = 'bottom', showarrow = FALSE));
              p; 
            }
          }
        }
      }
    }
  }
}

geex.select.gene<-function(cll, ds.id, grp.id, gn.id) { 
  out<-list();
  out$message<-'';
  
  ds.id<-ds.id[1];
  
  if (identical(NULL, cll)) {
    out$message<-msg.nocollection;
    out$table<-NULL;
  } else {
    if (identical(NA, ds.id) | ds.id=='') { 
      out$message<-msg.nodataset;
      out$table<-NULL;
    } else {
      ds<-geex.load.dataset(cll, ds.id);
      grp.id<-grp.id[grp.id %in% names(ds$group)];
      if (length(grp.id) == 0) {
        out$message<-msg.nogroup;
        out$table<-NULL;
      } else {
        gn.id<-CleanHtmlTags(gn.id); 
        gn.id<-rownames(ds$data$logged)[as.integer(gn.id)];
        if (length(gn.id)==0) {
          out$message<-msg.nogene;
          out$table<-NULL;
        } else {
          d<-ds$data$logged[gn.id, , drop=FALSE];
          grp2smp<-lapply(ds$group, function(s) s[s %in% colnames(d)]);
          smp.id<-unlist(grp2smp, use.names=FALSE);
          grp<-rep(names(grp2smp), sapply(grp2smp, length));
          d<-d[, smp.id, drop=FALSE];
          grp.id<-grp.id[grp.id %in% grp];
          if (length(grp.id) == 0) {
            out$message<-msg.nogroup;
            out$table<-NULL;
          } else {
            grp2smp<-grp2smp[grp.id];
            d<-d[, unlist(grp2smp, use.names=FALSE), drop=FALSE];
            if (length(grp2smp)>1) {
              ms<-sapply(grp2smp, function(s) rowMeans(d[, s, drop=FALSE], na.rm=TRUE)); 
              if (class(ms) == 'numeric') ms<-matrix(ms, nr=1); 
              rownames(ms)<-rownames(d);
              colnames(ms)<-paste('Mean', cll$mapping$longname2id[names(grp2smp)], sep='_'); 
              f<-as.factor(rep(names(grp2smp), sapply(grp2smp, length)));
              p<-apply(d, 1, function(d) {
                p<-summary(aov(d~f))[[1]][1, 5];
                if (is.null(p)) NA else p;
              })
              stat<-data.frame(round(ms, 4), p_ANOVA=as.numeric(format.pval(p, 2)), FDR=round(p.adjust(p, method='BH'), 3), stringsAsFactors=FALSE);
            } else {
              m<-rowMeans(d, na.rm=TRUE);
              p<-rep(NA, nrow(d));
              stat<-data.frame(round(m), as.numeric(format.pval(p, 2)), stringsAsFactors = FALSE)
            }
            stat<-data.frame(ds$anno[rownames(stat), 1:3, drop=FALSE], stat, stringsAsFactors=FALSE);
            ds<-cll$gex_combined$logged[rownames(d), rownames(cll$metadata$Dataset), drop=FALSE];
            ds<-colnames(ds)[!is.na(colSums(ds))];
            ds<-as.vector(cll$mapping$id2longname[ds]);
            out$message<-geex.html.gene.name(gn.id, FALSE, cll);
            out$data<-d;
            out$table<-stat;
            out$dataset<-ds;
            
            if (length(grp.id) == 1) {
              tbl <- cbind(out$table[, 1:3], round(rowMeans(out$data), 4), round(out$data, 4)); 
              colnames(tbl)[4] <- 'Mean';
              out$table <- tbl;
            }
          }
        }
      }
    }
  }
  
  out;
}

# prepare the gene list table for barplot
geex.plot.bar.table<-function(cll, ds) {
  if (identical(NA, cll)) NULL else 
    if (identical(NA, ds)) NULL else ds$anno;  
}

geex.plot.bar.table2<-function(ds, rid) {
  t<-ds$anno;
  t<-t[rownames(t) %in% CleanHtmlTags(rid), , drop=FALSE];
  if (nrow(t) == 0) NULL else t;
}