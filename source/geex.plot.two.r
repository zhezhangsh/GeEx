geex.plot.two<-function(cll, xgrp, ygrp, type, scale, color, species, gn, gs) { 
  
  # make an empty plot
  plotEmpty<-function(msg) plot(0, xaxt='n', yaxt='n', type='n', xlab='', ylab='', main=msg);   
  
  out<-list();
  
  gn<-CleanHtmlTags(gn);  
  scale<-tolower(scale);
  type<-tolower(type);
  
  if (identical(NA, cll)) {
    plotEmpty("No loaded data collection");
  } else if (identical(NA, xgrp[1]) | xgrp[1]=='') {
    plotEmpty("Group 1 not selected");
  } else if (identical(NA, ygrp[1]) | ygrp[1]=='') {
    plotEmpty("Group 2 not selected");
  } else {
    d<-geex.get2group(cll, xgrp, ygrp)[, 3:4, drop=FALSE];
    d<-d[!is.na(d[,1]) & !is.na(d[,2]), ,drop=FALSE];
    if (nrow(d) < 3) plotEmpty("Not enough data (N<3) to plot") else {
      if(scale=='unlogged') d<-exp(geex.get2group(cll, xgrp, ygrp)[, 3:4, drop=FALSE]*log(2)) else d<-geex.get2group(cll, xgrp, ygrp, scale)[, 3:4, drop=FALSE]
      
      dff<-d[,2]-d[,1];
      names(dff)<-rownames(d);
      
      if (length(gn) > 0) gn<-gn[gn %in% names(dff)];
      if (length(gs) > 0) gs<-lapply(gs, function(g) g[g %in% names(dff)]);
      
      ######################################################
      plotted<-PlotPairDiff(d, type, color, gs, gn); #######
      ######################################################
      
      ######################################################
      #Summarize gene sets
      if (length(gs) > 0) {     
        bg<-dff[!(names(dff) %in% unlist(gs, use.names=FALSE))];
        
        nm<-c(names(gs), 'All other genes');
        n<-c(sapply(gs, length), length(bg));
        m<-round(c(sapply(gs, function(g) if (length(g)>0) mean(dff[g], na.rm=TRUE) else NA), mean(bg)), 4);
        md<-round(c(sapply(gs, function(g) if (length(g)>0) median(dff[g], na.rm=TRUE) else NA), median(bg)), 4);
        pt<-sapply(gs, function(g) if (length(g)>1 & length(bg)>1) t.test(dff[g], bg)$p.value[[1]] else NA);
        pr<-sapply(gs, function(g) if (length(g)>1 & length(bg)>1) wilcox.test(dff[g], bg)$p.value[[1]] else NA);
        
        col<-plotted$highlight.color; 
        col<-col[names(col) %in% nm];
        if (length(col) > 0) {
          c<-rep('black', length(nm));
          names(c)<-nm;
          c[names(col)]<-col;
          nm<-paste('<font color="', c, '">', nm, '</font>', sep='');
        }
        
        out$geneset.stat<-data.frame(Name=nm, N=n, Mean=m, Median=md, p_T=c(pt, NA), p_RST=c(pr, NA), stringsAsFactors=FALSE);
        out$geneset.stat$p_T[!is.na(c(pt, NA))]<-as.numeric(format.pval(pt[!is.na(pt)], 2));
        out$geneset.stat$p_RST[!is.na(c(pr, NA))]<-as.numeric(format.pval(pr[!is.na(pr)], 2));
        if (length(pt) > 2) {
          out$geneset.stat$FDR_T=c(round(p.adjust(pt, method='BH'), 3), NA);
          out$geneset.stat$FDR_RST=c(round(p.adjust(pr, method='BH'), 3), NA);          
        }
      }
      ######################################################     
    }
  }
  out;
}

# decide species based on the two data groups to be compared. 'human' if both originally human or different spcies, or the other species if both the same non-human species
geex.get2group.species<-function(cll, xgrp, ygrp) { 

  if (identical(NA, cll) | xgrp=='' | ygrp=='') c() else {
    mp<-cll$mapping;
    meta<-cll$metadata;

    xid<-as.vector(mp$longname2id[xgrp]);
    yid<-as.vector(mp$longname2id[ygrp]);
    
    xds<-as.vector(meta$Group[xid, 'Dataset']);
    yds<-as.vector(meta$Group[yid, 'Dataset']);
    
    xsp<-tolower(as.vector(meta$Dataset[xds, 'Species']));
    ysp<-tolower(as.vector(meta$Dataset[yds, 'Species']));
    
    sp <- intersect(xsp, ysp); 
    if (length(sp) == 0) sp <- 'human';
    sp[1]; 
  } 
}

# Get 2 groups of data for plot
geex.get2group<-function(cll, xgrp, ygrp, scale='logged') { 
  if (identical(NULL, cll) | xgrp=='' | ygrp=='') {
    NULL;
  } else { 
    meta<-cll$metadata;
    mp<-cll$mapping;
    path<-cll$path;
    
    xid<-as.vector(mp$longname2id[xgrp]);
    yid<-as.vector(mp$longname2id[ygrp]);
    
    xds<-as.vector(meta$Group[xid, 'Dataset']);
    yds<-as.vector(meta$Group[yid, 'Dataset']);
    
    sc <- scale;
    if (scale == 'unlogged') sc <- 'logged'; 
    
    gex<-cll$gex_combined[[sc]][, c(xid, yid), drop=FALSE];
    gex<-gex[!is.na(gex[,1]) & !is.na(gex[,2]), , drop=FALSE];
    
    if (scale == 'unlogged') gex <- 2^gex; 
    
    gn<-cll$gene[, c('Species', 'Symbol'), drop=FALSE]; 
    gn<-gn[rownames(gex), , drop=FALSE];
    
    sp <- unique(gn[[1]]); 
    if (length(sp[sp!='human']) != 2) gn <- gn[gn[[1]]=='human', , drop=FALSE] else gn <- gn[gn[[1]]!='human', , drop=FALSE]
      
    gn.id<-rownames(gn);

    gex<-round(gex[rownames(gn), , drop=FALSE], 4);

    gex <- cbind(gex[, 1], gex[, 2], gex[, 2]-gex[, 1]);
    d<-data.frame(AddHref(gn.id, UrlEntrezGene(gn.id)), gn$Symbol, round(gex, 4), stringsAsFactors = FALSE);
    names(d)<-c('ID', 'Name', xid, yid, "Diff");
    
    d;
  }
}

geex.get2data <- function(input, output, session, session.data) {
  x.smp <- input$x.sample; 
  y.smp <- input$y.sample;
  x.grp <- input$x.group;
  y.grp <- input$y.group;
  
  if (x.smp=='Group mean' | x.smp=='') lab.x <- x.grp else lab.x <- x.smp;
  if (y.smp=='Group mean' | y.smp=='') lab.y <- y.grp else lab.y <- y.smp;
  
  session.data$two$gene <- geex.get2group(session.data$loaded, lab.x, lab.y, scale = input$two.scale);
  output$two.table.gene <- DT::renderDataTable({ session.data$two$gene
  }, options=dt.options2, rownames=FALSE, filter='bottom', server=TRUE, escape = FALSE);
}

#####################################################################################################################
### obsolete function
# make scatterplot of group means
geex.plot.scatter<-function(cll, xgrp, ygrp, highlight=NA) {
  
  if (identical(NA, cll) | xgrp=='' | ygrp=='') plotEmpty() else {
    d<-geex.get2group(cll, xgrp, ygrp);
    corr<-round(cor(d[,2], d[,3]), 4);    
    main<-paste('Corr = ', corr, ', N = ', nrow(d), sep='');
    par(mar=c(5,5,2,2));
    plot(d[, 2], d[, 3], pch=19, cex=1, main=main, cex.main=1.5, col='#88888888', xlab=xgrp, ylab=ygrp, cex.lab=1.5);
    
    # Plot highlight genes
    highlight<-highlight[highlight %in% rownames(d)];
    if (length(highlight) > 0) {
      points(d[highlight, 2], d[highlight, 3], pch=19, cex=1.25, col='#0000FF');
      points(d[highlight, 2], d[highlight, 3], cex=2, col='black');      
    }
  }
}
