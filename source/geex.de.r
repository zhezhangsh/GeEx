geex.de.input <- function(cll, cmp) {
  require(DEGandMore);
  data("DeMethodMeta"); 
  
  dat <- lapply(1:nrow(cmp), function(i) {
    s0 <- cll$metadata$Sample;
    
    g1 <- cmp[i, 'Control']; 
    g2 <- cmp[i, 'Case'];
  
    n1 <- cll$mapping$longname2id[g1];
    n2 <- cll$mapping$longname2id[g2];
    
    s1 <- rownames(s0)[s0$Group==n1];
    s2 <- rownames(s0)[s0$Group==n2];
    
    an <- cll$gene;
    an <- an[an$Species==cmp[i, 'Species'], , drop=FALSE];
    
    gx <- cll$gex_combined[[1]]; 
    gx <- gx[rownames(an), c(s1, s2), drop=FALSE]; 
    gx <- gx[!is.na(rowMeans(gx)), , drop=FALSE];
    
    gp <- list(s1, s2); 
    names(gp) <- c(g1, g2); 
    
    md <- rownames(DeMethodMeta)[DeMethodMeta[, 1]==cmp[i, 'Method']];
    
    list(mtrx=gx, grps=gp, mthd=md); 
  }); 
  
  names(dat) <- cmp[[1]]; 
  
  dat;
}

geex.de.comp <- function(cll, g1, g2, mth, spe, session.data) {
  comp <- session.data$de$comparison;
 
  s  <- cll$metadata$Sample;
  s0 <- s[s$Group==cll$mapping$longname2id[g1], , drop=FALSE];
  s1 <- s[s$Group==cll$mapping$longname2id[g2], , drop=FALSE];
  
  gx <- cll$gex_combined$logged[, c(rownames(s0), rownames(s1)), drop=FALSE]; 
  an <- cll$gene;
  an <- an[an$Species==spe, , drop=FALSE]; 
  gx <- gx[rownames(an), ]; 
  gx <- gx[!is.na(rowMeans(gx)), , drop=FALSE];
  
  comp <- data.frame(rbind(session.data$de$comp, c('', g1, nrow(s0), g2, nrow(s1), mth, spe, nrow(gx))), stringsAsFactors = FALSE);
  comp <- comp[!duplicated(comp[, -1]), , drop=FALSE]; 
  
  comp[[1]] <- paste('Comparison', 1:nrow(comp), sep='_'); 
  colnames(comp) <- c('ID', 'Control', 'N0', 'Case', 'N1', 'Method', 'Species', 'Num_Gene'); 
  rownames(comp) <- 1:nrow(comp); 
  
  comp;
}

geex.de.get2species <- function(cll, xgrp, ygrp, sp0) { 
  
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
    sp <- c('human', setdiff(sp, 'human')); 
    if (!identical(sp0, NULL)) sp <- intersect(sp0, sp); 
    sp; 
  } 
}