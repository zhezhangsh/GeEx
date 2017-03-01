geex.gene.cluster <- function(cll, dat, prm) {
  setProgress(10); 
  
  gex <- geex.load.dataset(cll, dat);
  grp <- gex$group;
  gex <- gex$data$logged;
  
  mns <- sapply(grp, function(g) rowMeans(gex[, g, drop=FALSE], na.rm=TRUE)); 
  sd0 <- apply(gex, 1, function(x) sd(x, na.rm=TRUE)); 
  sd1 <- apply(mns, 1, function(x) sd(x, na.rm=TRUE)); 
  mn0 <- rowMeans(mns, na.rm=TRUE); 
  
  co1 <- sort(mn0)[ceiling(prm[1]/100*nrow(gex))];
  co2 <- sort(sd0)[ceiling(prm[2]/100*nrow(gex))];
  co3 <- sort(sd1)[ceiling(prm[3]/100*nrow(gex))];
  
  setProgress(20); 
  
  nct <- prm[4]; 
  gx1 <- t(scale(t(gex)));
  gx0 <- gx1[mn0>=co1 & sd0>=co2 & sd1>=co3, , drop=FALSE]; 
  crr <- cor(t(gx0)); 
  hcl <- hclust(as.dist(1-crr)); 
  ctr <- cutree(hcl, h=1-prm[5]); 
  cnt <- sapply(1:max(ctr), function(i) length(ctr[ctr==i]));
  ind <- which(cnt>=nct); 
  cls <- lapply(ind, function(i) rownames(gx0)[ctr==i]);
  
  setProgress(30); 
  
  mcl <- sapply(cls, function(x) colMeans(gx0[x, , drop=FALSE])); 
  hcl <- hclust(as.dist(1-cor(mcl)));
  ctr <- cutree(hcl, h=1-prm[5]); 
  cls <- lapply(split(cls, ctr), unlist);
  mcl <- sapply(cls, function(x) colMeans(gx0[x, , drop=FALSE])); 
  crr <- t(cor(mcl, t(gx0))); 
  rmx <- apply(crr, 1, max); 
  ind <- max.col(crr); 
  cls <- lapply(1:ncol(crr), function(i) rownames(crr)[ind==i & rmx>=prm[5]]);
  cls <- cls[sapply(cls, length)>=nct]; 
  
  setProgress(40); 
  
  if (prm[6] > 0) {
    rct <- prm[7]; 
    nrn <- prm[6]; 
    for (i in 1:nrn) {
      pro <- 45+round((i/nrn)*(90-40));
      if (pro%%5==0) setProgress(pro); 
      
      mcl <- sapply(cls, function(x) colMeans(gx0[x, , drop=FALSE])); 
      crr <- t(cor(mcl, t(gx0))); 
      rmx <- apply(crr, 1, max); 
      ind <- max.col(crr); 
      cls <- lapply(1:ncol(crr), function(i) rownames(crr)[ind==i & rmx>=rct]);
      cls <- cls[sapply(cls, length)>=nct]; 
    }
  }; 
  
  cls <- cls[rev(order(sapply(cls, length)))]; 
  names(cls) <- paste('Cluster', 1:length(cls), sep='_'); 
  mns <- t(sapply(cls, function(x) colMeans(gx0[x, , drop=FALSE]))); 
  smm <- sapply(grp, function(x) rowMeans(mns[, x, drop=FALSE])); 
  
  cnm <- rep('Cluster_0', nrow(gx0)); 
  names(cnm) <- rownames(gx0); 
  for (i in 1:length(cls)) cnm[cls[[i]]] <- names(cls)[i]; 
  
  setProgress(90); 
  
  list(summary=smm, cluster=cls, group=grp, mean=mns, data=data.frame(Cluster=cnm, gx0, stringsAsFactors = FALSE)); 
}