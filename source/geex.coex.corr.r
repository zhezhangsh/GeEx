geex.coex.corr <- function(cll, gid, dst) {
  require(meta); 

  setProgress(value = 0.1); 
                
  smp <- cll$metadata$Sample;
  gex <- cll$gex_combined$logged;
  ann <- cll$gene;
  
  dat <- lapply(dst, function(d) {
    i <- rownames(smp)[smp$Dataset==d];
    d <- gex[, i, drop=FALSE]; 
    d <- d[!is.na(rowMeans(d)), , drop=FALSE]; 
    s <- ann[gid, 'Species']; 
    a <- ann[ann$Species==ann[gid, 'Species'], , drop=FALSE]; 
    d[rownames(d) %in% rownames(a), , drop=FALSE]; 
  });
  
  crr <- lapply(dat, function(d) {
    d0 <- d[gid, ]; 
    d1 <- d[rownames(d)!=gid, , drop=FALSE];
    cor(d0, t(d1))[1, ]; 
  });
  
  ids <- Reduce(union, lapply(crr, names)); 
  ids <- rownames(ann)[rownames(ann) %in% ids];
  
  tbl <- matrix(NA, nr=length(ids), nc=length(dst), dimnames=list(ids, dst));
  for (i in 1:length(dst)) tbl[names(crr[[i]]), i] <- crr[[i]]; 
  
  n <- sapply(dat, ncol); 
  
  inc <- floor(nrow(tbl)/8); 
  met <- sapply(1:nrow(tbl), function(i) {
    if (i %% inc == 0) incProgress(0.1);
    x <- meta::metacor(tbl[i, ], n, sm='COR'); 
    c(x$k, x$TE.random, x$pval.random, x$TE.fixed, x$pval.fixed, x$Q, x$df.Q); 
  }); 
  met <- t(met); 
  
  q.p <- pchisq(met[, 6], met[, 7], lower.tail = TRUE, log.p = TRUE);
  q.p[met[,7]==0] <- 0;
  tbl <- cbind(met[, c(1:5)], rowMeans(tbl, na.rm=TRUE), exp(q.p), tbl); 
  tbl <- FormatNumeric(tbl, digit = 5); 
  colnames(tbl) <- c('N', 'R_Random', 'P_Random', 'R_Fixed', 'P_Fixed', 'R_Mean', 'P_Heter', paste('R', dst, sep='_')); 
  
  ann <- cll$browse_table$Gene[rownames(tbl), ]; 
  
  setProgress(value = 0.9); 
  
  cbind(ann[, c(1, 3)], tbl, ann[, c(6, 7)]); 
}