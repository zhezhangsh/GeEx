geex.de.meta <- function(cll, cmp, res, md1, md2) {
  # Combine p value
  res <- res[cmp$ID]; 
  
  setProgress(value = 0.1); 
  
  ps <- lapply(res, function(s) s[, 5]); 
  id <- Reduce('union', lapply(ps, names)); 
  id <- id[order(as.numeric(id))]; 
  gn <- cll$browse_table$Gene[id, , drop=FALSE];
  gn <- gn[, c(1, 3, 4, 5, 2, 6:ncol(gn))]; 
  pv <- matrix(NA, nr=length(id), nc=length(ps), dimnames = list(id, cmp$ID)); 
  colnames(pv) <- paste('P', colnames(pv), sep='_');
  for (i in 1:length(ps)) pv[names(ps[[i]]), i] <- ps[[i]]; 
  
  ns <- apply(pv, 1,function(x) length(x[!is.na(x)])); 
  p0 <- CombinePvalue(pv, mthd=md1[1], normalize = FALSE, adjust.fisher = FALSE); 
  p1 <- CombinePvalue(pv, mthd=md1[1], normalize = TRUE,  adjust.fisher = FALSE); 
  q0 <- p.adjust(p0, method='BH'); 
  q1 <- p.adjust(p1, method='BH'); 
  
  t1 <- cbind(ns, p0, q0, p1, q1); 
  cnm <- CombinePvalueMethods();
  cnm <- names(cnm)[cnm==md1]; 
  colnames(t1) <- c('N_Comp', paste('P', cnm, sep='_'), paste('FDR', cnm, sep='_'), 
                    paste('P', cnm, 'NOM', sep='_'), paste('FDR', cnm, 'NOM', sep='_'));
  t1 <- cbind(gn[, 1:2], t1, pv, gn[, 3:ncol(gn)]);
  
  setProgress(value = 0.25); 
  
  # Mean difference
  dat <- geex.de.input(cll, cmp); 
  res <- suppressWarnings(CombineMean(dat, sm = md2)); 
  colnames(res)[1] <- 'N_Comp'; 
  t2 <- cbind(gn[, 1:2], res, gn[, 3:ncol(gn)]); 
  
  setProgress(value = 0.9); 
  
  list('p_value'=t1, 'mean_difference'=t2); 
}