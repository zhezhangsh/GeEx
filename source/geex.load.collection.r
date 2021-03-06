# load a data set, from the pre-processed data matrix
geex.load.dataset<-function(cll, ds.longname) {
  if (identical(cll, NULL) | ds.longname=='') NA else {
    smp<-cll$metadata$Sample;
    ds<-cll$metadata$Dataset;
    ds.id<-cll$mapping$longname2id[ds.longname];
    ds.id<-as.vector(ds.id[ds.id %in% rownames(ds)][1]);

    if (length(ds.id)==0 | ds.id[1]=='' | identical(NA, ds.id)) NA else {
      s<-smp[smp$Dataset==ds.id, ];
      e<-lapply(cll$gex_combined, function(g) g[, rownames(s), drop=FALSE]);
      e<-lapply(e, function(e) e[!is.na(rowMeans(e)), , drop=FALSE]);
      anno<-cll$gene[rownames(e[[1]]), ,drop=FALSE];
      id<-rownames(anno);
      anno<-data.frame(ID=AddHref(id, UrlEntrezGene(id)), Name=anno$Symbol, Species=anno$Species, N_Set=cll$gene[id, 'Num_Dataset'], row.names=rownames(anno), stringsAsFactors=FALSE);
      rownames(anno)<-id;
      
      group<-split(rownames(s), s$Group);
      names(group)<-cll$mapping$id2longname[names(group)];
      
      list(longname=ds.longname, anno=anno, sample=s, group=group, data=e);
    }
  } 
}

# Load data collection
geex.load.collection<-function(coll.name, GEX_HOME, input=NULL, output=NULL, session=NULL) { 
  coll.meta <- readRDS(paste(GEX_HOME, 'r', 'collection.rds', sep='/'));
  # Files to be loaded
  fn.load <- c('metadata', 'metadata_by_id', 'gene', 'mapping', 'browse_table', 'gex_combined');
  
  setProgress(value = 5);
  
  if (is.na(coll.name[1]) | coll.name[1]=='') NULL else {
    msg<-c(); # message to return
    
    id <- coll.name[1];
    nm <- coll.meta[id, 'Name'];

    paths <- c(paste(GEX_HOME, c('public', 'private'), tolower(nm), 'r', sep='/'));
    path  <- paths[file.exists(paths)];
    
    # Return data
    out<-list(
      selection=coll.name,
      id=id,
      name=nm,
      path=path
    );
    
    setProgress(10); 
    
    if (length(path) > 0) {
      path<-path[1];
      
      # Load in collection data and metadata
      loaded<-lapply(fn.load, function(fn) { 
        incProgress(); 
        fn <- paste(path, '/', fn, '.rds', sep='');
        if (!file.exists(fn)) NA else readRDS(fn);
      });
      names(loaded)<-fn.load;
      
      attr(attr(loaded$gex_combined$logged, 'physical'), 'filename') <- paste(path, 'gex_ff1', sep='/'); 
      attr(attr(loaded$gex_combined$percentile, 'physical'), 'filename') <- paste(path, 'gex_ff2', sep='/'); 
      
      # If there are required files not exist
      fn.unload<-fn.load[sapply(loaded, function(d) identical(NA, d))];
      if (length(fn.unload) > 0) msg<-c(msg, paste("File", fn.unload, "does not exist in collection"));
            
      out<-append(out, loaded);
      
      out$extra$longname$dataset<-paste(rownames(loaded$metadata$Dataset), loaded$metadata$Dataset$Name, sep=': ');
    } else {
      msg <- c(msg, paste("Data collection <", nm, "> does not exist in Rchive", sep=''));
    }
    
    if (length(msg) == 0) msg <- paste('Data collection "', coll.name, '" has been successfully loaded.', sep='');

    out$message<-msg;
    
    setProgress(value = 80);
    
    if (!is.null(input) & !is.null(output) & !is.null(session)) 
      session.data <- geex.update(input, output, session, list(loaded=out));
    
    setProgress(value = 95);
    
    out;
  }
}

