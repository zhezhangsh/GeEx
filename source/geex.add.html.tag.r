# Add HTML tag and color to a message
geex.html.msg<-function(msg, color='#8888FF') {
 paste('<font color="', color, '">', msg, '</font>', sep='');
}

geex.html.gene.name<-function(rid, selected, cll) {
  if (identical(NULL, cll)) list(h3(msg.nocollection)) else if (selected & length(rid)==0)  list(h3("")) else {
    if (length(rid)==0) list(h3(msg.nogene)) else {
      rid<-CleanHtmlTags(rid);
      gn <- rownames(cll$gene)[as.integer(rid)[1]];
      if (length(gn) == 0) list(h3(msg.nogene)) else {
        gn<-cll$gene[gn, 'Symbol'];
        list(h4(HTML(paste("Gene -", geex.html.msg(paste(gn, collapse=' / '))))))
      }
    }     
  }
}

geex.html.gene.name2<-function(gn, cll) {
  if (identical(NA, cll)) list(h3(msg.nocollection), br(), br()) else {
    if (nrow(gn) == 0) list(h3(msg.nogene), br(), br()) else {
      gn<-gn[, 'Name'];
      list(h3(HTML(paste("Gene", geex.html.msg(paste(gn, collapse=' / '))))), br(), br())
    }
  } 
}

geex.select.gene<-function(rid, cll) {
  rid<-CleanHtmlTags(rid);
  gn<-rid[rid %in% rownames(cll$gene)];
  #gn<-gn[gn %in% rownames(ds$data$logged)];
  cll$anno[gn, ];
}