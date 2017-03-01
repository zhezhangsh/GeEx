geex.geneset.ui <- function(chk1, txt1, sel1, sel2, sel3, tbl1, btt1, hei=0) {
  if (chk1 == '') {
    ln4 <- 'div(style="display: inline-block; width: 70px", h6(HTML("<b>Source:</b>"))),'; 
    ln5 <- paste('div(style="display: inline-block; width: 70%; height: ', hei, 'px", selectizeInput("', sel1, '", NULL, names(geneset), selected=names(geneset)[1])), br(),', sep='');
    ln6 <- 'div(style="display: inline-block; width: 70px", h6(HTML("<b>Collection:</b>"))),';
    ln7 <- paste('div(style="display: inline-block; width: 70%; height: ', hei, 'px", selectizeInput("', sel2, '", NULL, names(geneset[[1]]), selected=names(geneset[[1]])[1])), br(),', sep=''); 
    ln8 <- 'div(style="display: inline-block; width: 70px", h6(HTML("<b>Species:</b>\"))),';
    ln9 <- paste('div(style="display: inline-block; width: 70%; height: ', hei, 'px", selectizeInput("', sel3, '", NULL, NULL)),', sep='');
    lnx <- 'h5(HTML(geex.html.msg("Highlight row to select a gene set"))),';
    ln0 <- paste('DT::dataTableOutput("', tbl1, '"),', sep=''); 
    lnn <- paste('actionButton("', btt1, '", "Clear selection", style="font-size:80%; height: 30px")', sep='');
    
    lns <- c(ln4, ln5, ln6, ln7, ln8, ln9, lnx, ln0, lnn);
    lns <- paste(lns, collapse=' '); 
    lns <- paste('fluidRow(', lns, ')', sep='');    
  } else {
    ln1 <- paste('checkboxInput("', chk1, '", HTML(geex.html.msg("', txt1, '")), value=FALSE),', sep='');
    ln2 <- 'conditionalPanel(';
    ln3 <- paste('condition = \'input["', chk1, '"] == true\',', sep=''); 
    ln4 <- 'div(style="display: inline-block; width: 70px", h6(HTML("<b>Source:</b>"))),'; 
    ln5 <- paste('div(style="display: inline-block; width: 70%; height: ', hei, 'px", selectizeInput("', sel1, '", NULL, names(geneset), selected=names(geneset)[1])), br(),', sep='');
    ln6 <- 'div(style="display: inline-block; width: 70px", h6(HTML("<b>Collection:</b>"))),';
    ln7 <- paste('div(style="display: inline-block; width: 70%; height: ', hei, 'px", selectizeInput("', sel2, '", NULL, names(geneset[[1]]), selected=names(geneset[[1]])[1])), br(),', sep=''); 
    ln8 <- 'div(style="display: inline-block; width: 70px", h6(HTML("<b>Species:</b>\"))),';
    ln9 <- paste('div(style="display: inline-block; width: 70%; height: ', hei, 'px", selectizeInput("', sel3, '", NULL, NULL)),', sep='');
    lnx <- 'h5(HTML(geex.html.msg("Highlight row to select a gene set"))),';
    ln0 <- paste('DT::dataTableOutput("', tbl1, '"),', sep=''); 
    lnn <- paste('actionButton("', btt1, '", "Clear selection", style="font-size:80%; height: 30px"))', sep='');
    
    lns <- c(ln1, ln2, ln3, ln4, ln5, ln6, ln7, ln8, ln9, lnx, ln0, lnn);
    lns <- paste(lns, collapse=' '); 
    lns <- paste('fluidRow(', lns, ')', sep='');    
  }
  # lns;
  eval(parse(text=lns));
};

