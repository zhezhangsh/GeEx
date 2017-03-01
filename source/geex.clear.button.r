geex.clear.button <- function(nm, eval=TRUE) {
  ln <- paste("actionButton('", nm, "', 'Clear selection', style='font-size:80%; height: 30px')", sep=''); 
  if (eval) eval(parse(text=ln)) else ln;
}