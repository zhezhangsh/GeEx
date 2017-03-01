server_data <- function(input, output, session, session.data) { 
  source('server_data_meta.R', local=TRUE);
  source('server_data_expr.R', local=TRUE);
  source('server_data_gene.R', local=TRUE);
  source('server_data_comb.R', local=TRUE);
  
  session.data <- server_data_meta(input, output, session, session.data);
  session.data <- server_data_expr(input, output, session, session.data);
  session.data <- server_data_gene(input, output, session, session.data);
  session.data <- server_data_comb(input, output, session, session.data);
  
  session.data;
}