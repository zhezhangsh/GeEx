server_visualization <- function(input, output, session, session.data) {
  
  source('server_visualization_pca.R', local=TRUE);
  source('server_visualization_bar.R', local=TRUE);
  source('server_visualization_geneset.R', local=TRUE);
  source('server_visualization_two.R', local=TRUE);
  
  session.data <- server_visualization_pca(input, output, session, session.data);
  session.data <- server_visualization_bar(input, output, session, session.data);
  session.data <- server_visualization_geneset(input, output, session, session.data);
  session.data <- server_visualization_two(input, output, session, session.data);
  
  session.data;
}