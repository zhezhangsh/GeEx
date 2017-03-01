server_analysis <- function(input, output, session, session.data) {
  source('server_analysis_cluster.R', local=TRUE);
  source('server_analysis_coex.R', local=TRUE);
  source('server_analysis_de.R', local=TRUE);
  source('server_analysis_demeta.R', local=TRUE);
  
  session.data <- server_analysis_cluster(input, output, session, session.data);
  session.data <- server_analysis_coex(input, output, session, session.data);
  session.data <- server_analysis_de(input, output, session, session.data);
  session.data <- server_analysis_demeta(input, output, session, session.data);
  
  session.data;
}