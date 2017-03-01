.libPaths("/home/zhangz/R/x86_64-pc-linux-gnu-library/3.3");

# Pre-load data when this App is started
AWSOMICS_HOME <- "/srv/shiny-server";
APP_HOME <- "/srv/shiny-server/geex";
RCHIVE_HOME <- "/zhangz/rchive";

# subfolders
GEX_HOME <- paste(RCHIVE_HOME, '/data/gex', sep=''); # Gene expression data collections
GENE_HOME <- paste(RCHIVE_HOME, '/data/gene/public/entrez/r', sep=''); # Location of gene annotation data
GENESET_HOME <- paste(RCHIVE_HOME, '/data/gene.set/r', sep=''); # Location of gene annotation data

require(XML);
require(readxl);
require(shinythemes);
require(plotly); 
require(ff); 

require(RoCA);
require(rchive);
require(awsomics);
require(DEGandMore);

# load source code
fn <- paste(APP_HOME, 'source', dir(paste(APP_HOME, 'source', sep='/')), sep='/');
fnc <- sapply(fn, function(fn) if (gregexpr('\\.R$', fn, ignore.case=TRUE)>0) source(fn));

data("DeMethodMeta"); 

##########################################################################################################
# load collection metadata
coll <- readRDS(paste(GEX_HOME, 'r/collection.rds', sep='/'));
# select.collection<-paste(rownames(coll), coll$Name, sep=': ');

##########################################################################################################
# load gene set metadata
geneset <- readRDS(paste(GENESET_HOME, 'metadata_as_tree.rds', sep='/'));

##########################################################################################################
# Column names of expression data matrix
expr_columns      <- c("Group mean", "Single sample");
expr_anno_columns <- c('Name', 'Species', 'Gene_Type', 'Num_Dataset', 'Num_Sample', 'Synonyms', 'Description');

##########################################################################################################
# options
data.level.options <- c('Data set', 'Group', 'Sample');

##########################################################################################################
# DE analysis options
de.method <- DeMethodMeta[c('DeT', 'DeWelch', 'DeSam', 'DeLimma', 'DeBGmix', 'DePlgem', 'DeWilcoxon', 'DeRankP'),]
de.plot.type <- list('Pvalue'=1, 'FDR'=2, 'Volcano'=3, 'M-A'=4, 'P-A'=5);

##########################################################################################################
# Message text
msg.nocollection <- 'No data collection loaded';
msg.nodataset    <- 'No data set selected';
msg.nogroup      <- 'No group selected';
msg.nogene       <- 'No gene selected';
msg.nogeneset    <- 'No gene set selected';

##########################################################################################################
# datatable options
dt.options  <- list(
  autoWidth = FALSE, caseInsensitve = TRUE, regex = TRUE, sScrollX = TRUE, 
  initComplete = DT::JS("function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': '#666', 'color': '#fff'});", 
                        "}"));

dt.options0 <- c(dt.options, pageLength = 100, dom = 't'); 
dt.options1 <- c(dt.options, pageLength = 50); 
dt.options2 <- c(dt.options, pageLength = 5, pagingType = 'simple', dom = 'ftipr'); 
dt.options3 <- c(dt.options, pageLength = 4, pagingType = 'simple', dom = 'ftipr'); 
dt.options4 <- c(dt.options, pageLength = 6, pagingType = 'simple', dom = 'ftipr'); 
dt.options5 <- c(dt.options, pageLength = 100); 
dt.options6 <- c(dt.options, pageLength = 10, dom = 'ftipr'); 
dt.options7 <- c(dt.options, pageLength = 1, dom='t'); 
dt.options8 <- c(dt.options, pageLength = 8, pagingType = 'simple', dom = 'ftipr'); 
