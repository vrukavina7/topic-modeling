# 1. step ---------------
# clear the workspace
# -----------------------
rm(list=ls())
gc()
# -----------------------

# 2. step ---------------
# load configuration
# -----------------------
data_scrap_conf <- yaml::read_yaml(file = 'scrap.yaml',
                                   fileEncoding = 'UTF-8')
# -----------------------

# 3. step ---------------
# Create needed directories
# -----------------------
dir.create(path = data_scrap_conf$log_dir, recursive = TRUE)
dir.create(path = data_scrap_conf$destination_dir,
           recursive = TRUE)
# -----------------------

# 4. step ---------------
# setup logging
# -----------------------
log_file = file.path(data_scrap_conf$log_dir,
                     paste0(format(Sys.time(), '%Y-%m-%d'), "_scraper.log"))
log_file_handle <- file(log_file, open = 'wt')
sink(file = log_file_handle, append = TRUE, type = 'message')
# -----------------------

message(sessionInfo())

# 5. step ---------------
# execute data scrap
# -----------------------
## load needed functions
source(file = 'scrap-func.R')
## get archive links from first level (year, month, category)
archive_links <- get_links(root_url = data_scrap_conf$root_url,
                           target_url = data_scrap_conf$archive_url,
                           selector = data_scrap_conf$archive_selector,
                           filters = data_scrap_conf$years_to_scrap)
## get article links from second level (year, month, category, article)
article_links = get_article_links(root_url = data_scrap_conf$root_url,
                                  links = archive_links,
                                  selector = data_scrap_conf$archive_selector,
                                  filters = data_scrap_conf$years_to_scrap)
## scrap data from article_links, create dataframe and store to csv
scrap_data_and_store(links = article_links,
                     selector = data_scrap_conf$article_selector,
                     dir = data_scrap_conf$destination_dir)
# -----------------------

sink()

# -----------------------
# 6. close all connections
# -----------------------
closeAllConnections()
# -----------------------
