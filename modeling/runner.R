# 1. step ---------------
# clear the workspace
# -----------------------
rm(list=ls())
gc()
# -----------------------

# 2. step ---------------
# load configuration
# -----------------------
model_conf <- yaml::read_yaml(file = 'model.yaml', fileEncoding = 'UTF-8')
# -----------------------

# 3. step ---------------
# create needed directories
# -----------------------
dir.create(path = model_conf$log_dir, recursive = TRUE)
dir.create(model_conf$output_dir, recursive = TRUE)
# -----------------------

# 4. step ---------------
# setup logging
# -----------------------
log_file = file.path(model_conf$log_dir,
                     paste0(format(Sys.time(), '%Y-%m-%d'), "_model.log"))
log_file_handle <- file(log_file, open = 'wt')
sink(file = log_file_handle, append = TRUE, type = 'message')
# -----------------------

message(sessionInfo())
## load needed functions
source(file = 'model-func.R')


# 5. step ---------------
# load data and create corpus of documents
# -----------------------
data <- load_data(file = model_conf$data)
corpus <- create_corpus_and_store(x = data,
                                  output_dir = model_conf$output_dir)
rm(data)
# -----------------------

# 6. step ---------------
# create document term matrix, vocabulary and train and test set
# -----------------------
dtm_vocab <- create_dtm_vocabulary_and_store(x = corpus,
                                             dir = model_conf$output_dir)
rm(corpus)
dtm <- dtm_vocab$dtm
vocabulary <- dtm_vocab$vocabulary
rm(dtm_vocab)
# -----------------------

# 7. step ---------------
# hyperparameter tunning and fold CV
# -----------------------
hyperparams <- hyperparams_tuning_train_and_store(dtm = dtm,
                                                  tuning = model_conf$tuning,
                                                  dir = model_conf$output_dir)
optimal_hyperparams <- get_optimal_hyperparams_and_store(
  hyperparams = hyperparams, dir = model_conf$output_dir
)
rm(hyperparams)
store_optimal_k_by_knee(hyperparams = optimal_hyperparams,
                        dir = model_conf$output_dir)
# -----------------------

# 8. step ---------------
# train again with optimal hyperparams
# -----------------------
train_multiple_models_and_store(dtm = dtm,
                                hyperparams = optimal_hyperparams,
                                dir = model_conf$output_dir)
rm(dtm)
rm(vocabulary)
rm(optimal_hyperparams)
# -----------------------

# 9. step ---------------
# prepare data for shiny
# -----------------------
prepare_shiny_data(output_dir = model_conf$output_dir,
                   data_path = model_conf$data)
# -----------------------

sink()

# -----------------------
# 10. close all connections
# -----------------------
closeAllConnections()
# -----------------------
