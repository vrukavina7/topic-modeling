library(tm)
library(textstem)
library(data.table)
library(parallel)
library(doSNOW)
library(tibble)
library(dplyr)
library(LDAvis)
library(topicmodels)
library(stringi)
library(slam)
library(tsne)
library(jsonlite)
library(tidytext)
library(KneeArrower)


#' Load data from CSV
#'
#' @param file  A path to the target RData file
#' @param output_dir  An output directory for storing data as RData
#' @return A target data frame as tibble object
#' @export
load_data <- function(file) {
  data <- readRDS(file = file) %>%
    as_tibble %>% rename(c(text = content, doc_id = link))
  return(data)
}



#' Remove some special characters from corpus.
#'
#' @param x  item on which we will perform special character removal
#' @return A cleaned item
#' @export
remove_special_char_udf <- content_transformer(function(x) {
  x<- gsub("\\s*\\B#\\w+(?:\\s*#\\w+)*\\s*$", " ", x)
  x <- gsub("[^\x01-\x7F]", " ", x)
  x <- lemmatize_strings(x = x)
  return (x)
})


#' Create corpus from data frame source.
#'
#' @param x  data frame source
#' @param output_dir  An output directory for storing corpus in RData
#' @return VCorpus
#' @export
create_corpus_and_store <- function(x, output_dir) {
  corpus <- VCorpus(x = DataframeSource(x = x[, c('doc_id', 'text')])) %>%
    clean_corpus
  saveRDS(corpus, file = file.path(output_dir, 'corpus.RData'))
  return(corpus)
}


#' Clean corpus.
#'
#' @param x  corpus to clean
#' @return VCorpus
#' @export
clean_corpus <- function(x){
  corpus <- tm_map(x = x, FUN = remove_special_char_udf)
  corpus <- tm_map(x = corpus, removePunctuation)
  corpus <- tm_map(x = corpus, removeNumbers)
  corpus <- tm_map(x = corpus, content_transformer(tolower))
  corpus <- tm_map(x = corpus, removeWords, stopwords())
  corpus <- tm_map(x = corpus, stemDocument)
  corpus <- tm_map(x = corpus, stripWhitespace)
  return(corpus)
}


#' Create document term matrix and vocabulary from corpus and store it as a
#'  RData.
#'
#' @param x  corpus
#' @param dir  An output directory for storing Document Term Matrix and
#'  vocabulary
#' @return list of DocumentTermMatrix and vocabulary
#' @export
create_dtm_vocabulary_and_store <- function(x, dir){
  dtm <- DocumentTermMatrix(x = corpus)
  freq = data.frame(sort(colSums(as.matrix(dtm)), decreasing=TRUE))
  vocab <- rownames(freq)[freq[1] > 1 & freq[1] < nrow(dtm) / 2]
  dtm <- dtm[, vocab]
  saveRDS(dtm, file = file.path(dir, 'dtm.RData'))
  saveRDS(vocab, file = file.path(dir, 'vocabulary.RData'))
  return(list(dtm = dtm, vocabulary = vocab))
}


#' Function which will perform hyperparameter tuning.
#'
#' @param dtm  Document term matrix
#' @tuning  hyperparams configuration
#' @dir  Output directory to store hyperparameters in RData and CSV
#' @return Return all hyperparams settings with perplexity metric
#' @export
hyperparams_tuning_train_and_store <- function(dtm, tuning, dir) {
  alpha <- runif(n = tuning$alpha$num,
                 min = tuning$alpha$min,
                 max = tuning$alpha$max)
  delta <- runif(n = tuning$delta$num,
                 min = tuning$delta$min,
                 max = tuning$delta$max)
  topics <- seq(from = tuning$topics$from,
                to = tuning$topics$to,
                by = tuning$topics$by)
  folds <- tuning$folds
  seed <- tuning$seed
  burnin <- c(tuning$burnin)
  iter <- c(tuning$iter)
  keep <- tuning$keep
  thin <- tuning$thin
  workers <- parallel::detectCores() - 1 # leave one core alone :)
  cluster <- parallel::makeCluster(workers)
  registerDoSNOW(cluster)
  clusterEvalQ(cluster, library(tibble))
  clusterEvalQ(cluster, library(topicmodels))
  clusterEvalQ(cluster, library(ldatuning))
  split_folds <- sample(x = 1:folds, size = dtm$nrow, replace = TRUE)
  hyperparams <- data.frame(k = topics,
                            alpha = rep(x = alpha, each = length(topics)),
                            delta = rep(x = delta, each = length(topics)),
                            burnin = burnin,
                            iter = iter,
                            thin = thin,
                            keep = keep,
                            seed = seed) %>% as_tibble
  setorder(hyperparams, k)
  clusterExport(cluster, c('dtm', 'split_folds', 'folds', 'hyperparams'),
                envir = environment())
  system.time({
    hyperparams_all <- foreach(j = 1:nrow(hyperparams),
                               .combine = rbind) %dopar%{
      topic <- hyperparams[j, 'k']
      control <- list(keep = hyperparams[j, 'keep'],
                      thin = hyperparams[j, 'thin'],
                      alpha = hyperparams[j, 'alpha'],
                      delta = hyperparams[j, 'delta'],
                      burnin = hyperparams[j, 'burnin'],
                      iter = hyperparams[j, 'iter'],
                      seed = hyperparams[j, 'seed'])
      fold_res <- tibble()
      for(i in 1:folds){
        train_set <- dtm[split_folds != i, ]
        test_set <- dtm[split_folds == i, ]
        fitted <- LDA(train_set, k = topic, method = "Gibbs",
                      control = control)
        result_of_one <-hyperparams[j,]
        result_of_one['perplexity'] <- perplexity(fitted, newdata = test_set)
        fold_res <- rbind(fold_res, result_of_one)
      }
      return(fold_res)
    }
  })
  stopCluster(cluster)
  saveRDS(hyperparams_all, file = file.path(dir, 'hyperparams.RData'))
  write.csv2(x = hyperparams_all, file = file.path(dir, 'hyperparams.csv'))
  return(hyperparams_all)
}


#' Get optimal hyperparmeters grouped by topic k and perplexity.
#'
#' @param hyperparams  all hyperparams data frame
#' @param dir output directory where optimal hyperparams will be stored
#'  in RData and CSV
#' @return  data frame which has unique topic k choosed by min perplexity
#'          per topic
#' @export
get_optimal_hyperparams_and_store <- function(hyperparams, dir) {
  optimal_hyperparams <- hyperparams %>%
    group_by(k) %>% slice(which.min(perplexity))
  write.csv2(x = optimal_hyperparams,
             file = file.path(dir, 'optimal_hyperparams.csv'))
  saveRDS(optimal_hyperparams,
          file = file.path(dir, 'optimal_hyperparams.RData'))
  return(optimal_hyperparams)
}


#' Store optimal number of topics using elbow technick.
#'
#' @param hyperparams  hyperparameter (contains perplexity info)
#' @param dir  directory in which we will store information
#' @export
store_optimal_k_by_knee <- function(hyperparams, dir) {
  optimalk_first <- findCutoff(x = hyperparams$k,
                               y = hyperparams$perplexity,
                               method = 'first')
  optimalk_curvature <- findCutoff(x = hyperparams$k,
                                   y = hyperparams$perplexity,
                                   method = 'curvature')
  optimalk <- list(first = optimalk_first, curvature = optimalk_curvature)
  saveRDS(optimalk, file = file.path(dir, 'optimalk.RData'))
}


#' Train multiple models with optimal hyperparams and store all needed data to
#'  directory.
#'
#' @param dtm  document term matrix
#' @param hyperparams  optimal hyperparams
#' @param dir  Target directory in which we will store all needed data
#' @param suffix  Add sufix to the name of the outputh.
#' @export
train_multiple_models_and_store <- function(dtm, hyperparams, dir,
                                            suffix = '') {
  hyperparams$perplexity <- NULL
  workers <- parallel::detectCores() - 1 # leave one core alone :)
  cluster <- parallel::makeCluster(workers)
  registerDoSNOW(cluster)
  clusterEvalQ(cluster, library(tibble))
  clusterEvalQ(cluster, library(topicmodels))
  clusterEvalQ(cluster, library(ldatuning))
  clusterExport(cluster, c('dtm', 'hyperparams'), envir = environment())
  system.time({
    models <- foreach(j = 1:nrow(hyperparams)) %dopar%{
      topic <- hyperparams[j, 'k']
      control <- list(keep = hyperparams[j, 'keep'],
                      thin = hyperparams[j, 'thin'],
                      alpha = hyperparams[j, 'alpha'],
                      delta = hyperparams[j, 'delta'],
                      burnin = hyperparams[j, 'burnin'],
                      iter = hyperparams[j, 'iter'],
                      seed = hyperparams[j, 'seed'])
      fitted <- LDA(dtm,
                    k = topic,
                    method = "Gibbs",
                    control = control)
      return(fitted)
    }
  })
  stopCluster(cluster)
  names(models) <- hyperparams$k
  models_path <- file.path(dir, 'lda_models.RData')
  if (suffix != '') {
    models_path <- file.path(dir, paste0('lda_models-', suffix, '.RData'))
  }
  saveRDS(models, file = models_path)
}


#' Prepare data for shiny and store all needed to output dir.
#'
#' @param output_dir  Output directory where data will be stored
#' @param data_path  Path where data are stored.
#' @export
prepare_shiny_data <- function(output_dir, data_path) {
  data <- load_data(file = data_path)
  lda_models <- readRDS(file = file.path(output_dir, 'lda_models.RData'))
  dtm <- readRDS(file = file.path(output_dir, 'dtm.RData'))
  output_data <- lapply(names(lda_models), function(x) {
    lda_fit <- lda_models[[x]]
    num_of_topic <- lda_fit@k
    num_of_topic_vec <- c(1:num_of_topic)
    print(paste('Process LDA model for number of topics: ', num_of_topic))
    json_vis <- create_lda_vis_josn(fit = lda_fit)
    json_vis_converted <- fromJSON(json_vis)
    topic_probs <- posterior(lda_fit, newdata = dtm)$topics
    topic_probs <- topic_probs[, json_vis_converted$topic.order]
    colnames(topic_probs) <- paste0("Topic - ", num_of_topic_vec)
    top_words <- tidy(lda_fit, matrix = "beta") %>%
      group_by(topic) %>%
      top_n(5, beta) %>%
      ungroup() %>%
      arrange(topic, -beta) %>%
      group_by(topic) %>%
      slice(seq_len(5)) %>% # consider ties
      group_by(topic) %>%
      summarize (top_words = paste(term, collapse = " ")) %>%
      mutate(topic = paste0("Topic - ", topic)) %>%
      as.data.table()
    top_words <- top_words[json_vis_converted$topic.order]
    top_words[, topic := paste0("Topic - ", num_of_topic_vec)]
    topic_names <- grep("Topic - ", colnames(topic_probs), value = TRUE)
    colnames(topic_probs) <- paste0(top_words$top_words," <- [",
                                    topic_names, "]")
    output <- data.table(round(100*topic_probs), data)
    output_molten <- melt(output,
                          id.vars = colnames(data),
                          measure.vars = grep(pattern = 'Topic',
                                              colnames(output),
                                              value = TRUE),
                          value.name = "probability",
                          variable.name = "topic")
    setorder(output_molten, -doc_id, -probability)
    list(json_vis = json_vis,
         output_molten = output_molten)
  })
  names(output_data) <- names(lda_models)
  json_viss <- lapply(names(output_data), function(x) {
    output_data[[x]]$json_vis
  })
  names(json_viss) <- names(output_data)
  models_results <- lapply(names(output_data), function(x) {
    output_data[[x]]$output_molten
  })
  names(models_results) <- names(output_data)
  saveRDS(json_viss, file = file.path(output_dir, 'json_viss.RData'))
  saveRDS(models_results, file = file.path(output_dir, 'models_results.RData'))
}


#' Produce JSON string for LDA visualizing
#'
#' @param fit  A fitted lda model
#' @return  JSON string which can be used in servis
#' @export
create_lda_vis_josn <- function(fit){
  post <- posterior(fit)
  mat <- fit@wordassignments
  svd_tsne <- function(x) tsne(svd(x)$u)
  json_vis <- createJSON(
    phi = post[["terms"]],
    theta = post[["topics"]],
    vocab = colnames(post[["terms"]]),
    doc.length = slam::row_sums(mat, na.rm = TRUE),
    term.frequency = slam::col_sums(mat, na.rm = TRUE),
    mds.method = svd_tsne,
    plot.opts = list(xlab= "", ylab= "")
  )
  return(json_vis)
}
