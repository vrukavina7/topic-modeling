library(ggplot2)
library(ggthemes)
library(plotly)
library(LDAvis)
library(data.table)
library(pals)
library(dplyr)
library(tm)
library(textstem)
library(tidytext)
library(topicmodels)
library(jsonlite)
library(wordcloud2)
library(ECharts2Shiny)


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
#' @return VCorpus
#' @export
create_corpus <- function(x) {
  corpus <- VCorpus(x = DataframeSource(x = x[, c('doc_id', 'text')])) %>%
    clean_corpus
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


#' Apply computed model on new unseen text.
#'
#' @param lda_fit  Trained model
#' @param text  Inputed text by user
#' @param vocabulary  Used vocabulary in training process
#' @param json_vis  LDA json visualization, needed to order prediction
#' @param seed Use the same seed to stabilize prediction (LDA is generative)
#' @return data table which contains probabilities
#' @export
apply_model <- function(lda_fit, text, vocabulary, json_vis, seed) {
  if (text == "") {
    return(data.frame())
  }
  set.seed(seed = seed)
  corpus <- create_corpus(as.data.frame(list(doc_id = '1', text = text)))
  content <- strsplit(corpus[[1]]$content, "[[:space:]]")[[1]]
  mask <- c(t(sapply(content, function(i) {i %in% vocabulary})))
  content <- content[mask]
  if (length(content) == 0)
  {
    return(data.frame())
  }
  zero_mask <- c(t(sapply(vocabulary, function(i) {!i %in% content})))
  zero_vocab <- vocabulary[zero_mask]
  frequency_df <- as.data.frame(table(sapply(content, function(x) {x})))
  frequency_df['doc_id'] <- '1'
  zero_vocab_df <- data.frame(Var1 = zero_vocab, Freq = 0)
  zero_vocab_df['doc_id'] <- '1'
  dtm <- rbind(frequency_df, zero_vocab_df) %>% cast_dtm(doc_id, Var1, Freq,
                                                         weighting = weightTf)
  num_of_topic <- lda_fit@k
  num_of_topic_vec <- c(1:num_of_topic)
  json_vis_converted <- fromJSON(json_vis)
  topic_probs <- posterior(lda_fit, newdata = dtm)$topics
  topic_probs <- topic_probs[, json_vis_converted$topic.order]
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
  output <- data.table(probability = round(100*topic_probs), top_words)
  return(output)
}


#' Apply computed model on new unseen text.
#'
#' @param hyperparams  All hyperparams used in training process
#' @param optimalk  Optimal number of topics computed by first(derivative) and curvature
#' @return ggplot of perplexity and optimal number of topics
#' @export
perplexity_graph <- function(hyperparams, optimalk) {
  ggplotly(ggplot(data=hyperparams, aes(x = k, y = perplexity)) +
             geom_point() + geom_smooth() +
             labs(y="Perplexity", x="Number of topics") +
             ggtitle("Perplexity of hold out and training data") +
             geom_vline(xintercept = c(round(optimalk$first$x),
                                       round(optimalk$curvature$x)),
                        linetype="dotted") +
             geom_text(aes(x = round(optimalk$first$x),
                           label="\noptimal by first (derivation)",
                           y = optimalk$first$y), colour="green") +
             geom_text(aes(x = round(optimalk$curvature$x),
                           label="\noptimal by curvature",
                           y = optimalk$curvature$y), colour="red"))
}


#' Plot of distribution of the data per month.
#'
#' @param data  Data which contains topics and pubdate
#' @return ggplot of distribution of the data per month
#' @export
distribution_per_month_graph <- function(data) {
  data <- as.data.frame(data)
  data["year_month"] <- substr(data$pubdate, 0, 7)
  data <- data %>% group_by(doc_id) %>%
    filter(probability == max(probability))
  data <- data %>% group_by(year_month) %>% count(topic)
  ggplot(data, aes(x = year_month, y = n, fill = topic)) +
    geom_bar(stat = "identity") + ylab("num_of_documents") +
    scale_fill_manual(values = paste0(alphabet(20), "FF"),
                      name = "year_month") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("Number of documents per month colored by topics")
}


#' Plot of distribution proportion of the data per month.
#'
#' @param data  Data which contains topics and pubdate
#' @return ggplot of distribution proportion of the data per month
#' @export
distribution_per_month_proportion_graph <- function(data) {
  data <- as.data.frame(data)
  data["year_month"] <- substr(data$pubdate, 0, 7)
  data <- data %>% group_by(doc_id) %>%
    filter(probability == max(probability))
  data <- data %>% group_by(year_month) %>% count(topic)
  tmp <- data %>% group_by(year_month) %>% summarise(m = sum(n))
  data = merge(data, tmp, by = 'year_month')
  data['n'] <- data$n / data$m
  ggplot(data, aes(x = year_month, y = n, fill = topic)) +
    geom_bar(stat = "identity") + ylab("proportion") +
    scale_fill_manual(values = paste0(alphabet(20), "FF"),
                      name = "year_month") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("Proportion of topics per month colored by topics")
}


#' Plot wordcloud for each topic.
#'
#' @param model  trained LDA model
#' @param topic  number of topics
#' @param output  server output variable, needed for rendering wordcolud
#' @export
output_topic_wordcloud <- function(model, topic, output) {
  result = posterior(model)
  probabilities <- sort(result$terms[topic, ], decreasing=TRUE)
  probabilities <- probabilities[1:round(x = sqrt(x = length(probabilities)))]
  words <- names(probabilities)
  ECharts2Shiny::renderWordcloud(
    div_id = paste0("wordcloud", topic),
    data = data.frame(name = words, value = probabilities),
    shape = 'circle',
    grid_size = 5, sizeRange = c(15, 50),
    rotationRange = c(-45, 45),
    running_in_shiny = TRUE
  )
}


#' Plot top 10 word distribution by frequency.
#'
#' @param lda_fit  trained LDA model
#' @param json_vis  needed for ordering topic
#' @return  ggplot of top 10 words distribution by frequency
#' @export
top_word_distribution <- function(lda_fit, json_vis) {
  json_vis_converted <- fromJSON(json_vis)
  topics <- tidy(lda_fit, matrix = "beta")
  top_terms <- topics %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta) %>%
    mutate(topic = sapply(topic, function(x) {
      json_vis_converted$topic.order[[x]]
    })) %>%
    mutate(paste0("Topic - ", topic))
  top_terms %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~topic, scales = "free", labeller = label_both) +
    coord_flip() +
    scale_x_reordered() +
    theme(legend.position = "none")
}
