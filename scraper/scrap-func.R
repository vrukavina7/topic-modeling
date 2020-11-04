library(rvest)


#' Get list of links from target url.
#'
#' Scrap selected and filtered links.
#' @param root_url  The root url of the web page
#'                  (used for concatenation with relative paths)
#' @param target_url  Destination from which we want to scrap content
#' @param selector  Define nodes and attributes for selection
#'                  (select only part of the web page)
#' @param filters  Define filter for selection based on link string
#' @return A vector of scraped links.
#' @export
get_links <- function(root_url, target_url, selector, filters) {
  load_web <- read_html(x = target_url, encoding = 'utf-8')
  links <- load_web %>%
    html_nodes(selector$class) %>%
    html_nodes(selector$node) %>%
    html_attr(selector$attr)
  links <- links[lapply(links, length) > 0]
  output <- links
  if (!is.null(x = filters)) {
    output = c()
    for (filter in filters) {
      tmp_links = links[grepl(pattern = filter, x = links)]
      tmp_links = tmp_links[lapply(X = tmp_links, FUN = length) > 0]
      if (length(x = tmp_links) > 0) {
        output <- append(x = output, values = tmp_links)
      }
    }
    output = output[lapply(X = output, FUN = length) > 0]
  }
  if (length(x = output) > 0) {
      output <- paste0(root_url, output)
  }
  return(unique(x = output))
}


#' Get article list of links from target url.
#'
#' Scrap selected and filtered links.
#' @param root_url  The root url of the web page
#'                  (used for concatenation with relative paths)
#' @param links  Destinations from which we want to scrap content
#' @param selector  Define nodes and attributes for selection
#'                  (select only part of the web page)
#' @param filters  Define filter for selection based on link string
#' @return A vector of scraped links.
#' @export
get_article_links <- function(root_url, links, selector, filters) {
  output <- c()
  for (link in links) {
    output <- append(x = output,
                     values = get_links(root_url = root_url,
                                        target_url = link,
                                        selector = selector,
                                        filters = filters))
  }
  return(unique(x = output[!sapply(X = output, FUN = is.null)]))
}


#' Scrap content and store data.
#'
#' Scrap content from links, create dataframe and save it to destination.
#' @param links  A list of links from which we want to scrap data.
#' @param selector  Define nodes and attributes for selection
#'                  (select only part of the web page)
#' @param dir  A destination where data will be stored (in .csv)
#' @export
scrap_data_and_store <- function(links, selector, dir) {
  csv_dest <- file.path(dir, 'articles.csv')
  rdata_dest <- file.path(dir, 'articles.RData')
  rdata <- data.frame(title = character(),
                           author = character(),
                           pubdate = character(),
                           content = character(),
                           link = character(),
                           stringsAsFactors=FALSE)
  if (file.exists(rdata_dest)) {
    rdata <- readRDS(rdata_dest)
    check_links <- rdata$link
    links <- setdiff(links, check_links)
    if (length(links) == 0) {
      message('Nothing new to scrap, ... returning')
      return()
    }
  }
  article_df <- data.frame(title = character(),
                           author = character(),
                           pubdate = character(),
                           content = character(),
                           link = character(),
                           stringsAsFactors=FALSE)
  for (link in links) {
    article_df <- rbind(article_df,
                        scrap_article_data(link = link, selector = selector))
    scrap_article_data(link = link, selector = selector)
  }
  if(length(rownames(article_df)) != 0) {
    article_df <- rbind(article_df, rdata)
    write.csv(x = article_df, file = csv_dest,
              row.names = FALSE, append = FALSE)
    saveRDS(article_df, file = rdata_dest)
  }
}


#' Scrap article data.
#'
#' Scrap article data like title, authot, pubdate and content.
#' @param link  Destination from which we want to scrap content
#' @param selector  Define nodes and attributes for selection
#'                  (select only part of the web page)
#' @return A dataframe of the scraped data.
#' @export
scrap_article_data <- function(link, selector) {
  loaded_web <- read_html(x = link, encoding = 'utf-8')
  title <- get_title_from_article(article = loaded_web,
                                  selector = selector$title)
  author <- get_author_from_article(article = loaded_web,
                                    selector = selector$author)
  pubdate <- get_pubdate_from_article(article = loaded_web,
                                      selector = selector$pubdate)
  content <- get_content_from_article(article = loaded_web,
                                      selector = selector$content)
  if (length(x = title) > 0 & length(x = author) > 0 &
      length(x = pubdate) > 0 & length(x = content) > 0) {
    return(data.frame(title, pubdate, author, content, link))
  }
  message(sprintf('Cannot scrap data from link: %s', link))
  return(data.frame(title = character(),
                    author = character(),
                    pubdate = character(),
                    content = character(),
                    link = character(),
                    stringsAsFactors=FALSE))
}


#' Extract title from article.
#'
#' @param article  An object derived from read_html function.
#' @param selector  Define nodes and attributes for selection
#'                  (select only part of the web page)
#' @return A title from a given article.
#' @export
get_title_from_article <- function(article, selector) {
  title <- article %>% html_nodes(selector$node) %>% html_text(trim = TRUE)
  return(title)
}


#' Extract author info from article.
#'
#' @param article  An object derived from read_html function.
#' @param selector  Define nodes and attributes for selection
#'                  (select only part of the web page)
#' @return An author info from a given article.
#' @export
get_author_from_article <- function(article, selector) {
  author <- article %>% html_nodes(selector$node) %>% html_text(trim = TRUE)
  return(author)
}


#' Extract publication date from article.
#'
#' @param article  An object derived from read_html function.
#' @param selector  Define nodes and attributes for selection
#'                  (select only part of the web page)
#' @return A publication date from a given article.
#' @export
get_pubdate_from_article <- function(article, selector) {
  pubdate <- article %>% html_nodes(selector$node) %>% html_attr(selector$attr)
  return(as.POSIXct(x = pubdate, format = "%Y-%m-%dT%H:%M:%S"))
}


#' Extract content from article.
#'
#' @param article  An object derived from read_html function.
#' @param selector  Define nodes and attributes for selection
#'                  (select only part of the web page)
#' @return A content from a given article.
#' @export
get_content_from_article <- function(article, selector) {
  content <- article %>% html_nodes(selector$node) %>% html_text(trim = TRUE)
  return(content)
}
