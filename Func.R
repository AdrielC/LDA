library(stringr)
library(text2vec)
library(tidytext)
library(rvest)
library(tidyverse)
library(lubridate)
library(stringr)
library(parallel)
library(pbapply)
library(spacyr)
library(reticulate)
library(topicmodels)

if(!("spacy_condaenv" %in% reticulate::conda_list()$name)){
  spacy_install()
}

spacy_initialize() # cant run on older versions of pip (< v10.0)

# Sample Search Results ----------------------------------------

omegaWatchesSearch <- "https://www.overstock.com/shop/Jewelry-Watches/Mens-Watches/omega-watch,/k,/31096/subcat.html?SearchType=Header&keywords=omega%20watch&searchtype=Header"
woolRugSearch <- "https://www.overstock.com/shop/Home-Garden/Area-Rugs/wool-rug,/k,/244/cat.html?SearchType=Header&keywords=wool%20rug&searchtype=Header"
searchResultURL <- "https://www.overstock.com/Home-Garden/Patchway-Wool-Handmade-Area-Rug-9-x-12/10633137/product.html?recset=e2534154-be81-4098-9ccf-59fbfa453222&refccid=J2CEPNFJKJF3RGCAHRRQC4UR3Y&searchidx=0&recalg=63&recidx=0"

# build_query(): Build the search URL to be scraped --------------------------------------------------

build_query_URL <- function(query)
{ 
  
  query <- as.character(query)
  query <- gsub('([[]])|\\s+', '+', query)
  searchResultURL <- paste0("https://www.overstock.com/search?keywords="
                            , query
                            , "&SearchType=Header"
                            , sep = "")
  
  return(searchResultURL)
  
} 



# getProductResults -------------------------------------------------------

getProductResults <- function(searchResultURL)
{
  
  sessionNode <- tryCatch(read_html(searchResultURL),
                          error = function(e) stop("Invalid URL"))
  
  searchResultSetURLs <- tryCatch(tibble(
    
    title = html_nodes(sessionNode, ".product-title") %>%
      html_text() %>%
      str_replace_all("[\t\n]" , "") %>% 
      unlist(),
    
    link = html_nodes(sessionNode, ".product-tile") %>%
      html_children() %>%
      html_attr("href") %>% 
      .[-which(is.na(.))]
  ), 
  error = function(e) return(NULL))
  
  return(searchResultSetURLs)
  
}



# getProductInfo(): retrieve all the data from a product URL --------------


getProductInfo <- function(searchResultURL)
{
  
  sessionNode <- tryCatch(xml2::read_html(searchResultURL),
                          error = function(e) return(NULL))
  
  
  Details <- tryCatch(html_nodes(sessionNode, ".col-xs-6:nth-child(1) .toggle") %>%
                        html_text() %>%
                        str_replace_all("[\n]" , ""),
                      
                      error = function(e){return(NULL)}
  )
  
  specs <- tryCatch(html_nodes(sessionNode, ".col-xs-6+ .col-xs-6 .toggle") %>%
                      html_children() %>% 
                      pluck(1) %>% 
                      html_children() %>% 
                      pluck(1) %>% 
                      html_table(),
                    
                    error = function(e){return(NULL)}
  )
  
  if(is.null(specs)){
    return(NULL)
  }
  
  specs1 <- tryCatch(html_nodes(sessionNode, ".col-xs-6+ .col-xs-6 .toggle") %>%
                       html_children() %>% 
                       pluck(1) %>% 
                       html_children() %>% 
                       pluck(3) %>% 
                       html_table(),
                     
                     error = function(e){return(list(NA))}
  )
  
  names(specs) <- c("X1", "X2")
  
  details_full <- tryCatch(bind_rows(specs, specs1),
                           
                           error = function(e){return(list(NA))}
  )
  
  reviews <- tryCatch(html_nodes(sessionNode, ".review-description") %>%
                        html_text(),
                      
                      error = function(e){return(list(NA))}
  )
  
  rm(sessionNode)
  
  return(mget(ls()))
  
}

# scraper_apply(): Scrape all text info from a search --------------------------------------

scraper_apply <- function(searchResultSetURLs, cl = 8)
{
  
  system.time(nested_info_list <- tryCatch(pblapply(searchResultSetURLs$link, function(x){
    
    result <- getProductInfo(x)
    
  }, cl = cl),
  
  error = function(e) warning("No results returned")))# This is where the number of forks (as integer) are passed into pbapply
  
  names(nested_info_list) <- searchResultSetURLs$title
  
  if(length(nested_info_list) > 1){
    return(nested_info_list) 
  }
  
}


# scrape_searches(): This will take a character vector of different ---------------------------------------------------------
# search queries and will scrape all the information from each ------------------------------------------------------------
# This will output a processed_list which can be used to score multiple search queries

create_processed_list <- function(list)
{
  
  stopifnot(class(list) == "list")
  
  processed_list <- structure(list, class = "processed_list")
  
  return(processed_list)
  
}


scrape_searches <- function(queries, cl = 8L)
{
  
  stopifnot(class(queries) == "character")
  
  # Create an empty list to place our scrape objects
  queryResults <- create_processed_list(list())
  
  numQueries <- length(queries)
  
  # create progress bar
  pb <- txtProgressBar(min = 0, max = numQueries, style = 3)
  for(i in 1:numQueries){
    
    output <- queries[i] %>% 
      build_query_URL() %>%
      getProductResults() %>% 
      scraper_apply(cl) %>% 
      process_desc_rev(cl)
    
    if(!is.na(output[[1]][1])){
      queryResults[[queries[i]]] <- output
    }
    
    Sys.sleep(0.1)
    # update progress bar
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  return(queryResults)
  
}


# join_desc_rev(): Join the description and review of an item into one document and POS tag ----------------

process_desc_rev <- function(nested_info_list, cl)
{
  # Remove all null values
  nested_info_list[sapply(nested_info_list, is.null)] <- NULL
  
  cleanList <- lapply(nested_info_list
                      , function(x)
                      {
                        
                        doc <- c(paste0(
                          trimws(
                            substring(text = sub(pattern = "ITEM#: ",
                                                 replacement = "",  
                                                 x = trimws(x$Details, which = "both")
                            ), 
                            first = 9
                            ), 
                            which = "left"
                          ),
                          
                          paste0(x$reviews,
                                 collapse = ""),
                          
                          sep = ". "))
                        
                        docPrep <- textPrep(doc)
                        
                        return(docPrep)
                      }
  )
  
  cleanDF <- bind_cols(cleanList) %>% 
    gather(key = doc_id,
           value = text)
  
  cleanDF <- tryCatch(spacy_parse(cleanDF, tag = TRUE, entity = FALSE, lemma = FALSE),
                      error = function(e) return(NA))
  
  return(cleanDF)
}

# textPrep ----------------------------------------------------------------

textPrep = function(x) {
  x %>% 
    # make text lower case
    str_to_lower %>% 
    # remove non-alphanumeric symbols
    str_replace_all("[^[:alpha:]]", " ") %>% 
    # collapse multiple spaces
    str_replace_all("\\s+", " ")
}



# create_dtm() --------------------------------------------------------------

create_dtm <- function(TIF)
{
  
  dtm <- TIF %>% mutate(word = token) %>% 
    anti_join(stop_words, by = "word") %>% 
    mutate(word = paste(token, tag, sep = " | ")) %>% 
    count(word, doc_id) %>% 
    cast_dtm(doc_id, word, n)
  
  return(dtm)
  
}



# run_LDA -----------------------------------------------------------------

run_LDA <- function(x) UseMethod("run_LDA", x)

run_LDA.spacyr_parsed <- function(data, k = 1, method = "Gibbs", seed = 42)
{
  
  lda_out <- data %>%
    LDA_text_prep() %>% 
    topicmodels::LDA(
      k = k + 1, 
      method = method,
      control = list(seed = seed)
    )
  
  return(lda_out)
  
}

run_LDA.processed_list <- function(list, cl = 8, ...)
{
  
  system.time(
    
    lda_out <- pblapply(list, function(x, ...){
      
      if("list" %in% class(x)){
        tryCatch(x <- x$train,
                 error = function(e) stop("list does not contain train object"))
        class(x) <- c("spacyr_parsed", "data.frame")
      }
      
      lda_out <- run_LDA(x, ...)
      
      return(lda_out)
      
    },
    
    cl = cl)
    
  )
  
  if("list" %in% class(list[[1]])){
    
    for (i in 1:length(list)) {
      list[[i]][["lda_mod"]] <- lda_out[[i]]
    }
    
    lda_out <- list
  }
  
  return(lda_out)
  
}

run_LDA.data.frame <- function(data, k = 1, method = "Gibbs", seed = 42)
{
  
  lda_out <- data %>%
    anti_join(stop_words, by = "word") %>% 
    count(word, doc_id) %>% 
    cast_dtm(doc_id, word, n) %>% 
    LDA(
      k = k + 1, 
      method = method,
      control = list(seed = seed)
    )
  
  return(lda_out)
  
}



# holdOut -----------------------------------------------------------------

hold_out <- function(traindata) UseMethod("hold_out", traindata)

hold_out.spacyr_parsed <- function(traindata)
{
  
  levels <- levels(as.factor(traindata$doc_id))
  sampleSize <- as.integer(length(levels) * 0.10)
  if(sampleSize == 0){
    sampleSize <- 1
  }
  holdoutLevels <- sample(levels, size = sampleSize) # Choose 10% of data to holdout, or one document
  holdout <- traindata %>%
    filter(doc_id %in% holdoutLevels)
  train <- traindata %>%
    filter(!(doc_id %in% holdoutLevels))
  
  return(list("holdout" = holdout, "train" = train))
  
}

hold_out.processed_list <- function(traindata, cl = 8)
{
  
  system.time(
    
    list_out <- pblapply(traindata, function(x){
      
      out <- hold_out(x)
      
      return(out)
      
    },
    
    cl = cl)
    
  )
  
  list_out <- create_processed_list(list_out)
  
  return(list_out)
  
}


# compute_perplexity() -----------------------------------------------------

compute_perplexity <- function(trainModel_list, test_df, cl = 8){
  
  system.time(
    
    perplexityScore <- pblapply(X = trainModel_list, function(X){
      
      perplexityScore <- topicmodels::perplexity(X[["lda_mod"]], newdata = LDA_text_prep(X[["holdout"]]))
      
      return(perplexityScore)
      
    },
    
    cl = cl)
    
  )
  
  for (i in 1:length(trainModel_list)) {
    trainModel_list[[i]][["perplexity"]] <- perplexityScore[[i]]
  }
  
  return(trainModel_list)
  
}


# lda_viz(): visualize all the lad_outputs --------------------------------



lda_viz <- function(processed_list)
{
  
  plots <- lapply(names(processed_list), function(x){
    
    lda_order <- processed_list[[x]][["lda_mod"]] %>% 
      tidy(matrix = "beta") %>% 
      group_by(topic) %>% 
      top_n(15, beta) %>% 
      ungroup() %>% 
      arrange(topic, beta) %>% 
      mutate(order = row_number())
    
    gg_out <- lda_order %>% 
      ggplot( aes(x = order, y = beta, fill = as.factor(topic))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ topic, scales = "free") +
      scale_x_continuous(
        breaks = lda_order$order,
        labels = lda_order$term
      ) + 
      coord_flip() +
      ggtitle(paste0("Topic spread for keyword search for ", x))
    
    return(gg_out)
    
  })
  
  return(plots)
  
}



# LDA_text_prep -----------------------------------------------------------

LDA_text_prep <- function(spacyr_parsed)
{
  
  out <- spacyr_parsed %>%
    mutate(word = token) %>% 
    anti_join(stop_words, by = "word") %>% 
    mutate(word = paste(token, tag, sep = " | ")) %>% # separate the token and tag by a "|"
    count(word, doc_id) %>% 
    cast_dtm(doc_id, word, n)
  
  return(out)
  
}
