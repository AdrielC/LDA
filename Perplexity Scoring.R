source("Func.R")

#
# Procedure: Run LDA on all of the options while holding out two randomly selected documents for computing perplexity (metric for determining diversity).
#


# Scrape data and run LDA on random sample of search queries --------------









# Test --------------------------------------------------------------------

top_performing_keywords <- read_csv("top_performing_keywords.csv")
low_performing_keywords <- read_csv("low_performing_keywords.csv")
  
  


topKeywords <- scrape_searches(top_performing_keywords$`Keywrd Phrase Txt`, cl = 20) %>% 
  hold_out() %>% 
  run_LDA() %>% 
  compute_perplexity(cl = 20)

lowKeywords <- scrape_searches(low_performing_keywords$`Keywrd Phrase Txt`, cl = 20) %>% 
  hold_out() %>% 
  run_LDA() %>% 
  compute_perplexity(cl = 20)


low_sum <- lapply(X = lowKeywords, FUN = function(x){
  if(is.numeric(x$perplexity)){
    return(mean(x$perplexity))
  }})

mean(unlist(low_sum))

top_sum <- lapply(X = topKeywords, FUN = function(x){
  if(is.numeric(x$perplexity)){
    return(mean(x$perplexity))
  }})

mean(unlist(top_sum))

mean(unlist(top_sum))

low_rows <- lapply(X = lowKeywords, FUN = function(x){
  
  return(nrow(x$train))
  
  })

high_rows <- lapply(X = topKeywords, FUN = function(x){
  
  return(nrow(x$train))
  
  })

keyword_data <- low_rows %>% 
  bind_rows() %>%
  gather(key = term, value = numWords) %>% 
  mutate(type = "high bounce")

keyword_data1 <- high_rows %>% 
  bind_rows() %>%
  gather(key = term, value = numWords) %>% 
  mutate(type = "low bounce")

low_sum[[18]] <- NULL

low_df_perp <- low_sum %>% 
  bind_rows() %>%
  gather(key = term, value = perplexity)

top_df_perp <- top_sum %>% 
  bind_rows() %>%
  gather(key = term, value = perplexity)

perpData <- bind_rows(low_df_perp, top_df_perp)
  
full_data <- bind_rows(keyword_data, keyword_data1)

keyword_data2 <- bind_rows(keyword_data1, keyword_data)

keyword_data4 <- bind_rows(top_performing_keywords, low_performing_keywords) %>% 
  mutate(term = `Keywrd Phrase Txt`)

full_data1 <- full_data %>% 
  left_join(perpData, by = c("term")) %>% 
  filter(!is.na(perplexity)) %>% 
  left_join(keyword_data2) %>% 
  left_join(keyword_data4) %>% 
  mutate(scl_perp = scale(perplexity))

out_mod <- full_data1 %>% 
  lm(ReSearchPercent ~ numWords + SumSearches + type + scl_perp, data = .)


summary(out_mod)
  


full_data1 %>% 
  ggplot(aes(x = numWords, y = perplexity, col = type)) +
  geom_point() + 
  geom_smooth(method = "lm")


lm(bounce)







