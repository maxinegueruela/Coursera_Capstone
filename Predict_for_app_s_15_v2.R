#Consider to work on working directory
tidy_unigram <- readRDS("tidy_unigram_07_final.RDS")
tidy_bigram <- as.data.table(readRDS("tidy_bigram_07_final.RDS"))
tidy_trigram <- as.data.table(readRDS("tidy_trigram_07_final.RDS"))
tidy_quadgram <- as.data.table(readRDS("tidy_quadgram_07_final.RDS"))

#Function that cleans and make to lower the input from the user
clean_intro_text <- function(t){
  tt <- tolower(t) 
  tt <- removePunctuation(tt)
  tt <- removeNumbers(tt)
  tt <- str_replace_all(tt, "[^[:alnum:]]", " ")
  tt <- stripWhitespace(tt)
  return(tt)
}
pattern <- "\\w+( \\w+){0,%d}$"
entrada <- "happy mothers day"

  predicted_words <- function(entrada){
    clean_intro_text(entrada)
    count <- str_count(entrada, boundary("word"))
    if(count == 0) return("You have entered 0 words")
    pattern <- "\\w+( \\w+){0,%d}$"
    if (count > 0) {
      predicted <- tidy_quadgram[prefix %in% entrada, last_part]
      
      if(length(predicted)>0) return(predicted[1:3])
        
      if(length(predicted)==0){
        entrada_2 <- str_extract(entrada, sprintf(pattern, 1))
        predicted <- tidy_trigram[prefix %in% entrada_2, last_part]}
      
      if(length(predicted)>0) return(predicted[1:3])
      
      if(length(predicted)==0){
        entrada_3 <- str_extract(entrada, sprintf(pattern, 0)) 
        predicted <- tidy_bigram[prefix %in% entrada_3, last_part]}
      
      if(length(predicted)>0) return(predicted[1:3])
      
      if(length(predicted)==0){ 
        entrada_3 <- str_extract(entrada, sprintf(pattern, 0)) 
        predicted <- tidy_bigram[prefix %in% entrada_3, last_part]} 
      
      if(length(predicted)>0) return(predicted)
      #in case that predicted has always been 0, then, I select words from the unigrams.
      #For it, I select the two most common unigram and then one random unigram
      top_unigram <- tidy_unigram$word[1:2] #select the two most frequent unigrams
      randomWords <- sample(tidy_unigram$word, size = 1) #select 1 random unigram
      predicted <- c(top_unigram, randomWords)
      return(predicted)
    }
    
  }