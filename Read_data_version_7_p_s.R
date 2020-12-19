#Consider to work under working directory
rm(list=ls())  # clear workspace


library(data.table)
fileurl = 'https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip'
if (!file.exists('./Coursera-SwiftKey.zip')){
  download.file(fileurl,'./Coursera-SwiftKey.zip', mode = 'wb')
  unzip("Coursera-SwiftKey.zip", exdir = getwd())
}


library(tm)
library(NLP)
library(tidyr)
library(dplyr)
library(tidytext)
library(stringi)
library(stringr)
library(ggplot2)
library(RColorBrewer)

#Connect to data

twitter_us <- "en_US.twitter.txt"
blogs_us <- "en_US.blogs.txt"
news_us <- file("en_US.news.txt", "r")
#Read and store data.
leo_twitter_raw <-readLines(twitter_us,skipNul = TRUE) #Defalut encoding is UTF-8
leo_blog_raw <-readLines(blogs_us,skipNul = TRUE)
leo_news_raw <-readLines(news_us,skipNul = TRUE, warn = FALSE)

#Count lines in each text
nlines_twitter <- length(leo_twitter_raw)
nlines_blog <- length(leo_blog_raw)
nlines_news <- length(leo_news_raw)

#File size
size_blogs_file <- file.size("en_US.blogs.txt")/ 1024^2 #convert to MB
size_news_file <- file.size("en_US.news.txt")/ 1024^2
size_twitter_file <- file.size("en_US.twitter.txt")/ 1024^2

#Number of words in each file

leo_twitter_wordcount <- sum(stri_count_words(leo_twitter_raw))
leo_blog_wordcount <- sum(stri_count_words(leo_blog_raw))
leo_news_wordcount <- sum(stri_count_words(leo_news_raw))

#Max lenght in a line
max_char_twitter <- max(nchar(leo_twitter_raw))
max_char_blog <- max(nchar(leo_blog_raw))
max_char_news <- max(nchar(leo_news_raw))

#Quiz week 1

lovee <- grep("love", leo_twitter_raw)
hatee <- grep("hate", leo_twitter_raw)
preg5 <-grep("biostats", leo_twitter_raw)
preg6 <-grep("A computer once beat me at chess, but it was no match for me at kickboxing", leo_twitter)


#Summary

data.frame("Data" = c("twitter", "blogs", "news"),
           "File.size.Mb" = c(size_blogs_file,size_news_file, size_twitter_file),
           "Line.Count" = c(nlines_twitter,nlines_blog,nlines_news),
           "Word count" = c(leo_blog_wordcount, leo_news_wordcount, leo_twitter_wordcount),
           "Max char.line"=c(max_char_blog,max_char_news,max_char_twitter))


#Week 2 mejorando el anáisis usando tidytext

#plots
summary_plot <- lapply(list(leo_twitter_raw, leo_blog_raw, leo_news_raw), function(x) stri_count_words(x))
plot_hist_twitter <- qplot(summary_plot[[1]],geom = "histogram",binwidth = 1, main = "Twitter",
               xlab = "Words per line", ylab = "Frequency")

plot_hist_blog <- qplot(summary_plot[[2]],geom = "histogram", binwidth = 5, main = "Blog",
                           xlab = "Words per line", ylab = "Frequency")

plot_hist_news <- qplot(summary_plot[[3]],geom = "histogram",binwidth = 5, main = "News",
                        xlab = "Words per line", ylab = "Frequency")

#clean memory from the plots
rm(plot_hist_twitter, plot_hist_blog, plot_hist_news)

#Ahora lo que hago es limpiar varios caracteres que no me sirven
# Remove all non-english characters because they might cause difficulties in the future
leo_twitter_raw2 <- iconv(leo_twitter_raw, "latin1", "ASCII", sub="")
leo_blog_raw2 <- iconv(leo_blog_raw, "latin1", "ASCII", sub="")
leo_news_raw2 <- iconv(leo_news_raw, "latin1", "ASCII", sub="")

#Because of the file sizes and that text analysis is a time and memory process use, I take a sample of each text document
#Set seed for reproducibility
set.seed(54321)
leo_twitter_sample<- sample(leo_twitter_raw2,length(leo_twitter_raw)*0.07)
leo_blog_sample<- sample(leo_blog_raw2,length(leo_blog_raw2)*0.07)
leo_news_sample<- sample(leo_news_raw2,length(leo_news_raw2)*0.07)


#After continuing I remove data that will no longer be needed
rm(leo_twitter_raw2, leo_blog_raw2, leo_news_raw2,leo_twitter_raw, leo_blog_raw,leo_news_raw)
#Before unifying the three data_samples into one a cleaning of the unnecessary words
#is made

#Create a full data set with the three texts documents from above

# CORPUS

sample_data <- c(leo_twitter_sample, leo_blog_sample,leo_news_sample)
#Check that there are no NAs in the data
sum(is.na(sample_data))

writeLines(sample_data, "./final_data/sample15.txt")
sample_data_stored <- "./final_data/sample15.txt"
sample_data_stored<-readLines(sample_data_stored,skipNul = TRUE)

corpus <- VCorpus(VectorSource(sample_data_stored))

library(tm)
#Remove punctuation - replace punctuation marks with " "
corpus <- tm_map(corpus, removePunctuation) 
#Transform to lower case
corpus <- tm_map(corpus, content_transformer(tolower))
#Strip digits
corpus <- tm_map(corpus, removeNumbers)

#inspect output
writeLines(as.character(corpus[[30]]))

## Taking out URLs
remove_web_url <- function(x) gsub("http[[:alnum:]]*", "", x)
corpus <- tm_map(corpus, content_transformer(remove_web_url))

#Strip extra whitespace from a text document. Multiple whitespace characters are collapsed to a single blank.
corpus <- tm_map(corpus, stripWhitespace)

#After cleaning most of the unnecesary information, the next proceess is called
#tokenization
#First create a tidy data frame from the corpus vector. After this it is possible to continue with the next process, tokenization
tidy_corpus <-  corpus %>% tidy() #also select the neccesary function because the funtion tidy adds columns that have NA or are not neccesary for this project
tidy_corpus <- tidy_corpus %>% select(id,text)

#The function unnest_token from the tidytext package is used now to create token
#Furthermore, this function converts all characters to lower case
# and also punctuation is stripped
tidy_corpus <- tidy_corpus %>% unnest_tokens(word,text)

#data(stop_words)
#It could have been useful cleaning stopwords in the corpus,but I found that in a tidy set the funcion anti_join is
#faster than the function removeWords from the library tm
#tidy_corpus <- tidy_corpus %>% anti_join(stop_words,by= "word")
naughty_words <- tibble(word = readLines('https://raw.githubusercontent.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en'))
tidy_corpus <- tidy_corpus %>% anti_join(naughty_words)

suppressMessages(library(wordcloud))

tidy_corpus %>% count(word) %>%  with(wordcloud(word, n, max.words = 50,random.order = F, rot.per=0.35, colors=brewer.pal(8, "Dark2")))


#Next step after doing the exploratory analysis and cleaning the final data set from unnecesary words
# is to observe

#See the top words by frequency

#plot topwords or unigrams
tidy_corpus %>% count(word, sort = TRUE) %>% head(12) %>% mutate(word = reorder(word,n)) %>%
  ggplot(aes(word,n)) + geom_col() + xlab(NULL) + ylab("Word count") +  coord_flip() + theme_bw() +
  ggtitle("Most common words")

#Create N-grams
tidy_unigram <- tidy_corpus
tidy_bigram <- tidy_corpus %>% unnest_tokens(bigram, word, token = "ngrams", n = 2) %>% drop_na()
tidy_trigram <- tidy_corpus %>% unnest_tokens(trigram, word, token = "ngrams", n = 3) %>% drop_na()
tidy_quadgram <- tidy_corpus %>% unnest_tokens(quadgram, word, token = "ngrams", n = 4) %>% drop_na()

#Plot fof most commmon bi-grams
tidy_bigram %>% count(bigram, sort = TRUE) %>% head(12) %>% mutate(bigram = reorder(bigram,n)) %>%
   ggplot(aes(bigram,n)) + geom_col() + xlab(NULL)+ ylab("Frequency count") +  coord_flip() + theme_bw() + ggtitle("Most common bi-grams")

#Plot fof most commmon tri-grams
tidy_trigram %>% count(trigram, sort = TRUE) %>% head(12) %>% mutate(trigram = reorder(trigram,n)) %>%
  ggplot(aes(trigram,n)) + geom_col() + xlab(NULL) + ylab("Frequency count") +  coord_flip() + theme_bw() + ggtitle("Most common tri-grams")


participation_word<-function(x,word_coverage){ #x is the unigram output sorted by frequency, y is the percent word coverage
  word_count<-0 # initial counter
  participation <- word_coverage*sum(x$n) # number of words to hit coverage
  for (i in 1:nrow(x))
  {if (word_count >= participation) {
    return (i)
  }
    word_count<-word_count+x$n[i]
  }}

#Create column of n count and probability for each n-gram

tidy_unigram_prob <- tidy_unigram %>% count(word, sort = TRUE)
tidy_unigram_prob <- tidy_unigram_prob %>% mutate(prob = n/sum(n))

tidy_bigram_prob <- tidy_bigram %>% count(bigram, sort = TRUE)
tidy_bigram_prob <- tidy_bigram_prob %>% mutate(prob = n/sum(n))

# tidy_trigram_prob <- tidy_trigram %>% count(trigram, sort=TRUE)
# tidy_trigram_prob <- tidy_trigram_prob %>% count(trigram, sort=TRUE)
  
tidy_trigram_prob <- tidy_trigram %>% count(trigram, sort = TRUE)
tidy_trigram_prob <- tidy_trigram_prob %>% mutate(prob = n/sum(n))
                                                  
tidy_quadgram_prob <- tidy_quadgram %>% count(quadgram, sort = TRUE)
tidy_quadgram_prob <- tidy_quadgram_prob %>% mutate(prob = n/sum(n))

#Store n-grams and then read it

saveRDS(tidy_unigram_prob, "./final_data/tidy_unigram_07_p.RDS")
saveRDS(tidy_bigram_prob, "./final_data/tidy_bigram_07_p.RDS")
saveRDS(tidy_trigram_prob, "./final_data/tidy_trigram_07_p.RDS")
saveRDS(tidy_quadgram_prob, "./final_data/tidy_quadgram_07_p.RDS")


tidy_unigram_prob <- readRDS("./final_data/tidy_unigram_15_p.RDS")
tidy_bigram_prob <- readRDS("./final_data/tidy_bigram_15_p.RDS")
tidy_trigram_prob <- readRDS("./final_data/tidy_trigram_15_p.RDS")
tidy_quadgram_prob <- readRDS("./final_data/tidy_quadgram_15_p.RDS")


#Fuction to create a prefix with the first word of the bigram which will then be used to lookup and select the second word
prefix_separation_bigram = function(x){
  prefix = character(nrow(x))
  last_part = character(nrow(x))
  part <- strsplit(x$bigram, " ", fixed=TRUE)
  for(i in 1:nrow(x)){
    prefix[i] = part[[i]][1]
    last_part[i] = part[[i]][2]
  }
  x$prefix <- prefix
  x$last_part <- last_part
  return(x)
}

#Funcion para separar las primeras 2 palabras del trigrama, para que busque eso y luego traiga 
#el resto cuando haga el modelo
prefix_separation_trigram = function(x){
  prefix = character(nrow(x))
  last_part = character(nrow(x))
  part <- strsplit(x$trigram, " ", fixed=TRUE)
  for(i in 1:nrow(x)){
    prefix[i] = paste(part[[i]][1],part[[i]][2])
    last_part[i] = part[[i]][3]
  }
  x$prefix <- prefix
  x$last_part <- last_part
  return(x)
}

#Funcion para separar las primeras 3 palabras del cuatrigrama, para que busque eso y luego traiga 
#el resto cuando haga el modelo
prefix_separation_quadgram = function(x){
  prefix = character(nrow(x))
  last_part = character(nrow(x))
  part <- strsplit(x$quadgram, " ", fixed=TRUE)
  for(i in 1:nrow(x)){
    prefix[i] = paste(part[[i]][1],part[[i]][2], part[[i]][3])
    last_part[i] = part[[i]][4]
  }
  x$prefix <- prefix
  x$last_part <- last_part
  #list(prefix=prefix, rest=rest)
  return(x)
}

#Aca lo que hago es generar primero los prefijos que es donde buscaré el match
#y eso traerá comor resultado la columna "rest". Para todo esto tengo que 
#primero generar esas columnas en un tidy_set y luego pasarlo a un data.frame ya que 
# voy a usar la función setkeyv (package data.frame) para que luego pueda usarlo como "key"

tidy_bigram_prefix <- prefix_separation_bigram(tidy_bigram_prob)
tidy_trigram_prefix <- prefix_separation_trigram(tidy_trigram_prob)
tidy_quadgram_prefix <- prefix_separation_quadgram(tidy_quadgram_prob)

#Tengo que generar un data.table para que funcione la función setkeyv del paquete "data.table". Luego puedo
#hacer una busqueda y que me traiga solo la líneas de la columna "rest" que tengan un match en lo que estoy 
#buscando y la columna "prefix"
library("data.table")
tidy_bigram_p <- as.data.table(tidy_bigram_prefix)
setkeyv(tidy_bigram_p,"prefix")

tidy_trigram_p <- as.data.table(tidy_trigram_prefix)
setkeyv(tidy_trigram_p,"prefix")
prediccion <-tidy_trigram_p[prefix %in% "happy mothers", last_part]

tidy_quadgram_p <- as.data.table(tidy_quadgram_prefix)
setkeyv(tidy_quadgram_p,"prefix")

object.size(tidy_quadgram_p)/1048576 #me da el peso en mb


#Ahora que tengo los prefijos y el "resto" puedo quitar columnas de los objetos ya que me suman mucho espacio
#por ejemplo el quadgram pesa originalmente 400 mb pero al meterle el prefijo y el "resto" me termina dando el doble de peso
#lo que hago ahora es usar "select" del tidyverse para quitar la columna quadgram y la n, total la información que necesito
#ya esta en las otras columnas
library(dplyr)
tidy_bigram <- select(tidy_bigram_p, -c(bigram, n))
tidy_trigram <- select(tidy_trigram_p, -c(trigram, n))
tidy_quadgram <- select(tidy_quadgram_p, -c(quadgram, n))

rm(tidy_bigram_p, tidy_trigram_p, tidy_quadgram_p, tidy_bigram_prefix, tidy_trigram_prefix, tidy_quadgram_prefix,tidy_bigram_prob, tidy_trigram_prob, tidy_quadgram_prob)

saveRDS(tidy_unigram_prob, "./final_data/tidy_ungram_07_final.RDS")
saveRDS(tidy_bigram, "./final_data/tidy_bigram_07_final.RDS")
saveRDS(tidy_trigram, "./final_data/tidy_trigram_07_final.RDS")
saveRDS(tidy_quadgram, "./final_data/tidy_quadgram_07_final.RDS")

library(data.table)
tidy_unigram <- readRDS("tidy_unigram_25_p.RDS")
tidy_bigram <- as.data.table(readRDS("tidy_bigram_25_final.RDS"))
tidy_trigram <- as.data.table(readRDS("tidy_trigram_25_final.RDS"))
tidy_quadgram <- as.data.table(readRDS("tidy_quadgram_25_final.RDS"))

#Funcion para que me limpie la palabras que cargo
clean_intro_text <- function(t){
  tt <- tolower(t) 
  tt <- removePunctuation(tt)
  tt <- removeNumbers(tt)
  tt <- str_replace_all(tt, "[^[:alnum:]]", " ")
  tt <- stripWhitespace(tt)
  return(tt)
}
pattern <- "\\w+( \\w+){0,%d}$"
library(stringr)
entrada <- "happy mothers day"
count <- str_count(entrada, boundary("word"))


predicted_words <- function(entrada){
  clean_intro_text(entrada)
  count <- str_count(entrada, boundary("word"))
    pattern <- "\\w+( \\w+){0,%d}$"
    if(count == 0) return("You have entered 0 words")
    if (count > 0) {
      predicted <- tidy_quadgram[prefix %in% entrada, last_part]
      
      if(length(predicted)>0){
        
        if(length(predicted) == 1){
          randomWords <- sample(tidy_unigram$word, size = 2)
          predicted <- c(predicted, randomWords)
          return(predicted)
        }
        else if(length(predicted) == 2){
          randomWords <- sample(tidy_unigram$word, size = 1)
          predicted <- c(predicted, randomWords)
          return(predicted)
        }
        else(return(predicted[1:3]))} 
      
      if(length(predicted)==0){
        entrada_2 <- str_extract(entrada, sprintf(pattern, 1))
        predicted <- tidy_trigram[prefix %in% entrada_2, last_part]}
      
      if(length(predicted)>0){
          if(length(predicted) == 1){
            randomWords <- sample(tidy_unigram$word, size = 2)
            predicted <- c(predicted, randomWords)
            return(predicted)
          }
          else if(length(predicted) == 2){
            randomWords <- sample(tidy_unigram$word, size = 1)
            predicted <- c(predicted, randomWords)
            return(predicted)
          }
          else(return(predicted[1:3]))}
        
      if(length(predicted)==0){
        entrada_3 <- str_extract(entrada, sprintf(pattern, 0)) 
        predicted <- tidy_bigram[prefix %in% entrada_3, last_part]}
      
      if(length(predicted)>0){
        if(length(predicted) == 1){
          randomWords <- sample(tidy_unigram$word, size = 2)
          predicted <- c(predicted, randomWords)
          return(predicted)
        }
        else if(length(predicted) == 2){
          randomWords <- sample(tidy_unigram$word, size = 1)
          predicted <- c(predicted, randomWords)
          return(predicted)
        }
        else(return(predicted[1:3]))}
       
      
      #in case that predicted has always been 0, then, I select words from the unigrams.
      #For it, I select the most common unigram and then two random unigrams
      top_unigram <- tidy_unigram$word[1] #select the most frequent unigram
      randomWords <- sample(tidy_unigram$word, size = 2) #select 2 random unigrams
      predicted <- c(top_unigram, randomWords)
      return(predicted)
    }
  
}
  



Tomar de aca 
https://github.com/eileen98034/Capstone/blob/master/Final/ShinyApplication/predict.R
https://github.com/mark-blackmore/JHU-Data-Science-Capstone/blob/master/05_Task_Script.md 
https://github.com/MarcoLeti/DataScience-SwiftKeyCapstoneProject/blob/master/ShinyApp/functions/predictionModel.R 
