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


#Week 2 mejorando el análisis usando tidytext


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

#Because of the file sizes and that text analysis is a time and memory proccess use, I take a sample of each text document
#Set seed for reproducibility
set.seed(54321)
leo_twitter_sample<- sample(leo_twitter_raw2,length(leo_twitter_raw)*0.1)
leo_blog_sample<- sample(leo_blog_raw2,length(leo_blog_raw2)*0.1)
leo_news_sample<- sample(leo_news_raw2,length(leo_news_raw2)*0.1)



#After continuing I remove data that will no longer be needed
rm(leo_twitter_raw2, leo_blog_raw2, leo_news_raw2,leo_twitter_raw, leo_blog_raw,leo_news_raw)
#Before unifying the three data_samples into one a cleaning of the unnecessary words
#is made

#Create a full data set with the three texts documents from above


# CORPUS

sample_data <- c(leo_twitter_sample, leo_blog_sample,leo_news_sample)
#Check that there are no NAs in the data
sum(is.na(sample_data))

writeLines(sample_data, "./final_data/sample.txt")
sample_data_stored <- "./final_data/sample.txt"
sample_data_stored<-readLines(sample_data_stored,skipNul = TRUE)

corpus <- VCorpus(VectorSource(sample_data))

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

#NO usarlo ya que tarda mucho. corpus <- tm_map(corpus, removeWords, stopwords("english"))
#Strip extra whitespace from a text document. Multiple whitespace characters are collapsed to a single blank.
corpus <- tm_map(corpus, stripWhitespace)

#After cleaning most of the unnecesary information, the next proceess is called
#tokenization
#First create a tidy data frame from the corpus vector. After this it is possible to continue with the next process, tokenization
tidy_corpus <-  corpus %>% tidy() %>% select(id,text) #also select the neccesary function because the funtion tidy adds columns that have NA or are not neccesary for this project


#The function unnest_token from the tidytext package is used now to create token
#Furthermore, this function converts all characters to lower case
# and also punctuation is stripped
tidy_corpus <- tidy_corpus %>% unnest_tokens(word,text)

data(stop_words)
#It could have been useful cleaning stopwords in the corpus,but I found that in a tidy set the funcion anti_join is
#faster than the function removeWords from the library tm
tidy_corpus <- tidy_corpus %>% anti_join(stop_words,by= "word")
naughty_words <- tibble(word = readLines('https://raw.githubusercontent.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en'))
tidy_corpus <- tidy_corpus %>% anti_join(naughty_words)

suppressMessages(library(wordcloud))

tidy_corpus %>% count(word) %>%  with(wordcloud(word, n, max.words = 50,random.order = F, rot.per=0.35, colors=brewer.pal(8, "Dark2")))


#Next step after doing the exploratory analysis and cleaning the final data set from unnecesary words
# is to observe

#See the top words by frequency
tidy_corpus %>% count(word, sort = TRUE) %>% head(12)
tidy_count_unigram <- tidy_corpus %>% count(word, sort = TRUE)
#plot topwords or unigrams
tidy_corpus %>% count(word, sort = TRUE) %>% head(12) %>% mutate(word = reorder(word,n)) %>%
  ggplot(aes(word,n)) + geom_col() + xlab(NULL) + ylab("Word count") +  coord_flip() + theme_bw() +
  ggtitle("Most common words")

#Create N-grams

tidy_bigram <- tidy_corpus %>% unnest_tokens(bigram, word, token = "ngrams", n = 2) %>% drop_na()
tidy_trigram <- tidy_corpus %>% unnest_tokens(trigram, word, token = "ngrams", n = 3) %>% drop_na()
tidy_quadgram <- tidy_corpus %>% unnest_tokens(quadgram, word, token = "ngrams", n = 4) %>% drop_na()

#This is a useful time to use tidyr�s separate(), which splits a column into multiple based on a delimiter. This lets us separate it into two columns, “word1” and “word2”, at which point we can remove cases where either is a stop-word

bigrams_separated <- tidy_bigram %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- tidy_bigram %>%
  separate(bigram, c("word1", "word2"), sep = " ")%>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#After cleaning possible stop words it is neccesary to reunite columns "word1" and "word2" into one columms
bigrams_united <- bigrams_filtered %>% unite(bigram, word1, word2, sep = " ")
bigrams_united

#PLot fof most commmon bi-grams
tidy_bigram %>% count(bigram, sort = TRUE) %>% head(12) %>% mutate(bigram = reorder(bigram,n)) %>%
   ggplot(aes(bigram,n)) + geom_col() + xlab(NULL)+ ylab("Frequency count") +  coord_flip() + theme_bw() + ggtitle("Most common bi-grams")

#PLot fof most commmon tri-grams
tidy_trigram %>% count(trigram, sort = TRUE) %>% head(12) %>% mutate(trigram = reorder(trigram,n)) %>%
  ggplot(aes(trigram,n)) + geom_col() + xlab(NULL) + ylab("Frequency count") +  coord_flip() + theme_bw() + ggtitle("Most common tri-grams")

#Visualizing a network of bigrams with ggraph
#We may be interested in visualizing all of the relationships among words simultaneously, rather than just the top few at a time. As one common visualization, we can arrange the words into a network, or “graph.”
libray(igraph)


#x is the unigram considered by frequency. Y would be the % that is needed to know from the word coverage from the total words in the data set
Count_word_dict<-function(x,count_dict) {
    word_sum<-0 # set count to 0
    porcentage_cover <- count_dict*sum(x$n)
for (i in 1:nrow(x)){
  if (word_sum >= porcentage_cover) {
    return (i)}
  word_sum<-word_sum+x$n[i]
}}



wordcoverage<-function(x,wordcover) #x is the unigram output sorted by frequency, y is the percent word coverage
{nwords<-0 # initial counter
coverage<-wordcover*sum(x$n) # number of words to hit coverage
for (i in 1:nrow(x))
{if (nwords >= coverage) {return (i)}
  nwords<-nwords+x$n[i]
}}
