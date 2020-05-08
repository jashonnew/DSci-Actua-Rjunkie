pacman::p_load(wordcloud, wordcloud2, tidyverse, RColorBrewer, stringr, stringi, tm, tidytext, gofastr)


user_reviews <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/user_reviews.tsv') %>% 
  filter(grade < 3.1)
  

#docs <- stri_c(user_reviews$text, sep = " ")

#docs <- user_reviews$text %>%
  #tm_map(removeNumbers) %>%
  #tm_map(removePunctuation) %>%
  #tm_map(stripWhitespace)
#docs <- tm_map(docs, content_transformer(tolower))
#docs <- tm_map(docs, removeWords, stopwords("english"))

dat <- user_reviews %>% 
  mutate(text = gsub("https\\S*", "", text), 
         text = gsub("@\\S*", "", text), 
         text = gsub("amp", "", text), 
         text = gsub("[\r\n]", "", text),
         text = gsub("[[:punct:]]", "", text))


#dtm <- TermDocumentMatrix(dat$text) 
#matrix <- as.matrix(dtm) 
#words <- sort(rowSums(matrix),decreasing=TRUE) 
#df <- data.frame(word = names(words),freq=words)


dtm <-  dat %>%
  select(text) %>%
  unnest_tokens(word, text)
words <- dtm %>% count(word, sort=TRUE)
words_wo_stop <- words %>% anti_join(stop_words) %>% 
  filter(word != "game")

wordcloud(words = words_wo_stop$word, freq = words_wo_stop$n, min.freq = 1,           max.words=200, random.order=FALSE, rot.per=0.35,            colors=brewer.pal(8, "Set1"))
