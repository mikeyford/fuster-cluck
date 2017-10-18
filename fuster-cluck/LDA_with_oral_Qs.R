library(topicmodels)
library(tidytext)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tm)
library(plyr)

qText <- read.csv("short_questions.csv", header = FALSE)

corpus <- Corpus(DataframeSource(qText))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, c(stopwords('english'),"ask","state","secretary","department","parliamentary","six", "government","question","week","years", "year", "statement","last","affairs","departments","will","make","can","plans","made", "assessment","many","steps","whether"))

qDocumentMatrix <- DocumentTermMatrix(corpus)

qLDA <- LDA(qDocumentMatrix, k = 2, control = list(seed = 41))

qTopics <- tidy(qLDA, matrix = "beta")

topTerms <- qTopics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

#these plots give the top terms in each topic, still lots of generic words dominating.
topTerms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

betaSpread <- qTopics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

#the spread of log_ratio gives the relative likelyhood of a word being in either topic (intended for k = 2)
#words classified at the extreme of each topic are given by these arrange functions, we can see the beginnings of a home/health topic and an international topic.
arrange(betaSpread, log_ratio)
arrange(betaSpread,desc(log_ratio))