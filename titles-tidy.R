### Working with titles ###
library(tm)
library(dplyr)
library(tidytext)
library(tidyr)
corp_titles <- read.delim("Data/titles.txt", stringsAsFactors = F, header = TRUE, sep = "\t")

mystopwords <- tibble(word = c("http", "costello", "www.manythings.org"))

## Text pre-processing


token.txt <- corp_titles %>% 
  unnest_tokens(words, Title)

token.txt <- token.txt %>% 
  filter(!words %in% stop_words$word)

token.txt$word <- gsub("\\s+","",token.txt$word)
token.txt <- token.txt[-grep("\\b\\d+\\b", token.txt$word),]
token.txt <- anti_join(token.txt, mystopwords, 
                       by = "word")
# Stemming
library(SnowballC)
stem.txt<-token.txt %>%
  mutate_at("word", funs(wordStem((.), language="en")))

## Top words (stemmed)
top.word <- stem.txt %>% 
  count(word, sort = TRUE)
head(top.word)

library(ggplot2)

stem.txt %>%
  count(word, sort = TRUE) %>%
  filter(n > 200) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  theme_minimal() +
  xlab(NULL) +
  coord_flip()


## bigrams
title_bigrams <- corp_titles %>%
  unnest_tokens(bigram, Title, token = "ngrams", n = 2)


bigrams_separated <- title_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

## Correlating words

word_pairs <- token.txt %>%
  widyr::pairwise_count(word, Year, sort = TRUE)


word_cors <- token.txt %>%
  group_by(word) %>%
  filter(n() >= 10) %>%
  widyr::pairwise_cor(word, Year, sort = TRUE)


set.seed(2016)

## igraph, ggraph packages needed here
library(igraph)
library(ggraph)
word_cors %>%
  filter(correlation > .78) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()


# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

# Combine into one column
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united
bigram_counts <- bigrams_united %>% 
  count(bigram, sort = TRUE)

## tf-idf

bigram_tf_idf <- bigrams_united %>%
  count(Year, bigram) %>%
  bind_tf_idf(bigram, Year, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf


##Plot of frequencies, but not really useful
bigram_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>% 
  group_by(Year) %>% 
  filter(Year == 2003) %>% 
  top_n(8) %>% 
  ungroup() %>%
  ggplot(aes(bigram, tf_idf, fill = Year)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~Year, ncol = 2, scales = "free") +
  coord_flip()

## graphing bigrams - use this

library(igraph)
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
bigram_counts

bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

library(ggraph)
set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

stop_words <- tibble(word = c("language","1","2","3","4","5","6","7","8","9","19","activities","also","among","analysis","approach","based","c","can","class","compare","conducted","culture","data","demonstrate","describe","develop","discuss","especially","examined","examines","examples","explain","explore","findings","five","focus","found","four","give","given","high","however","ideas","introduce","investigates","issues","level","many","material","may","one","paper","participant","participants","poster","pre","present","presentation","provide","questionnaire","report","reported","reports","research","result","sample","science","session","several","share","short","show","six","sports","studies","study","survey","three","throughout","two","us","use","using","way","ways","workshop","year"))

bigrams_united %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         Year <= 2012
         ) %>%
  count(Year, word1, word2, sort = TRUE) %>%
  unite("bigram", c(word1, word2), sep = " ") %>%
  group_by(Year) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(Year = factor(Year) %>% forcats::fct_rev()) %>%
  ggplot(aes(drlib::reorder_within(bigram, n, Year), n, fill = Year)) +
  geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) +
  drlib::scale_x_reordered() +
  facet_wrap(~ Year, ncol = 2, scales = "free") +
  coord_flip()

