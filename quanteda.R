# install.packages("readtext")
# install.packages("quanteda")
# install.packages("topicmodels")
library(readtext)
library(quanteda.textstats)
library(quanteda.textplots)
library(quanteda.textmodels)
library(quanteda)
library(topicmodels)
library(ggplot2)
library(RColorBrewer)
library(tidytext)
library(tidyr)
library(dplyr)

corp <-
  readtext::readtext("Data/abstracts",
                     docvarsfrom = c("filenames"),
                     docvarnames = "Year")


fulltext <- corpus(corp)
fulltext <- corpus_reshape(fulltext, to = "paragraphs")
dtm <- tokens(fulltext)
dtm <- tokens_remove(dtm, stopwords("english"))
dtm <- tokens(dtm,
              what = "word",
              remove_punct = TRUE,
              remove_symbols = TRUE,
              remove_numbers = TRUE,
              remove_url = TRUE,
              remove_separators = TRUE,
              split_hyphens = FALSE,
)
dfm <- dfm(dtm)
doc_freq <- docfreq(dfm)
dfm <- dfm[, doc_freq >= 2]
dfm <- dfm_tfidf(dfm, scheme_tf = "count")
dfm

### Convert to bigrams first, then do this ###

corpus_year = corpus_subset(fulltext, Year %in% c("2003":"2021", "2022"))
dtm_year <-  tokens(
  corpus_year,
  remove_numbers = TRUE,
  remove_punct = TRUE,
  remove_symbols = TRUE,
  split_hyphens = FALSE
  ) %>% 
  tokens_remove(stopwords("english")) 

dfm_year <- dfm(dtm_year)

keyness = textstat_keyness(dfm_year, target = c("2022.txt"))
textplot_keyness(keyness)
set.seed(142)


wordcloud::comparison.cloud(dtm)

textplot_wordcloud(
  dtm_year,
  comparison = TRUE,
  max_words = 100,
  color = c("blue", "red", "goldenrod", "gray40"),
  labelcolor = c("gray10"),
  labelsize = 2,
  min_size = 1,
  max_size = 2,
  rotation = .33
)




g <- textplot_xray(
  kwic(tokens(fulltext), pattern = "statistic*"),
  kwic(tokens(fulltext), pattern = "research"),
  kwic(tokens(fulltext), pattern = "analy*"),
  kwic(tokens(fulltext), pattern = "theor*"),
  scale = "absolute",
  sort = TRUE
)


g + theme(legend.position = "none")


kw <- kwic(dtm, pattern = "theory", valuetype = "fixed")

tx <- textplot_xray(kw,
                    scale = "absolute",
                    sort = TRUE)

dtm_year %>%
  textstat_frequency(n = 50) %>%
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  labs(x = NULL, y = NULL) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust =
                                                       1))
wf <- textmodel_wordfish(dtm_year)
textplot_scale1d(wf, margin = "features",
                 groups = TRUE,
                 highlighted = c("theory", "motivation"), 
                 highlighted_color = "red")

freq <- textstat_frequency(dtm_year)
head(freq, 100)


### Rejected
rt2 <-
  readtext(
    "/Users/Home/Documents/R Worksets/JALT2018/JALT2018 Prez/rejected-titles",
    text_field = "Abstracts",
    docvarsfrom = c("filenames"),
    docvarnames = "Year"
  )
fulltext2 <- corpus(rt2)
dtm_title2 = dfm(
  fulltext2,
  remove = stopwords("english"),
  remove_numbers = TRUE,
  remove_punct = TRUE,
  stem = FALSE
)
pal2 <- brewer.pal(8, "Dark2")
textplot_wordcloud(
  dtm_title2,
  comparison = FALSE,
  max_words = 200,
  min_count = 15,
  min_size = 1,
  max_size = 3,
  rotation = .33,
  color = pal2
)

#### tidyverse

title2_td <- tidy(dfm)
title2_td

title2_td %>%
  count(term, sort = TRUE) %>%
  filter(n > 15) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

title2_td_idf <- title2_td %>%
  bind_tf_idf(term, document, count) %>%
  arrange(desc(tf_idf))

year_term_counts <- title2_td %>%
  extract(document, "year", "(\\d+)", convert = TRUE) %>%
  complete(year, term, fill = list(count = 0)) %>%
  group_by(year) %>%
  mutate(year_total = sum(count))

year_term_counts %>%
  filter(term %in% c("theory", "agency", "students")) %>%
  ggplot(aes(year, count / year_total)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~term, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format()) +
  ylab("% frequency of word in titles")

title2_td_idf %>%
  select(-count) %>%
  arrange(desc(tf_idf))

### USE THIS MARCH 2023 #####

#### Some crossover with the above here.
##### Working with abstracts to get frequency counts, wordclouds

#### Load quanteda-base.R


toks <-
  tokens(
    fulltext,
    remove_punct = TRUE,
    remove_numbers = TRUE,
    remove_symbols = TRUE
  )
compound_toks <- tokens_compound(toks, pattern = phrase(multiword))
toks2 <- tokens_remove(compound_toks, stopwords("english"))
cleaned_toks <-
  tokens_select(toks2, pattern = stopwords1, selection = 'remove')
cleaned_toks2  <-
  tokens_select(toks2,
                pattern = c(stopwords1, stopwords2),
                selection = 'remove')
toks_dfm <-
  dfm(toks2) ## use cleaned_toks2 if require stopwords removed
toks_dfm_clean <- dfm(cleaned_toks2)


##Subset to work with first and last years
corpus_subset(fulltext, Year %in% c(2022)) %>%
  tokens(
    remove_punct = TRUE,
    remove_numbers = TRUE,
    remove_symbols = TRUE
  ) %>%
  tokens_compound(pattern = phrase(multiword)) %>%
  tokens_remove(stopwords("english")) %>%
  tokens_select(pattern = stopwords1, selection = 'remove') %>%
  tokens_select(pattern = c(stopwords1, stopwords2),
                selection = 'remove') %>%
  ## s_cleaned_toks2 <- tokens_wordstem(s_cleaned_toks2) ## Stemming
  dfm() %>%
  textplot_wordcloud(
    max_words = 200,
    labelsize = 2,
    labeloffset = .1,
    color = "red",
    ordered_color = TRUE,
    random_order = FALSE,
    min_size = .5,
    max_size = 2,
    rotation = 0,
    min_count = 75,
  )
mtext("2021",
      adj = 0,
      #1
      cex = 3,
      line = -50) #-10
      
      corpus_year = corpus_subset(fulltext, Year %in% c("2004", "2003", "2021", "2022"))
      dtm <-
        tokens(corpus_year,
               remove_numbers = TRUE,
               remove_symbols = TRUE)
      fm <- tokens_remove(dtm, stopwords("english"))
      dtm <- dfm(fm, tolower = FALSE, remove_punct = TRUE)
      keyness = textstat_keyness(dtm)
      textplot_keyness(keyness)
      
      
      
      
      #### Unsupervised Machine Learning ####
      # See "Text Analysis in R", p. 257
      # In first par_dtm use text[x] to look at model for single year for comparative purposes
      
      LDcorp <-
        readtext(
          "Data/LD",
          text_field = "Titles",
          docvarsfrom = c("filenames"),
          docvarnames = "Year"
        )
      
      
      fulltext <- corpus(LDcorp)
      stopwords1 <- c("also",
          "may",
          "can",
          "presentation",
          "data",
          "one",
          "two",
          "use",
          "asked",
          "used",
          "participants",
          "study",
          "research",
          "often")
      texts <- corpus_reshape(fulltext, to = "paragraphs")
      par_dtm <-
        dfm(
          texts[17],
          remove_punct = TRUE,
          remove_numbers = TRUE,
          remove_symbols = TRUE,
          remove = c(stopwords("english"), stopwords1)
        )
      
      par_dtm <- dfm_trim(par_dtm, min_termfreq = 5)
      par_dtm <- convert(par_dtm, to = "topicmodels")
      set.seed(1)
      lda_model <- topicmodels::LDA(par_dtm, method = "Gibbs", k = 10)
      mod <- terms(lda_model, 10)
      mod
      # Readability Stats
      
      read.stats <-
        textstat_readability(corpus(texts), measure = "Flesch.Kincaid")
      summary(texts)
      
      read.stats
      
      # Getting frequencies
      
      dfmat3 <- toks_dfm %>%
        dfm_group(groups = "Year") %>%
        dfm_weight(scheme = "count")
      
      
      tstat2 <- textstat_frequency(dfmat3, n = 10,
                                   groups = "Year")
      
      
      
      freqPlot <-
        ggplot(data = tstat2, aes(x = nrow(tstat2):1, y = frequency)) +
        geom_point() +
        facet_wrap( ~ group, scales = "free") +
        coord_flip() +
        scale_x_continuous(breaks = nrow(tstat2):1,
                           labels = tstat2$feature) +
        labs(x = NULL, y = "Word Count")
      
      freqPlot
      topfeatures(dfmat3, 25)
      # similarities
      
      textstat_simil(dfmat3, method = "correlation", margin = "documents")
      
      
      # Proportions rather than counts
      
      dfmat <- toks_dfm %>%
        dfm_group(groups = "Year") %>%
        dfm_weight(scheme = "propmax")
      
      
      freq = textstat_frequency(dfmat, groups = docnames(dfmat))
      
      
      
      tbl <- topfeatures(toks_dfm, 25)
      tbl <- as.data.frame(tbl)
      kable(tbl, format)
      
      
      topfeatures(dfmat, 25)
      