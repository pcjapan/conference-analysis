setwd("~/Documents/R Worksets/Conference Analysis/Data")

library(tm)
library(SnowballC)
library(wordcloud)
library(corpustools)
library(quanteda.corpora)
titles2 = read.csv("titles.txt", stringsAsFactors = F, header = TRUE, sep = "\t")
head(titles2)
title_corpus = Corpus(VectorSource(titles2$Title))
title_corpus
f <- content_transformer(function(x, pattern) gsub(pattern, "", x))
title_corpus = tm_map(title_corpus, content_transformer(tolower))
title_corpus = tm_map(title_corpus, removeNumbers)
title_corpus <- tm_map(title_corpus, f, "[!\"#$%&'*+,./)(:;<=>?@]")
title_corpus = tm_map(title_corpus, removeWords, c("the", "and", "for", "that", "you", "why", "so", "one", "two", "three", "four", "it", "its", "who", "whats", "what", "most", "dont", "just", "which", "than", "but", "have", "there", "to", "too", "ten", "five", "has", "be", "with", "your", "about", "into", "are", "does", "all", "from"))
title_corpus =  tm_map(title_corpus, stripWhitespace)
title_corpus =  tm_map(title_corpus, stemDocument)
head(title_corpus)
inspect(title_corpus[1])
title_dtm <- DocumentTermMatrix(title_corpus)
title_dtm
inspect(title_dtm[500:505, 500:505])
removeSparseTerms(title_dtm, 0.99)
inspect(title_dtm[1,1:20])
findFreqTerms(title_dtm, 100)
freq <- data.frame(sort(colSums(as.matrix(title_dtm)), decreasing = TRUE))

wordcloud(rownames(freq), freq[,1], scale = c(4, 1), max.words = 50, random.order = FALSE, colors = brewer.pal(4, "Dark2"), rot.per = 0)

title_freq <- sort(colSums(as.matrix(title_dtm)), decreasing = TRUE)
head(title_freq)

 
tc = create_tcorpus(titles2, text_columns = "titles")
tc$preprocess('token', 'stem',
              remove_stopwords = TRUE, use_stemming = TRUE)
dtm = get_dtm(tc, feature = "stem")
dtm_wordcloud(dtm, nterms = 50)
words = get_kwic(tc, query = "korea*")

tc_subset = tc$subset_query(query = 'korea*', copy = T, window = 20)
dtm_wordcloud(dtm, nterms = 50)

g = semnet_window(tc, 'stem', window.size = 10)
g = backbone_filter(g, max_vertices = 100)
plot_semnet(g, return_graph = F)

library(topicmodels)
m = tc$lda_fit('stem', K = 10, alpha = .1)
terms(m, 10)


dtm = get_dtm(tc, 'stem')
m = LDA(dtm, k = 10)
dtm = dtm[slam::row_sums(dtm) > 0, ]
phi = as.matrix(posterior(m)$terms)
theta <- as.matrix(posterior(m)$topics)
vocab <- colnames(phi)
doc.length = slam::row_sums(dtm)
term.freq = slam::col_sums(dtm)[match(vocab, colnames(dtm))]
library(LDAvis)
json = createJSON(phi = phi, theta = theta,
                  vocab = vocab,
                  doc.length = doc.length,
                  term.frequency = term.freq)
serVis(json)

#### Titles - Quanteda - Collocations ####
### This works 4/18/23 ###
#### COLLOCATION ANALYSIS ####

library(quanteda)
library(quanteda.textstats)
library(dplyr)
library(ggplot2)

rt <- readtext::readtext("titles", text_field = "Titles", docvarsfrom = c("filenames"), docvarnames = "Year", encoding = "UTF-8")
head(rt)

fulltext <- corpus(rt)
a <- corpus_reshape(fulltext, to = 'sentences')

ndoc(a)


toks_a <- tokens_remove(tokens(a, remove_punct = TRUE), stopwords("english"))
toks_a_cap <- tokens_select(toks_a, 
                                valuetype = "regex", 
                                case_insensitive = FALSE, 
                                padding = TRUE) %>% 
  textstat_collocations()

toks_df <- as.data.frame(toks_a_cap[1:2])
toks_df <- toks_df[order(toks_df$count, decreasing = TRUE), ] 


#### Countries #### 
country = read.csv("Country Data/countryLang.txt", stringsAsFactors = F, header = TRUE, sep = "\t")
clist <- country %>% 
  group_by(Country)

regions <- countrycode::countrycode(clist[[3]], origin = "country.name", destination = "region")
df <- data.frame(clist, regions)

#### Dataframe of counts by region ####
rlist <- df %>% 
  group_by(regions) %>% 
  summarize(count = n())

clist <- country %>% 
  group_by(Country) %>% 
  summarize(count = n())

#### ACCEPTED ONLY ####
Alist <- allAcceptedCountry %>% 
  group_by(Country)
Aregions <- countrycode::countrycode(Alist[[1]], origin = "country.name", destination = "region")
Adf <- data.frame(Alist, Aregions)

arlist <- Adf %>% 
  group_by(Aregions) %>% 
  summarize(sum(n))

aclist <- Acountry %>% 
  group_by(Country) %>% 
  summarize(sum(n))

#### Rejected ONLY ####
Rlist <- rejectedCountry %>% 
  group_by(Country)
Rregions <- countrycode::countrycode(Rlist[[1]], origin = "country.name", destination = "region")
Rdf <- data.frame(Rlist, Rregions)

rrlist <- Rdf %>% 
  group_by(Rregions) %>% 
  summarize(sum(n))

rclist <- Rdf %>% 
  group_by(Country) %>% 
  summarize(sum(n))
rejected_regions <- rempsyc::nice_table(rrlist,
                                       title = c("Table 1", "Counts of Rejected Presentations by Region"))
flextable::save_as_docx(rejected_regions, path = "rejected_regions.docx")


#### ABSTRACTS ####

ab <- readtext::readtext("abstracts", text_field = "Titles", docvarsfrom = c("filenames"), docvarnames = "Year")
abC <- corpus(ab)
texts = corpus_reshape(abC, to = "paragraphs")

tokeninfo <- summary(texts)
tokeninfo$Year <- docvars(texts, "Year")
if (require(ggplot2)) ggplot(data = tokeninfo, aes(x = Year, y = Tokens, group = 1)) +
  geom_line() + geom_point() + scale_x_continuous(labels = c(seq(2003, 2022)),
                                                  breaks = seq(2003, 2022)) + theme_bw()
tokeninfo[which.max(tokeninfo$Tokens), ]
tokeninfo[which.min(tokeninfo$Tokens), ]

#### Clean the corpus ####

toks <- tokens(abC, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE)



#### Additional Processing if needed ####

toks <- tokens_compound(toks, pattern = phrase(multiword))
toks <-  tokens_select(toks, pattern = stopwords1, selection = 'remove')
toks <- tokens_remove(tokens(toks, remove_punct = TRUE), stopwords("english"))



#### Basic Stats - readibility ####

read.stats <- textstat_readability(corpus(texts), measure = c("Flesch", "Flesch.Kincaid"))
read.stats

#### Word count ####

ntoken(texts)

ab_col_caps <- tokens_select(toks, pattern = '^[A-Z]', 
                                valuetype = 'regex', 
                                case_insensitive = FALSE, 
                                padding = TRUE) %>% 
  textstat_collocations(min_count = 5, size = 3)
head(ab_col_caps, 20)
tail(ab_col_caps, 20)

toks_dfm <- dfm(toks)

# Clustering

cluster_dfm <- dfm_trim(toks_dfm, min_termfreq = 50, min_docfreq = 5)

cluster_dist_mat <- dfm_weight(cluster_dfm, scheme = "prop") %>%
  textstat_dist(method = "euclidean") %>% 
  as.dist()


dfm_cluster <- hclust(cluster_dist_mat)
dfm_cluster$labels <- docnames(toks_dfm)

plot(dfm_cluster, xlab = "", sub = "", 
     main = "Euclidean Distance on Normalized Token Frequency")

# Hapax richness, defined as the number of words that occur only once divided by the total number of words
# https://quanteda.io/articles/pkgdown/replication/digital-humanities.html

hapax_proportion <- rowSums(toks_dfm == 1) / ntoken(toks_dfm)
barplot(hapax_proportion, beside = TRUE, col = "grey")
topfeatures(toks_dfm, n = 200)
toks.df <- convert(toks_dfm, to = "data.frame")



#### Most referenced authors ####

authors <- read.delim("authors.txt", sep = "\t", header = TRUE)
head(authors)

authors.long <- reshape::melt(authors, id = ("Year"))
authors.long <- authors.long  %>%  
  dplyr::rename(c("Author" = "variable", "Count" = "value"))
authors.long$Year <- as.factor(authors.long$Year)

ggplot(authors.long) + aes(x = Year, y = Count) +
  geom_point(size = 2, color = "black", fill = "black", shape = 21) +
  geom_segment( aes(x = Year, xend = Year, y = 0, yend = Count), color = "gray70") + 
  scale_x_discrete(breaks = c("2003","2007","2011","2015","2019" )) +
  scale_y_continuous(breaks = seq(0,18,2)) +
  xlab("") +
  ylab("Number of Mentions") +
  coord_flip() +
  theme_minimal() +
  facet_wrap(vars(Author))

if (require("quanteda.textmodels")) {
  quant_dfm <- dfm_trim(toks_dfm, min_termfreq = 10, max_docfreq = 10)
}

tstat_doc <- textstat_simil(quant_dfm, quant_dfm[c("2021.txt", "2022.txt"), ], margin = "documents", method = "simple matching")
as.list(tstat_doc)
dotchart(as.list(tstat_doc)$"2022", xlab = "Cosine similarity", pch = 19)

(tstat4 <- textstat_dist(quant_dfm, margin = "documents"))
as.matrix(tstat4)
as.list(tstat4)
as.dist(tstat4)

plot(hclust(as.dist(tstat4)))
#### WORDCLOUDS for full corpus #### 

textplot_wordcloud(toks_dfm,
                   max_words = 175,
                   labelsize = 2,
                   labeloffset = .1,
                   min_size = .5,
                   max_size = 1.5,
                   rotation = 0,
                   adjust = -.1,
                   min_count = 15)

textplot_wordcloud(toks_dfm,
                   max_words = 150,
                   labelsize = 2,
                   min_size=.5,
                   max_size = 1.5,
                   rotation = 0,
                   adjust = -.1,
                   min_count = 20,
                   color = "grey10")

library(tm)
library(wordcloud)
library(cluster) 
library(ggplot2)
cname <- NULL
cname <- file.path("LD")
docs <- VCorpus(DirSource(cname))

#### Preparing data#### 

docs <- tm_map(docs,removePunctuation) 
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, tolower)
docs <- tm_map(docs, removeWords, stopwords("english"))
for (j in seq(docs)) {
  
  docs[[j]] <- gsub("can do", "can_do", docs[[j]])
  docs[[j]] <- gsub("english learners", "english_learners", docs[[j]])
  docs[[j]] <- gsub("extensive reading", "extensive_reading", docs[[j]])
  docs[[j]] <- gsub("foreign language", "foreign_language", docs[[j]])
  docs[[j]] <- gsub("high school", "high_school", docs[[j]])
  docs[[j]] <- gsub("language classroom", "language_classroom", docs[[j]])
  docs[[j]] <- gsub("language learners", "language_learners", docs[[j]])
  docs[[j]] <- gsub("language learning", "language_learning", docs[[j]])
  docs[[j]] <- gsub("language teaching", "language_teaching", docs[[j]])
  docs[[j]] <- gsub("language use", "language_use", docs[[j]])
  docs[[j]] <- gsub("learner agency", "learner_agency", docs[[j]])
  docs[[j]] <- gsub("learner autonomy", "learner_autonomy", docs[[j]])
  docs[[j]] <- gsub("learner motivation", "learner_motivation", docs[[j]])
  docs[[j]] <- gsub("school students", "school_students", docs[[j]])
  docs[[j]] <- gsub("second language", "second_language", docs[[j]])
  docs[[j]] <- gsub("student motivation", "student_motivation", docs[[j]])
  docs[[j]] <- gsub("task based", "task_based", docs[[j]])
  docs[[j]] <- gsub("teaching english", "teaching_english", docs[[j]])
  docs[[j]] <- gsub("teaching young", "teaching_young", docs[[j]])
  docs[[j]] <- gsub("university students", "university_students", docs[[j]])
  
}
docs <- tm_map(docs, removeWords, c("presenter", "presentation", "presenters", "present", "describe", "discuss", "report", "reports","study", "examines", "investigates", "workshop", "poster", "participant", "participants", "result", "research", "one", "two", "three", "four", "among", "session", "introduce", "many", "paper", "examples", "also", "share", "may", "explore", "ideas", "findings", "show", "analysis", "data", "short", "survey", "issues", "way", "ways","will","presented", "reported", "examined", "questionnaire", "explain", "found", "give", "given", "several", "year", "first", "however", "can"))
docs <- tm_map(docs, PlainTextDocument)

## docs <- tm_map(docs, stemDocument)

#### Now to work with the data #### 

dtm <- DocumentTermMatrix(docs) 
tdm <- TermDocumentMatrix(docs)  
freq <- colSums(as.matrix(dtm))
ord <- order(freq)
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)  
set.seed(NULL)
set.seed(142)
dark2 <- brewer.pal(8, "Dark2")   
wordcloud(names(freq), freq, scale = c(3,1), min.freq=4, rot.per=0, colors=dark2, max.words=100, random.order = FALSE)

library(quanteda)
dict <- quanteda::dictionary(list(nation = c('nation', 'extensive*')))
dict_toks <- quanteda::tokens_lookup(toks_a, dictionary = dict)
print(dict_toks)
quanteda::dfm(dict_toks)
toks_ngram <- tokens_ngrams(dict_toks, n = 2:4)
head(toks_ngram)
abC.stats <- summary(abC)
abC.stats$Text <- reorder(abC.stats$Text, 1:ndoc(abC), order = T)

nation.dict <- dictionary(list(author = c("nation", "vocabulary"), er = c("extensive", "read*")))

dict_dtm <- dfm_lookup(dtm, nation.dict, nomatch = "_unmatched")
tail(dict_dtm)


toks <- tokens(abC, remove_punct = TRUE)
toks <- tokens_select(toks, pattern = stopwords(source = 'smart'), selection = 'remove')
tstat_col_caps <- tokens_select(toks, pattern = '^[A-Z]', 
                                valuetype = 'regex', 
                                case_insensitive = FALSE, 
                                padding = TRUE) %>% 
  textstat_collocations(min_count = 50)
head(tstat_col_caps, 20)


##### FORMAT ####
##### 
format <- read.delim("format.txt", header = TRUE, sep = "\t")
format$Year <- as.factor(format$Year)
library(dplyr)
library(ggplot2)
library(ggridges)
library(viridis)
library(hrbrthemes)
library(forcats)

p <- data.frame(format %>%
                     filter(Status %in% "A") %>% 
                     group_by(Format, Year) %>%
                     summarise(no_rows = length(Format))
                   ) %>%
  arrange(desc(no_rows)) %>%
  mutate(text = paste("Number of\nSessions: ", no_rows)) %>%
  ggplot(aes(x = no_rows, y = Format, size = no_rows, colour = Format, text = text)) +
  geom_point(alpha = 0.7)  +
  scale_size(range = c(2, 25)) +
  theme_ipsum() +
  scale_color_viridis(discrete = TRUE, guide = FALSE) +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
  xlab("Session Count") +
  facet_wrap(~Year)


pp <- plotly::ggplotly(p, tooltip = "text")
pp


focus <- read.delim("format-2003-6.txt", header = TRUE, sep = "\t")
focus$Year <- as.factor(focus$Year)
f2.df <- data.frame(focus %>%
                     filter(Status %in% "A") %>% 
                     group_by(Year, Format) %>%
                     summarise(no_rows = length(Format))
)

ggplot(f2.df, aes(x = no_rows, y = forcats::fct_reorder(Format, no_rows, .fun = sum), fill = Format, height = after_stat(density))) +
  geom_density_ridges(stat = "binline", bins = 8, scale = 1) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_fill_brewer(palette = 4) +
  theme_ridges()


##### Content / Context ####
##### 

content <- read.delim("content-context.txt", header = TRUE, sep = "\t")

content_df <- data.frame(content %>%
                  filter(Status %in% "A") %>% 
                  group_by(Category, Year) %>%
                  summarise(no_rows = length(Category))
) %>%
  arrange(desc(no_rows)) %>%
  mutate(text = paste("Number of\nSessions: ", no_rows)) %>%
  ggplot(aes(x = no_rows, y = Category, size = no_rows, colour = Category, text = text)) +
  geom_point(alpha = 0.7)  +
  scale_size(range = c(1, 10)) +
  theme_ipsum() +
  scale_color_viridis(discrete = TRUE, guide = FALSE) +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
  xlab("Session Count") +
  facet_wrap(~Year)

context_plot <- data.frame(content %>%
                             filter(Status %in% "A") %>% 
                             group_by(Context, Year, .groups = "keep") %>%
                             summarise(no_rows = length(Context))
) %>%
  arrange(desc(no_rows)) %>%
  mutate(text = paste("Number of\nSessions: ", no_rows)) %>%
  ggplot(aes(x = no_rows, y = Context, size = no_rows, colour = Context, text = text)) +
  geom_point(alpha = 0.7)  +
  scale_size(range = c(1, 10)) +
  theme_ipsum() +
  scale_color_viridis(discrete = TRUE, guide = FALSE) +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
  xlab("Session Count") +
  facet_wrap(~Year)
context_plot
#### Methodology-related sessions ####
#### Working with dfm - dfmat3 from quanteda.R ####

head(dfmat3)
tidy_td <- tidy(dfmat3)
tidy_dfm


year_term_counts <- tidy_dfm %>%
  extract(row, "year", "(\\d+)", convert = TRUE) %>%
  complete(year, column, fill = list(value = 0)) %>%
  group_by(year) %>%
  mutate(year_total = sum(value))
head(year_term_counts)


#### Exploring frequency of terms with regression line  ####
#   Note using log10 transformation on y axis  #


year_term_counts %>%
  filter(column %in% c("data", "pilot", "qualitative", "quantitative", "questionnaire", "reliability", "research")) %>%
  ggplot(aes(year, value)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ column, nrow = 2) +
  scale_y_log10() +
  ylab("Word Count") +
  theme_minimal()

library(tidyverse)
data <- read.delim("content-summed.txt", header = TRUE, sep = "\t")

empty_bar <- 2
to_add <- matrix(NA, empty_bar, ncol(data))
colnames(to_add) <- colnames(data)
data <- rbind(data, to_add)
data$ID <- seq(1, nrow(data))

label_data <- data
number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$ID-0.5) /number_of_bar
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)
p <- ggplot(data, aes(x=as.factor(ID), y=Count)) +
  geom_bar(stat="identity", fill=alpha("skyblue", 0.7)) +
  annotate("text", x = rep(max(data$id),5), y = c(10, 100, 500, 1000, 1500), label = c("10", "100", "500", "1000", "1500") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  ylim(-100,1650) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")      # Adjust the margin to make in sort labels are not truncated!
  ) +
  coord_polar(start = 0) +
  geom_text(data=label_data, aes(x=ID, y=Count+10, label=Category, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 


text-c <- scan("summary")
