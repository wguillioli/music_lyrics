# Project: Music Lyrics
# Purpose: Text mining of different music lyrics to get insights on their singing

rm(list = ls())
fldr <- "C:/GitHub/music_lyrics/"
setwd(fldr)

# Custom ggplot theme
cust_theme <- theme_minimal() +
  theme(panel.grid.major = element_line(color = "#e1e1e1",  linetype = "dotted"),
        panel.grid.minor = element_blank(),
        legend.position  = "bottom",
        legend.key       = element_blank())

blue_palette <- c("#004f79", "#0085ca",  "#66b5df", "#474747","#eeeeee")

cbPalette <- c("#CC6666","#E69F00", "#999999", "#56B4E9", "#009E73", "#F0E442",
               "#0072B2", "#D55E00", "#CC79A7")
  

require(tidyverse)
require(readr)
require(tidytext)
require(wordcloud)
require(scales)
require(reshape2)
library(forcats)
library(igraph)
library(ggraph)
require(tm)
require(widyr)
require(topicmodels)
data("stop_words")

afinn <- get_sentiments("afinn")
bing <- get_sentiments("bing")
nrc <- get_sentiments("nrc")

# LOAD DATA

# get all artists from data folder
artists <- list.dirs(path = "data/", 
                     full.names = FALSE, 
                     recursive = FALSE)

# for each artist get all album names into df with one row per each combination
artists_albums_df <- tibble()

for (i in 1:length(artists)){
  albums <- list.dirs(path = paste0("data/",artists[i]), 
                      full.names = FALSE, 
                      recursive = FALSE)
  
  for (j in 1:length(albums)){
    artist_album_row <- c(artists[i], albums[j])
    names(artist_album_row) <- c("artist", "album")
    
    artists_albums_df <- bind_rows(artists_albums_df,
                                   artist_album_row)
  }
}

# for each album, get all the song names and lyrics into a df
lyrics_df <- tibble()

for (i in 1:nrow(artists_albums_df)){
  album_path <- paste0(fldr, "data/",
                       artists_albums_df[i,1], "/", #artist
                       artists_albums_df[i,2]) #album
  
  songs <- list.files(album_path)
  
  for (j in 1:length(songs)){
    song_num <- unlist(str_split(songs[j], pattern = "_"))[1]
    song_name <- unlist(str_split(songs[j], pattern = "_"))[2]
    song_path <- paste0(album_path, "/", songs[j])
    lyrics <- read_file(song_path)
    print(song_path)
    
    song_name <- strsplit(song_name, split = "[.]")[[1]][1]
    
    lyric_row <- c(artists_albums_df[i,1],
                   artists_albums_df[i,2],
                   paste0(artists_albums_df[i,1], ": ", artists_albums_df[i,2]),
                   song_num,
                   song_name,
                   lyrics)
    
    names(lyric_row) <- c("artist", "album", "artist_album", 
                          "song_num", "song", "lyrics")
    
    lyrics_df <- bind_rows(lyrics_df, lyric_row)
  }
}

# DATA PREP

# tokenize lyrics per sentence
lyrics_sentences <- lyrics_df %>%
  unnest_tokens(song_line, lyrics, token = "lines") %>%
  mutate(artist_album_song = paste0(artist_album, song)) %>%
  group_by(artist_album_song) %>%
  mutate(song_line_number = row_number()) %>%
  ungroup() %>%
  select(-artist_album_song)
    
# tokenize lyrics per word
lyrics_words <- lyrics_sentences %>%
  unnest_tokens(word, song_line) 
  
# remove stop words
lyrics_words <- lyrics_words %>%
  anti_join(stop_words)

# print # of words per album
lyrics_words %>%
  group_by(artist_album) %>%
  summarise(number_of_words = n())

# most common words across all albums
# a mix of love and war
lyrics_words %>%
  count(word, sort = TRUE)

# plot top 10 most common words
lyrics_words %>%
  count(word, sort = TRUE) %>%
  slice(1:10) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(n,word)) +
  geom_col()
  
# most common words per album
lyrics_words %>% 
  count(artist_album, word, sort = TRUE) %>%
  group_by(artist_album) %>%
  slice(1:10) %>%
  print(n = Inf)

# add plot
  
# SENTIMENT ANALYSIS

# plot sentiment change by album (calculated by line)
albumns_sentiment <- lyrics_words %>%
  inner_join(bing) %>%
  count(artist_album, song_line_number, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

ggplot(albumns_sentiment, aes(song_line_number, sentiment, fill = artist_album)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~artist_album, ncol = 2, scales = "free_x") +
  cust_theme


# top sentiments per album
nrc_filtered <- nrc %>%
  filter(sentiment != "positive" & sentiment != "negative")

lyrics_words %>%
  inner_join(nrc_filtered, relationship = "many-to-many") %>%
  count(artist_album, sentiment, sort = TRUE) %>%
  group_by(artist_album) %>%
  slice(1:5)

# sadness is common, so what are the top sad words for each album
nrc_sad <- get_sentiments("nrc") %>% 
  filter(sentiment == "sadness")

lyrics_words %>%
  #filter(album == "Number of the Beast") %>%
  inner_join(nrc_sad) %>%
  count(artist_album, word, sort = TRUE) %>%
  group_by(artist_album) %>%
  slice_max(order_by = n, n = 5)

# most common and positive words per album

albums_to_parse <- unique(lyrics_words$artist_album)

for (i in 1:length(albums_to_parse)){
  print(i)
  
  lyrics_words_top <- lyrics_words %>%
    filter(artist_album == albums_to_parse[i]) %>%
    inner_join(bing) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup()
  
  myplot <- lyrics_words_top %>%
    group_by(sentiment) %>%
    slice_max(n, n = 10, with_ties = FALSE) %>% 
    ungroup() %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(n, word, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") +
    labs(x = "Contribution to sentiment",
         y = NULL) +
    ggtitle(albums_to_parse[i])
  
  print(myplot)
  
}

# hallowed not positive necesariamente

# word clouds by album
set.seed(666)
lyrics_words %>%
  filter(artist_album == albums_to_parse[1]) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))  

set.seed(666)
lyrics_words %>%
  filter(artist_album == albums_to_parse[2]) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))  

set.seed(666)
lyrics_words %>%
  filter(artist_album == albums_to_parse[3]) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))  

# plot / calculate most negative songs per album

words_per_song <- lyrics_words %>%
  group_by(artist_album, song) %>%
  summarise(words = n())

negative <- bing %>% 
  filter(sentiment == "negative")

negative_words_per_song <- lyrics_words %>%
  semi_join(negative)

negative_ratio <- negative_words_per_song %>%
  group_by(artist_album, song) %>%
  summarise(negativewords =n()) %>%
  left_join(words_per_song, by = c("artist_album", "song")) %>%
  mutate(ratio = round(negativewords/words, 2)) %>%
  ungroup()

ggplot(negative_ratio, aes(song, ratio, fill = artist_album)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~artist_album, ncol = 2, scales = "free_y") +
  coord_flip()

#
# tf-idf

album_words <- lyrics_df %>%
  unnest_tokens(word, lyrics) %>%
  count(artist_album, word, sort = TRUE)

total_words <- album_words %>% 
  group_by(artist_album) %>% 
  summarize(total = sum(n))

album_words <- left_join(album_words, total_words)

album_tf_idf <- album_words %>%
  bind_tf_idf(word, artist_album, n)

album_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

album_tf_idf %>%
  group_by(artist_album) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = artist_album)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~artist_album, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

# 4 n-grams

lyrics_df$lyrics_clean <- removeWords(lyrics_df$lyrics, stop_words$word)

# try here with clean lyrics vs normal
lyrics_bigrams <- lyrics_df %>%
  unnest_tokens(bigram, lyrics, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))

lyrics_bigrams %>%
  count(bigram, sort = TRUE)

bigrams_separated <- lyrics_bigrams %>%
  separate(bigram,c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_tfidf <- bigrams_united %>%
  count(artist_album, bigram) %>%
  bind_tf_idf(bigram, artist_album, n) %>%
  arrange(desc(tf_idf))

bigrams_tfidf %>%
  group_by(artist_album) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf), fill = artist_album)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~artist_album, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

# function that takes album name of album and ngram filter 
# and plots the network graph by album
plot_graph <- function(album_to_plot, num){

  bigram_counts <- bigrams_filtered %>%
    filter(artist_album == album_to_plot) %>%
    count(word1, word2, sort = TRUE)
  
  #bigram_counts
  
  bigram_graph <- bigram_counts %>%
    filter(n > num) %>%
    graph_from_data_frame()
  
 # a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  
  set.seed(97702)
  ggraph(bigram_graph, layout = "fr") +
    geom_edge_link() +
    geom_node_point(color = "gray", size = 3) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
  
}

plot_graph(albums_to_parse[1], 1)

plot_graph(albums_to_parse[2], 1)

plot_graph(albums_to_parse[3], 2)

plot_graph(albums_to_parse[4], 3)


# impact of negated words
negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(afinn, by = c(word2 = "word")) %>%
  count(artist_album, word1, word2, value, sort = TRUE)

negated_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by \"not\"")

# 4.2 corr

# correlation of words

# correlating among albumns 

words_pairs <- lyrics_words %>%
  pairwise_count(word, artist_album, sort = TRUE)

words_pairs
table(words_pairs$n)

word_corrs <- lyrics_words %>%
  group_by(word) %>%
  filter(n() >= 10) %>%
  pairwise_cor(word, artist_album, sort = TRUE)

word_corrs

word_corrs %>%
  filter(item1 %in% c("heaven", "love", "hate", "mad")) %>%
  group_by(item1) %>%
  slice_max(correlation, n = 6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

# didn't work

# LDA topic model on albums

word_counts_beast <- lyrics_words %>%
  #filter(artist_album == "Iron Maiden: Number of the Beast") %>%
  count(artist_album, word, sort = TRUE)

beast_dtm <- word_counts_beast %>%
  cast_dtm(artist_album, word, n)

beast_dtm

lda <- LDA(beast_dtm, k = 4, control = list(seed = 1234))
lda

topics <- tidy(lda, matrix = "beta")

topics

top_terms <- topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()