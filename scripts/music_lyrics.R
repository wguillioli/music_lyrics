# Project: Music Lyrics
# Purpose: Text mining of different music lyrics to get insights on their singing

rm(list = ls())
fldr <- "C:/GitHub/music_lyrics/"
setwd(fldr)

require(tidyverse)
require(readr)
require(tidytext)
require(wordcloud)
require(scales)
require(reshape2)
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
  
# SENTIMENT ANALYSIS

# plot sentiment change by album (calculated by line)
albumns_sentiment <- lyrics_words %>%
  inner_join(bing) %>%
  count(artist_album, song_line_number, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

ggplot(albumns_sentiment, aes(song_line_number, sentiment, fill = artist_album)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~artist_album, ncol = 2, scales = "free_x")


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

# for each album, get sentiment score by song and name
#voy




