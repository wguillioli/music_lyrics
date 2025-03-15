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
data("stop_words")

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

# most common words per album
# voy terminar 1.3
lyrics_words %>%
  count(word, artist_album, sort = TRUE) %>%
  ungroup() %>%
  group_by(artist_album) %>%
  slice_max(n, n = 7, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(word = reorder(word, n))





