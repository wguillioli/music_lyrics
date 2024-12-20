# Project: Music Lyrics
# Purpose: Text mining of differnet music lyrics to understand what artists
# sing about

rm(list = ls())
fldr <- "C:/GitHub/music_lyrics/"
setwd(fldr)

require(tidyverse)
require(readr)
require(tidytext)
require(wordcloud)
data("stop_words")

# loop over data folder to get a list of all combinations of artist-albums
# that we need to extract the songs and lyrics from
# return should be a list frame with colums: artist, album, song

# get all artists from data folder
artists <- list.dirs(path = "data/", 
                      full.names = FALSE, 
                      recursive = FALSE)

# now, for each artist get all albums
artists_albums_df <- data.frame()

for (i in 1:length(artists)){
  #print(artists[i])
  
  albums <- list.dirs(path = paste0("data/",artists[i]), 
                      full.names = FALSE, 
                      recursive = FALSE)
  
  for (j in 1:length(albums)){
   # print(albums[j])
    
    artist_album_row <- c(artists[i], albums[j])
    names(artist_album_row) <- c("artist", "album")
#    print(artist_album_row)
    
    artists_albums_df <- bind_rows(artists_albums_df,
                                   artist_album_row)
    
  }
}

# for each album, get all the song names and lyrics into a df
# df has artist, album, # song, song name, lyrics columns

lyrics_df <- data.frame()

for (i in 1:nrow(artists_albums_df)){
  #print(artists_albums_df[i,])
  
  album_path <- paste0(fldr, "data/",
                       artists_albums_df[i,1], "/", #artist
                       artists_albums_df[i,2]) #album
  #print(album_path)
  
  songs <- list.files(album_path)
  #print(songs)
  
  for (j in 1:length(songs)){
    
    song_num <- unlist(str_split(songs[j], pattern = "_"))[1]
    
    song_name <- unlist(str_split(songs[j], pattern = "_"))[2]
    
    #song_name <- songs[j]
    
    song_path <- paste0(album_path, "/", songs[j])
    
    lyrics <- read_file(song_path)
    
    print(song_path)
    
    lyric_row <- c(artists_albums_df[i,1],
                   artists_albums_df[i,2],
                   song_num,
                   song_name,
                   lyrics)
    
    names(lyric_row) <- c("artist", "album", "song_num", "song", "lyrics")
    
    lyrics_df <- bind_rows(lyrics_df, lyric_row)
    
  }
  
}

#tokenize all lyrics and clean
lyrics_tokens_df <- lyrics_df %>%
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words)

beast <- lyrics_tokens_df %>%
  filter(artist == "Iron Maiden" & album == "Number of the Beast")

xfactor <- lyrics_tokens_df %>%
  filter(artist == "Iron Maiden" & album == "The X Factor")

t1989 <- lyrics_tokens_df %>%
  filter(artist == "Taylor Swift" & album == "1989")

#word clouds
beast %>%
  count(word, sort = TRUE) %>%
  with(wordcloud(word, n, max.words = 50))

xfactor %>%
  count(word, sort = TRUE) %>%
  with(wordcloud(word, n, max.words = 50))

t1989 %>%
  count(word, sort = TRUE) %>%
  with(wordcloud(word, n, max.words = 50))



# https://rpubs.com/rafrys/723764
# https://www.tidytextmining.com/tidytext
# https://www.azlyrics.com/i/ironmaiden.html


