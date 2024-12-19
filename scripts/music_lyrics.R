# Iron Maiden

rm(list = ls())

require(tidyverse)
require(readr)
require(tidytext)
require(wordcloud)
data("stop_words")

fldr <- "C:/GitHub/music_lyrics/"
setwd(fldr)

#fldr <- "C:/GitHub/maiden/data/number_of_the_beast/"
#fldr <- "C:/GitHub/maiden/data/the_x_factor/"

(artists <- list.dirs(path = "data/", 
                   full.names = FALSE, 
                   recursive = FALSE))

(albumns <- list.dirs(path = paste0("data/",artists[1]), 
                      full.names = FALSE, 
                      recursive = FALSE))

album_to_parse <- paste0(fldr, "data/", artists[1], "/", albumns[1])


d <- data.frame()

list.files(album_to_parse)

for (i in 1:length((list.files(fldr)))){
  print(i)

  song_path <- paste0(fldr, 
                      list.files(fldr)[i])
  
  lyrics <- read_file(song_path)
  
  file_name <- list.files(fldr)[i]
  
  song_num <- unlist(str_split(file_name, pattern = "_"))[1]
  
  song_name <- unlist(str_split(file_name, pattern = "_"))[2]
  #song_name <- unlist(str_split(song_name, pattern = "."))[1]
  
  song_row <- c(song_num, song_name, lyrics)
  names(song_row) <- c("song_num", "song_name", "lyrics")
  
  d <- bind_rows(d, song_row)
  
}

u <- d %>%
  unnest_tokens(word, lyrics)

u <- u %>%
  anti_join(stop_words)

u %>%
  count(word, sort = TRUE)

u %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 50))

#d <- tibble(song_num = character(), 
#                song_name = character(), 
#                lyrics =  character())


# https://rpubs.com/rafrys/723764
# https://www.tidytextmining.com/tidytext
# https://www.azlyrics.com/i/ironmaiden.html


