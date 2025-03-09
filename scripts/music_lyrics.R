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
require(scales)
data("stop_words")

# do a 3 color palete
# do a ggplot template with theme minimal


# loop over data folder to get a list of all combinations of artist-albums
# that we need to extract the songs and lyrics from
# return should be a list frame with colums: artist, album, song

# get all artists from data folder
artists <- list.dirs(path = "data/", 
                      full.names = FALSE, 
                      recursive = FALSE)

# for each artist get all album names
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

# tokenize lyrics
lyrics_tokens <- lyrics_df %>%
  unnest_tokens(song_line, lyrics, token = "lines") %>% 
  mutate(artist_album_song = paste0(artist_album, song)) %>%
  group_by(artist_album_song) %>%
  mutate(line_number = row_number()) %>%
  ungroup() %>%
  select(-artist_album_song) %>%
  unnest_tokens(word, song_line) %>%
  anti_join(stop_words)

# print # of words per album
lyrics_tokens %>%
  group_by(artist_album) %>%
  summarise(number_of_words = n())

# plot top 10 most used words per album
words_per_album <- 
lyrics_tokens %>%
  count(word, artist_album, sort = TRUE) %>%
  ungroup() %>%
  group_by(artist_album) %>%
  slice_max(n, n = 7, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(word = reorder(word, n))

words_per_album %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = artist_album)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~artist_album, scales = "free") +
  labs(x = "Top 10 words per album",
       y = NULL) +
  theme_minimal()

# plot word clouds for all albums
#https://medium.com/towards-data-science/create-a-word-cloud-with-r-bde3e7422e8a

albums_to_parse <- unique(lyrics_tokens$artist_album)

for (i in 1:length(albums_to_parse)){

layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, albums_to_parse[i])

set.seed(123)
lyrics_tokens %>%
  filter(artist_album == albums_to_parse[i]) %>%
  count(word, sort = TRUE) %>%
  with(wordcloud(word, n, 
                 max.words = 50, 
                 random.order = FALSE
                 #rot.per = 0.35,
          
                 # colors=brewer.pal(8, "Dark2") #do palette per album
                 #random.color = TRUE,
                 #colors = "blue", #palette per album?
                 #ordered.colors = TRUE,
                 ))
}

# cpmpre novels like fig 1.3
frequency <- lyrics_tokens_df %>%
  #arrange(desc(album)) %>%
  select(album, word) %>%
  count(album, word) %>%
  group_by(album) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  pivot_wider(names_from = album, values_from = proportion) %>%
  #select(word, `Number of the Beast`, `The X Factor`, `1989`) %>%
  pivot_longer(`1989`:`Number of the Beast`,
               names_to = "album", values_to = "proportion")



ggplot(frequency, aes(x = proportion, y = `The X Factor`, 
                      color = abs(`The X Factor` - proportion))) +
  theme_minimal() +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  #scale_color_gradient(limits = c(0, 0.001), 
  #                     low = "darkslategray4", high = "gray75") +
  facet_wrap(~album, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "The X Factor", x = NULL)

# quantify that relatioship 
cor.test(data = frequency[frequency$album == "1989",],
         ~ proportion + `The X Factor`) #.0379

# doesn't work with beast

#CH2 sentiments

afinn <- get_sentiments("afinn")
bing <- get_sentiments("bing")
nrc <- get_sentiments("nrc")

# https://rpubs.com/rafrys/723764
# https://www.tidytextmining.com/tidytext
# https://www.azlyrics.com/i/ironmaiden.html

# use nrc to figure out the top 3/5 sentiments of each album

nrc_filtered <- nrc %>%
  filter(sentiment != "positive" & sentiment != "negative")

sent_beast <- beast %>%
  #filter(book == "Emma") %>%
  inner_join(nrc_filtered, 
             relationship = "many-to-many") %>%
  count(sentiment, sort = TRUE) %>%
  mutate(p = n/sum(n)) %>%
  mutate(album = "Number of the Beast")

sent_xfactor <- xfactor %>%
  #filter(book == "Emma") %>%
  inner_join(nrc_filtered,
             relationship = "many-to-many") %>%
  count(sentiment, sort = TRUE) %>%
  mutate(p = n/sum(n)) %>%
  mutate(album = "The X-Factor")

sent_t1989 <- t1989 %>%
  #filter(book == "Emma") %>%
  inner_join(nrc_filtered,
             relationship = "many-to-many") %>%
  count(sentiment, sort = TRUE) %>%
  mutate(p = n/sum(n)) %>%
  mutate(album = "1989")

top_sentiments_by_album <- rbind(sent_beast,
                                 sent_xfactor,
                                 sent_t1989)

# plot side by side, order by sentiment/album
#ggplot(top_sentiments_by_album) +
 # theme_minimal() +
#  geom_col(aes(x = p, y = sentiment, fill = sentiment)) + 
#  facet_wrap(~album)

top_sentiments_by_album %>%
  group_by(album) %>%
  slice_max(n, n = 7, with_ties = FALSE) %>% 
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, p)) %>%
  ggplot(aes(p, sentiment, fill = album)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~album, scales = "free_y") +
  labs(x = "Sentiment by Album",
       y = NULL) +
  theme_minimal()
# fix so they are ordered and cosmetics, but joy vs fear is main.

# so what are the fear words of the 3? vs the joy words?

# filter nrc to fear and joy words only
nrc_fear_joy <- get_sentiments("nrc") %>% 
  filter(sentiment %in% c("fear", "joy"))

# for the 3 albums keep only joy and fear words
tokens_fear_joy <- inner_join(nrc_fear_joy, 
                              lyrics_tokens_df) 

tokens_fear_joy <- tokens_fear_joy %>%
  select(artist, album, word, sentiment)

# for fear, keep top 10 words and plot side by side
top_fear_words <- tokens_fear_joy %>%
  filter(sentiment == "fear") %>%
  group_by(album, word) %>%
  tally()

top_fear_words <- top_fear_words %>%
  group_by(album) %>%
  mutate (p = n/sum(n)) #proportion by album

top_fear_words %>%
  group_by(album) %>%
  slice_max(p, n = 10, with_ties = FALSE) %>% 
  ungroup() %>%
  mutate(word = reorder(word, p)) %>%
  ggplot(aes(p, word, fill = album)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~album, scales = "free_y") +
  labs(x = "Top 10 fear words by album",
       y = NULL) +
  theme_minimal()

# now for joy
# keep top 10 words and plot side by side
top_joy_words <- tokens_fear_joy %>%
  filter(sentiment == "joy") %>%
  group_by(album, word) %>%
  tally()

top_joy_words <- top_joy_words %>%
  group_by(album) %>%
  mutate (p = n/sum(n)) #proportion by album

top_joy_words %>%
  group_by(album) %>%
  slice_max(p, n = 10, with_ties = FALSE) %>% 
  ungroup() %>%
  mutate(word = reorder(word, p)) %>%
  ggplot(aes(p, word, fill = album)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~album, scales = "free_y") +
  labs(x = "Top 10 joy words by album",
       y = NULL) +
  theme_minimal()

# ahora si jodido, plot sentiment as the album plot changes
# by song first, then by line

album_plot_sentiment <- lyrics_tokens_df %>%
  inner_join(bing) %>%
  count(album, song_num, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

ggplot(album_plot_sentiment, aes(song_num, sentiment, fill = album)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~album, ncol = 3, scales = "free_x") +
  theme_minimal()

# now count lines for each song lyric
x <- lyrics_df[2,5]
x
lines <- str_split(x, pattern = "\r\n")
lines

lines_df <- data.frame(lines)
colnames(lines_df) <- "lyric"

lines_df <- lines_df %>%
  #filter(lyric !="") %>%
  mutate(line = row_number())

lines_df$block <- NA
head(lines_df)

# assign parrafo #
empties <- c()
for (i in 1:nrow(lines_df)){
  if (lines_df[i,1] == ""){
    print(i)
    empties <- c(empties, i)
  } 
}
print(empties)

for (i in 1:nrow(lines_df)){
  for (j in 1:length(empties)){
    if(is.na(lines_df[i,3]) && (lines_df[i,2] <= empties[j])){
      lines_df[i,3] <- j
    }
  }
  if(is.na(lines_df[i,3])){
    lines_df[i,3] <- j+1
  }
}  

lines_df <- lines_df %>%
  filter(lyric != "")


#now make a fucntion for all lyrics

#tokenize all lyrics and clean
t <- lyrics_df[1,] %>%
  unnest_tokens(word, lyrics, token = stringr::str_split, pattern = "\r\n") %>%
  anti_join(stop_words)



