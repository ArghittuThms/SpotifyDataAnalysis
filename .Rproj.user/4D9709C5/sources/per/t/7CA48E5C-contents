library(dplyr)
library(ggplot2)
library(purrr)
library(modelr)
library(broom)
library(tidyr)
library(spotifyr)

devtools::install_github('charlie86/spotifyr')

Sys.setenv(SPOTIFY_CLIENT_ID = '4c0f54d6d78649bc871e2078d15d0b4e')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'b9d8dff917b047df9cd9882703a8d77a')

access_token <- get_spotify_access_token()


## ROCK ------------------------------------------------------------------------
## Get artist dataset
rock_artists <- get_genre_artists('classic_rock', limit = 50)

## rock_artists <- rbind(rock_artists, get_genre_artists('classic_rock', limit = 50, offset = 250)) ## Da scrivere meglio

## Get all rock artists
a = 50;
while (a < 1000) {
  rock_artists <- rbind(rock_artists, get_genre_artists('classic_rock', limit = 50, offset = a))
  
  a = a + 50
}

## Filter most popular artist
rock_top_artist <- rock_artists %>%
  filter(popularity > 60)

## Get top song of each top artist
rock_top_artist <- rock_top_artist %>%
  mutate(top.song = map(id, get_top_song))


rock_top_artist <- rock_top_artist %>%
  cbind(features = map(.x = top.song, .f = ~get_track_audio_features(.x$id)))

for (i in 1:nrow(rock_top_artist)) {
  rock_top_artist$top.song[[i]] <- rock_top_artist$top.song[[i]] %>%
    mutate(song.features = map(rock_top_artist$top.song[[i]][[6]], get_song_features))
}

rock_top_artist$top.song[[1]] <- rock_top_artist$top.song[[1]] %>%
  mutate(song.features = map(rock_top_artist$top.song[[1]][[6]], get_song_features))

prova = get_track_audio_features("6dGnYIeXmHdcikdzNNDMm2")

# Clearing dataframe
rock_top_artist <- rock_top_artist %>%
  select(-images, -href, -uri, -external_urls.spotify, -followers.href)

for (i in 1:nrow(rock_top_artist)) {
  rock_top_artist$top.song[[i]] <- rock_top_artist$top.song[[i]] %>%
    select(-album.release_date_precision, -artists, -disc_number, -href, -is_local, -is_playable, -preview_url, -type, -uri, -album.album_type, 
           -album.artists, -album.href, -album.images,  -album.uri, -album.external_urls.spotify, -external_urls.spotify)
}

## Get all datasets
metal_top_artist <- get_db('metal')
pop_top_artist <- get_db('pop')
hip_hop_top_artist <- get_db('rap')
latin_top_artist <- get_db('latin')
electronic_top_artist <- get_db('electronic')

## Manage pop dataset

prova <- anti_join(pop_top_artist, hip_hop_top_artist, "id")    # Correct

pop_top_artist <- anti_join(pop_top_artist, hip_hop_top_artist, "id")

## Put all dataset toghether

all_genre_top_artist <- data.frame() %>%
  rbind(pop_top_artist, hip_hop_top_artist, rock_top_artist, metal_top_artist, latin_top_artist, electronic_top_artist) %>%
  group_by(genre) %>%
  summarise(genre)


## GLOBAL FUNCTION -------------------------------------------------------------

## GET GENRE TOP ARTIST WITH TOP SONG WITH AUDUIO FEAUTURES
get_db <- function(genre) {
  
  # Get the entire db of artist about a specific genre
  temp <- get_genre_artists(genre, limit = 50)
  
  a = 50;
  while (a < 1000) {
    temp <- rbind(temp, get_genre_artists(genre, limit = 50, offset = a))
    
    a = a + 50
  }
  
  # Filter most popular artist
  temp <- temp %>%
    filter(popularity > 60)
  
  # Get top song of each top artist
  temp <- temp %>%
    mutate(top.song = map(id, get_top_song))
  
  # Get the features of each top song of all the artists
  for (i in 1:nrow(temp)) {
    temp$top.song[[i]] <- temp$top.song[[i]] %>%
      mutate(song.features = map(temp$top.song[[i]][[6]], get_song_features))
  }
  
  # Clearing dataframe
  temp <- temp %>%
    select(-images, -href, -uri, -external_urls.spotify, -followers.href)
  
  for (i in 1:nrow(temp)) {
    temp$top.song[[i]] <- temp$top.song[[i]] %>%
      select(-album.release_date_precision, -artists, -disc_number, -href, -is_local, -is_playable, -preview_url, -type, -uri, -album.album_type, 
             -album.artists, -album.href, -album.images,  -album.uri, -album.external_urls.spotify, -external_urls.spotify)
  }
  
  
  return(temp)
}

## Get all artists of a genre
get_all_artist <- function(genre){
  ## ...
}

## Get top song of all most popular artist
get_top_song <- function(id) {
  get_artist_top_tracks(id)
}

## Get the features of a song
get_song_features <- function(id) {
  get_track_audio_features(id)
}

