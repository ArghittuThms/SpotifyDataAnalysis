---
title: "Spotify Data Analysis: la musica al giorno d'oggi"
author: Thomas Arghittu - 157802 - arghittu.thomas@spes.uniud.it
date: 24/08/2022
output:
  prettydoc::html_pretty:
    theme: tactile
    highlight: github
    toc: true # table of content true
    toc_depth: 3  # upto three depths of headings (specified by #, ## and ###)
    number_sections: false  ## if you want number sections at each table header
---
<style>
  * {
  text-align: justify;
}

  .title, .author, .date  {
  text-align: center;
}
</style>
<script>
  // Move TOC to the Table of Contents heading (with id "table-of-contents")
  $(function() {
    $( "#TOC" ).insertAfter( $( "#table-of-contents" ) );
  });
</script>



<p>
  <a>Spotify</a>, il servizio di streaming musicale offerto dall'omonimo colosso svedese, ha raggiunto lo scorso febbraio 406 milioni di utenti attivi
  mensili diventando così uno dei servizi online più popolari di sempre: d'altra parte, chi non ne ha mai sentito parlare? Il suo algoritmo è in grado
  di consigliare brani e podcast per tutti i gusti nei minimi dettagli, grazie alla grande quantità di variabili che si celano dietro ogni canzone o
  artista.
</p>
<p>
  Negli ultimi anni sono diventato un grande ascoltatore e, sebbene abbia scoperto il fascino della collezione di vinili, Spotify rimane sempre un mio
  alleato quando mi sposto fuori di casa. <br>
  Buttandomi anche nel mondo della produzione musicale, in maniera molto amatoriale, mi sono sempre chiesto quali fossero gli ingredienti per creare una
  canzone di successo: questa analisi dei dati quindi, punta a rispondere a questo quesito utilizzando le informazioni provenienti dalle APIs di 
  Spotify.
</p>
<p>
  Per poter affrontare l'argomento, ho deciso di utilizzare un approccio up-to-down, partendo dagli aspetti più generali per poi andare a restringere 
  sempre di più il campo fino ad andare ad analizzare le 10 canzoni e i 10 artisti più in voga al momento a livello mondiale.
</p>


# Librerie richieste

<p>
  Lo studio è stato svolto usando il linguaggio R e RStudio. Le librerie utilizzate sono:
</p>


```r
library(dplyr)      # Manipolazione dei dati
library(ggplot2)    # Creazione dei grafici
library(purrr)      # Toolkit che aggiunge funzionalità ad R
library(modelr)     # Manipolazione dei dati
library(broom)      # Converte data-frame in strutture dati tipo tibbles
library(tidyr)      # Manipolazione dei dati
library(spotifyr)   # Metodi per utilizzo delle APIs Spotify
library(corrplot)   # Grafici di correlazione
library(DT)         # Stampa tabelle su html
```



# Preparazione dei dati

## Sorgente dati e APIs

<p>
  In internet esistono diversi data-set messi a disposizione e basati sulle informazioni provenienti da Spotify. Tuttavia, ho scelto di utilizzare le
  APIs messe a disposizione proprio dalla piattaforma per creare il mio data-set personale.
</p>
<p>
  Le APIs sono delle interfacce con le quali i data-scientist possono ottenere dati messi a disposizioni da enti, aziende o in generale altri
  programmatori. Normalmente sono a pagamento (ovvero chi le mette a disposizione può avere un ricavo) o open-source: per utilizzare quelle di Spotify 
  è sufficiente avere un account e ottenere un token da utilizzare poi nelle richieste tramite APIs.
</p>


```r
# Identificatico account Spotify
Sys.setenv(SPOTIFY_CLIENT_ID = '******************************')
# Codice personale per utilizzo APIs
Sys.setenv(SPOTIFY_CLIENT_SECRET = '*******************************')      

# Restituisce il token
access_token <- get_spotify_access_token()                                  
```



## Struttura del data-set

<p>
  Il data-set è composto dai maggiori artisti di sei generi diversi che ho deciso di prendere in considerazione: il Rock, il Metal, il Pop, 
  l'Hip-Hop, la musica latina e quella elettronica.
</p>
<p>
  Per ottenere i migliori artisti di ogni genere (dove per migliori si intende quelli con un valore di popolarità assegnato da Spotify che va da 0 a 
  100 maggiore di 60):
</p>


```r
# Funzione per ottenere i top artisti di ogni genere (con le top canzoni di ogni artista)
get_db <- function(genre) {
  
  # Richiesta a Spotify degli artisti di un determinato genere
  # Il limite massimo di artisti a richiesta è 50, 
  # quindi è necessario usare un ciclo per mandare più richieste
  temp <- get_genre_artists(genre, limit = 50)
  
  a = 50;
  while (a < 1000) {
    temp <- rbind(temp, get_genre_artists(genre, limit = 50, offset = a))
    a = a + 50
  }
  
  # Filtro per mantenere solo gli artisti con popolarità maggiore di 60
  temp <- temp %>%
    filter(popularity > 60)
  
  # Per ogni artista presente, si chiede a Spotify le 10 canzoni più famose
  temp <- temp %>%
    mutate(top.song = map(id, function(id){get_artist_top_tracks(id)}))
  
  # Per ogni canzone, si chiede a Spotify le caratteristiche
  for (i in 1:nrow(temp)) {
    temp$top.song[[i]] <- temp$top.song[[i]] %>%
      mutate(song.features = map(temp$top.song[[i]][[6]], 
                                 function(id){get_track_audio_features(id)}))
  }
  
  # Pulizia del data-set 
  temp <- temp %>%
    select(-images, -href, -uri, -external_urls.spotify, -followers.href)
  
  for (i in 1:nrow(temp)) {
    temp$top.song[[i]] <- temp$top.song[[i]] %>%
      select(-album.release_date_precision, -artists, -disc_number, -href, -is_local, 
             -is_playable, -preview_url, -type, -uri, -album.album_type, -album.artists, 
             -album.href, -album.images,  -album.uri, -album.external_urls.spotify, 
             -external_urls.spotify)
  }
  
  return(temp)
}

# Getting dei dataset
rock_top_artist <- get_db('classic_rock')
metal_top_artist <- get_db('metal')
pop_top_artist <- get_db('pop')
hip_hop_top_artist <- get_db('rap')
latin_top_artist <- get_db('latin')
electronic_top_artist <- get_db('electronic')

# Unico dataset
all_genre_top_artist <- data.frame() %>%
  rbind(pop_top_artist, hip_hop_top_artist, rock_top_artist, metal_top_artist, 
        latin_top_artist, electronic_top_artist) %>%
  group_by(genre)
```

<p>
  Il data-set così ottenuto è composto da:
  
* 1834 Artisti (di 6 generi diversi)
* Ciascun artista possiede 10 canzoni con le rispettive specifiche (per un totale di 18340 canzoni)
</p>



## Sommario delle variabili

Nome variabile    | Descrizione
-------------     | -------------
id                | Identificativo unico utilizzato da Spotify per individuare canzoni o artisti
name              | Nome canzone o artista
popularity        | Indice che identifica popolarità di una canzone o artista (Range 0:100)
genre             | Genere dell'artista
top.song          | Data-frame conytenente le migliori 10 canzoni di ogni artista
                  |
explicit          | Identifica il contenuto della canzone in esplicito o meno
duration_ms       | Durata della canzone (in milli-secondi)
danceability      | Descrive quanto una canzone è adatta per essere ballata, 0.0 poco ballabile e 1.0 molto ballabile
energy            | Quantità di energia che una canzone è in grado di far percepire
key               | Tonalità stimata della traccia. Valori da 1 a 12 (C, C#, D, D#, E, F, F#, G, G#, A, A#, B)
mode              | Modalità della traccia (maggiore o minore)
loudness          | Volume complessivo di una traccia (in DB)
speechiness       | Presenza di parola pronunciate nella traccia
acousticness      | Valore che indica quanto la traccia sia acustica. 1.0 indica che è acustica al 100%
instrumentalness  | Prevede se una traccia non contiene voci. 1.0 indica che la traccia non contiene parole
liveness          | Prevede la presenza di un pubblico nella traccia (in modo di capire se la canzone è stata registrata dal vivo)
valence           | Positività del brano. valori tra 0.0 e 1.0
tempo             | Tempo complessivo stimato del brano (misurato in BPM)


# Analisi generale

## Caratteristiche di ogni genere


```r
    audio_features <- all_genre_top_artist %>%
      select(top.song)
    
    # Unnest della colonna top.song a livello superiore 
    audio_features <- unnest(audio_features, top.song)
    
    # Eliminazione di colonne rindondanti (che posso causare anche collisioni con il 
    # prossimo unnest)
    audio_features <- audio_features %>%
      select(genre, name, popularity, explicit, song.features)
    
    # Unnest della colonna song.features a livello superiore (data-frame allo stesso livello)
    audio_features <- unnest(audio_features, song.features)
    
    # Creazione nuova colonna duration_s (conversione della colonna duration_ms in secondi)
    audio_features <- audio_features %>%
      mutate(duration_s = map(duration_ms, function(x){x/1000}))
    
    # Assegnazione tipo numerico alla nuova colonna duration_s
    audio_features$duration_s <- as.numeric(audio_features$duration_s)
  
    
  # Analisi caratteristiche di ogni genere
    
    # Creazione data-frame per visualizzazione delle caratteristiche di ogni genere
    temp <- names(audio_features)[c(5, 6, 8, 10:15)]
    
    genre_analysis <- audio_features %>%
      select(c("genre", temp)) %>%
      pivot_longer(cols = temp, values_transform = as.numeric)
    
    # Creazione grafico   
    genre_analysis %>%
      ggplot(aes(x = value)) +
      geom_density(aes(color = genre)) +
      facet_wrap(~name, ncol = 3, scales = 'free') +
      labs(title = 'Genre Characteristic',x = '', y = '') +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 50, hjust = 1),axis.text.y = element_blank())
```

![plot of chunk unnamed-chunk-31](figure/unnamed-chunk-31-1.png)


















