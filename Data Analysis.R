library(corrplot)

## ANALISI CARATTERISTICHE GENERALI GENERI

  ## Creazione data-frame per panoramica generale della musica (divisa per generi)
  
    # Selezione di tutte le canzoni degli artisti del data-frame generale
    # NB: aggiunge automaticamente una colonna con il genere
    audio_features <- all_genre_top_artist %>%
      select(top.song)
    
    # Unnest della colonna top.song a livello superiore 
    audio_features <- unnest(audio_features, top.song)
    
    # Eliminazione di colonne rindondanti (che posso causare anche collisioni con il prossimo unnest)
    audio_features <- audio_features %>%
      select(genre, name, popularity, explicit, song.features)
    
    # Unnest della colonna song.features a livello superiore (data-frame allo stesso livello)
    audio_features <- unnest(audio_features, song.features)
    
    # Creazione nuova colonna duration_s (conversione della colonna duration_ms in secondi)
    audio_features <- audio_features %>%
      mutate(duration_s = map(duration_ms, function(x){x/1000}))
    
    # Assegnazione tipo numerico alla nuova colonna duration_s
    audio_features$duration_s <- as.numeric(audio_features$duration_s)
  
    
  ## Analisi caratteristiche di ogni genere
    
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
  
    
  ## Analisi caratteristiche medie (con istogramma) di ogni genere (solo con 3 features)
    
    # Creazione data-frame per visualizzazione delle caratteristiche generali di ogni genere 
    genre_analysis_average <- audio_features %>%
      group_by(genre) %>%
      nest() %>%
      mutate(av_duration = map(data, function(x){mean(x$duration_s, na.rm = TRUE)})) %>%
      mutate(av_energy = map(data, function(x){mean(x$energy, na.rm = TRUE)})) %>%
      mutate(av_tempo = map(data, function(x){mean(x$tempo, na.rm = TRUE)})) %>%
      mutate(av_danceability = map(data, function(x){mean(x$danceability, na.rm = TRUE)})) %>%
      mutate(av_valence = map(data, function(x){mean(x$valence, na.rm = TRUE)})) %>%
      select(-data, -av_duration, -av_tempo)
      
    # Gathering data-frame (portare le variabili a valori)
    genre_analysis_average <- gather(genre_analysis_average, 'av_energy', 'av_danceability', 'av_valence', 
                                     key = "Characteristic", value = "value")
    
    # Riordinamento data-frame per genere
    genre_analysis_average <- genre_analysis_average %>%
      arrange(genre)
    
    # Assegnazione tipo numerico alla colonna value 
    genre_analysis_average$value <- as.numeric(genre_analysis_average$value)
    
    # Creazione grafico 
    ggplot(genre_analysis_average, aes(fill=Characteristic, y=value, x=genre)) + 
      geom_bar(position="dodge", stat="identity") +
      theme_bw()
  
  
  ## Correlazione tra features che caratterizzano la musica
    
    # Creazione data-frame
    genre_characteristic <- audio_features %>%
      ungroup() %>%
      select(popularity, danceability, energy, key, loudness, mode, speechiness,acousticness, instrumentalness, liveness, valence, tempo, duration_ms)
    
    # Visualizzazione data-frame con corrplot
    corrplot(cor(genre_characteristic, use="pairwise.complete.obs"), 
             method = "color",  
             type = "upper",
             order = "original")
  
  
  
  
## ANALISI SPECIFICA CANZONI/GENERI PIU POPOLARI
    
    # Creazione data-frame con le 200 canzoni più popolari
    top_200_song <- audio_features %>%
      select(popularity, danceability, energy, key, loudness, mode, speechiness,acousticness, 
             instrumentalness, liveness, valence, tempo, duration_s) %>%
      arrange(desc(popularity)) %>%
      head(200)
    
    
  ## Genere delle top 200 canzoni
    
    # Grafico
    top_200_song %>%
      count(genre) %>%
      ggplot() +
      geom_col(aes(x = genre, y = n, fill = genre)) +
      coord_polar() +
      theme_bw() +
      theme(axis.text.x = element_text(hjust = 1), axis.text.y = element_text(hjust = 1)) + 
      ggtitle("Genre of most popular song") + 
      xlab("Song Genre") + 
      ylab("Number of songs")
      
    
  ## Caratteristiche delle canzoni famose
    
    temp3 <- names(top_200_song)[c(3,4,6,8:13)]
    
    top_200_analysis <- top_200_song %>%
      select(c("genre", temp3)) %>%
      pivot_longer(cols = temp3)
    
    # Grafico che mostra le caratteristiche delle canzoni più popolari
    top_200_analysis %>%
      ggplot(aes(x = name, y = value)) +
      geom_jitter(aes(color = genre)) +
      facet_wrap(~name, ncol = 3, scales = 'free') +
      labs(title = 'Audio Feature Pattern Frequency Plots', x = '', y = '') +
      theme_bw()+
      theme(axis.text.y = element_blank(), axis.text.x = element_blank())
  
    
  ## Durata delle canzoni più popolari
    
    # Grafico
    ggplot(data = top_200_song, mapping = aes(x = duration_s)) +
      geom_density(aes(color = genre)) +
      labs(title = 'Duration in sec of most popularity song',x = '', y = '') +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 50, hjust = 1),axis.text.y = element_blank())
    
  
  ## Chiave e modalità delle canzoni più popolari
    
    top_200_song <- top_200_song %>%
      mutate(key_name = map(key, function(x){
        switch (x,
          is.na : NULL,
          is.nan : NULL,
          -1 : as.character("NS", na.rm = TRUE),
          1 : as.character("DO", na.rm = TRUE),
          2 : as.character("Do#", na.rm = TRUE),
          3 : as.character("Re", na.rm = TRUE),
          4 : as.character("Re#", na.rm = TRUE),
          5 : as.character("Mi", na.rm = TRUE),
          6 : as.character("Fa", na.rm = TRUE),
          7 : as.character("Fa#", na.rm = TRUE),
          8 : as.character("Sol", na.rm = TRUE),
          9 : as.character("Sol#", na.rm = TRUE),
          10 : as.character("La", na.rm = TRUE),
          11 : as.character("La#", na.rm = TRUE),
          12 : as.character("Si", na.rm = TRUE)
        )
      }))
    
    # Grafico
    ggplot(data = top_200_song, mapping = aes(x = key, fill = genre)) +
      geom_bar(aes(color = genre), position="dodge") +
      labs(title = 'Key of most popularity song',x = '', y = '') +
      theme_bw() +
      scale_x_continuous(breaks = 0:11, labels = c("C", "C #", "D", "D #", "E", "F", "F #", "G", "G #", "A", "A #", "B")) +
      theme(axis.text.x = element_text(angle = 50, hjust = 1), axis.text.y = element_blank())
    
    
    ggplot(data = top_200_song, mapping = aes(x = key)) +
      geom_density(aes(color = genre)) +
      labs(title = 'Key of most popularity song',x = '', y = '') +
      theme_bw() +
      scale_x_continuous(breaks = 0:11, labels = c("C", "C #", "D", "D #", "E", "F", "F #", "G", "G #", "A", "A #", "B")) +
      theme(axis.text.x = element_text(angle = 50, hjust = 1), axis.text.y = element_blank())
    
    
  ## Confronto generi delle canzoni più popolari con generi a livello generale
    
    # Grafico
    audio_features %>%
      count(genre) %>%
      ggplot() +
      geom_col(aes(x = genre, y = n, fill = genre)) +
      coord_polar() +
      theme_bw() +
      theme(axis.text.x = element_text(hjust = 1), axis.text.y = element_text(hjust = 1)) + 
      ggtitle("Genre of most popular song") + 
      xlab("Song Genre") + 
      ylab("Number of songs")
    
      
    
    
    
    
    
  
  
  
