# Carregar pacote rtweet para acesso aos dados no Twitter
library(rtweet)
# Bibliotecas para manipular dados e criar gráficos
library(ggplot2)
library(dplyr)
# Biblioteca para textmine
library(tidytext)
# Pacote para criar grafos
library(igraph)
library(ggraph)
# Pacote para manipular datas
library(lubridate)

# Vamos buscar 2000 publicacoes no Twitter sobre Visa e Mastercard. 
# Estamos considerando o aumento das publicações pela suspensão das operacões na Rússia em 05 de março de 2022

visa<- search_tweets("#visa", n = 2000,
                     lang = "en",
                     include_rts = FALSE)

master<-search_tweets("#mastercard", n = 2000,
                     lang = "en",
                     include_rts = FALSE)


# Vamos salvar os dados extraídos do Twitter

save(master, visa, file = "tweets.RData")

#load("tweets.RData") 

## Serie Temporal com postagens sobre #visa
visa %>%
  ts_plot("1 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequência de '#visa' no Twitter",
    subtitle = "Aumento das postagens a partir da suspensão das atividades na Rússia",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )


## Serie temporal de tweets sobre #mastercard

master %>%
  ts_plot("1 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequência de '#mastercard' no Twitter",
    subtitle = "Aumento das postagens a partir da suspensão das atividades na Rússia",
    caption = "\nFonte: Dados Coletados da Twitter's REST API via rtweet"
  )



# Quem está publicando sobre

head(visa$screen_name, 5)


# Juntar bases para analise

base_unica <- bind_rows(visa %>% 
                        mutate(empresa = "visa"), 
                        master %>% 
                        mutate(empresa = "master"))

save (base_unica, file = "base_unica.RDATA")

#### Agora série temporal com dados das duas operadoras. 

base_unica %>%
  dplyr::group_by(`empresa`) %>%
  ts_plot("3 hours", trim = 1L)+
  ggplot2::theme_minimal()+
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(x = "Dias", y = "Frequência", title = "Frequência de Tweets associados às operadoras #mastercard e #visa",
    subtitle = "Variação temporal organizada por dias",
    colour = "Empresa", caption = "\nFonte: Dados coletados pela API do Twitter via rtweet")



### Distribuição temporal dos tweets (agora apresentada em gráfico de barras)
ggplot(base_unica, aes(x = created_at, fill = empresa)) +
  geom_histogram(position = "identity", #bins = 20,
                 show.legend = FALSE) +
  facet_wrap(~empresa, ncol = 1)+
  ggplot2::labs(x = "Dias", y = "Frequência", title = "Frequência de Tweets associados às operadoras #mastercard e #visa",
                subtitle = "Variação temporal organizada por dias",
                colour = "Empresa", caption = "\nFonte: Dados coletados pela API do Twitter via rtweet")


#Vamos agora verificar a frequencia de palavras nos tweets

library(tidytext)
library(stringr)

remove_reg <- "&amp;|&lt;|&gt;"

base_limpa <- base_unica %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"))

frequencia <- base_limpa %>% 
  group_by(empresa) %>% 
  count(word, sort = TRUE) %>% 
  left_join(base_limpa %>% 
              group_by(empresa) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)


# Vamos imprimir os 10 mais frequentes
head(frequencia,10)


#remodelar os dados
library(tidyr)

frequencia_tidy <- frequencia %>% 
  select(empresa, word, freq) %>% 
  spread(empresa, freq) %>%
  arrange(visa, master)

head(frequencia_tidy,5)

# Hora de plotar a base para comparar 

library(scales)

ggplot(frequencia_tidy, aes(visa, master)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "blue")+
  theme_bw()+
  ggplot2::labs(x = "Visa", y = "Mastercard", title = "Frequência de Tweets associados às operadoras #mastercard e #visa",
                subtitle = "Comparação de uso de palavras em cada hashtag",
                caption = "\nFonte: Dados coletados pela API do Twitter via rtweet")


# As palavras próximas à linha azul indicam o uso pelas duas tags. Quanto mais afastada da linha, mais exclusivo o uso. 

#Comparar o uso de palavras associadas à cada operadora

uso_palavras <- base_limpa %>%
  filter(!str_detect(word, "^@")) %>%
  count(word, empresa) %>%
  group_by(word) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  spread(empresa, n, fill = 0) %>%
  mutate_if(is.numeric, list(~(. + 1) / (sum(.) + 1))) %>%
  mutate(logratio = log(visa / master)) %>%
  arrange(desc(logratio))


uso_palavras %>% 
  arrange(abs(logratio))

#Plotar palavras mais usadas 
uso_palavras %>%
  group_by(logratio < 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  ylab("Taxa log odds (visa/master)") +
  scale_fill_discrete(name = "", labels = c("visa", "master"))



#### Voltar a usar os dados de cada operadora. Vamos fazer apenas com uma.
library(tidytext)

# remove elementos e limpa 
master$stripped_text <- gsub("http.*","",  master$text)

master$stripped_text <- gsub("https.*","", master$stripped_text)

master_clean <- master %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

# Limpar stopwords
master_clean <- master_clean %>%
  anti_join(get_stopwords(language = "en"))

#Plota palavras únicas

master_clean %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Frequência",
       x = "Palavras únicas",
       title = "Contagem de palavras únicas tuitadas sobre #mastercard",
       subtitle = "As palavras se relacionam diretamente ao confito bélico")+
  theme_bw()


# Verificar par de palavras 

master_pair <- master %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(paired_words, stripped_text, token = "ngrams", n = 2)

master_pair %>%
  count(paired_words, sort = TRUE)

#carregar pacote
library(tidyr)

master_separar_palavras <- master_pair %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

master_filtro <- master_separar_palavras %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# Nova contagem de bigramns 
master_freq <- master_filtro %>%
  count(word1, word2, sort = TRUE)

head(master_freq)

library(igraph)
library(ggraph)

# Plotagem
master_freq %>%
  filter(n >= 20) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n))+
  #geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Pares de palavras associadas à Mastercard no contexto bélico",
       subtitle = "Indica a frequência de uso associado das palavras",
       x = "", y = "", colour = "Freq")+
  theme_bw()

# Verificar favoritos e retweets

master_fav<-master %>% 
  select(created_at, screen_name, text, favorite_count, retweet_count) %>% 
  arrange(desc(favorite_count))

visa_fav<-visa %>% 
  select(text, favorite_count, retweet_count) %>% 
  arrange(desc(favorite_count))

master %>% 
  arrange(-favorite_count) %>%
  slice(1) %>% 
  select(created_at, screen_name, text, retweet_count,favorite_count)

visa %>% 
  arrange(-retweet_count) %>%
  slice(1) %>% 
  select(created_at, screen_name, text, retweet_count)

master_fav %>% 
  count(screen_name, sort = TRUE) %>%
  top_n(10) %>%
  mutate(screen_name = paste0("@", screen_name))

# Verificar hashtags associadas

master %>% 
  unnest_tokens(hashtag, text, "tweets", to_lower = FALSE) %>%
  filter(str_detect(hashtag, "^#"),
         hashtag != "#ClimateEmergency") %>%
  count(hashtag, sort = TRUE) %>%
  top_n(10)

#### Vamos agora criar uma nuvem de palavras #####

words <- master %>%
  mutate(text = str_remove_all(text, "&amp;|&lt;|&gt;"),
         text = str_remove_all(text, "\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)"),
         text = str_remove_all(text, "[^\x01-\x7F]")) %>% 
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"),
         !str_detect(word, "^#"),         
         !str_detect(word, "@\\S+")) %>%
  count(word, sort = TRUE)

library(wordcloud2)

# words2 = words[-5,] caso queira eliminar palavras iniciais

wordcloud2(words, color='random-dark')


### Analise de sentimento 

senti_words <- words %>%
  inner_join(get_sentiments("bing")) %>%
  ungroup()


senti_words %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  theme_bw()+
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Palabras positivas e negativas presentes nos tweets",
       y = NULL)


library(reshape2)

senti_words %>%
  inner_join(get_sentiments("bing")) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#E8451A", "#076147"),
                   max.words = 500, scale=c(5,.4), 
                   title.colors=c("red","#076147"))



save.image(file = "mastercard_files.RData")

