# Carregar pacote - O rtweet é recomendado
library(rtweet)
# plotting and pipes - tidyverse!
library(ggplot2)
library(dplyr)
# text mining library
library(tidytext)
# plotting packages
library(igraph)
library(ggraph)
library(lubridate)

cacho<- search_tweets("#todecacho", n = 18000,
                     lang = "pt",
                     include_rts = FALSE)

transicao<-search_tweets("#transicaocapilar", n = 18000,
                     lang = "pt",
                     include_rts = FALSE)


## plot time series of tweets of cacho
cacho %>%
  ts_plot("1 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequência de '#Tôdecacho' no Twitter",
    #subtitle = "Status no Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )


## plot time series de tweets da cacho
transicao %>%
  ts_plot("1 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequência de '#transicaocapilar' no Twitter",
    subtitle = "Status no Twitter",
    caption = "\nFonte: Dados Coletados da Twitter's REST API via rtweet"
  )


## plot time series de tweets da transicao
transicao %>%
  ts_plot("3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequência de 'transicao' no Twitter",
    subtitle = "Status no Twitter",
    caption = "\nFonte: Dados Coletados da Twitter's REST API via rtweet"
)



## get user IDs de contas 
tmls <- get_timelines(c("SalonLineBrasil" ), n = 10000)

## plot the frequencia de tweets para cada usuário 
tmls %>%
  dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequência de postagens da conta @SalonLineBrasil",
    subtitle = "Twitter status (tweet)",
    caption = "\nFonte: Dados Coletados da Twitter's REST API via rtweet"
)


    
# Juntar bases. Só faz sentido em alguns casos.
base_unica <- bind_rows(transicao %>% 
                        mutate(marca = "transicao"), 
                        cacho %>% 
                        mutate(marca = "cacho"))



base_unica %>%
  dplyr::group_by(`marca`) %>%
  ts_plot("3 hours", trim = 1L)+
  ggplot2::geom_point()+
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(x = NULL, y = NULL,
    title = "Frequência de Tweets associados à #todecacho e #transicaocapilar",
    subtitle = "Variação temporal organizada por dias",
    caption = "\nFonte: Dados coletados pela API do Twitter via rtweet")




#Distribuição temporal dos tweets
ggplot(base_unica, aes(x = created_at, fill = marca)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  facet_wrap(~marca, ncol = 1)

#Frequencia de palavras 

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
  group_by(marca) %>% 
  count(word, sort = TRUE) %>% 
  left_join(base_limpa %>% 
              group_by(marca) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

frequencia

#remodelar os dados
library(tidyr)

frequencia <- frequencia %>% 
  select(marca, word, freq) %>% 
  spread(marca, freq) %>%
  arrange(cacho, transicao)

frequencia

# Hora de plotar 

library(scales)

ggplot(frequencia, aes(cacho, transicao)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")


#Comparar o uso de palavras 

base_limpa_2 <- base_limpa %>%
  filter(created_at >= as.Date("2016-01-01"),
         created_at < as.Date("2017-01-01"))

uso_palavras <- base_limpa %>%
  filter(!str_detect(word, "^@")) %>%
  count(word, marca) %>%
  group_by(word) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  spread(marca, n, fill = 0) %>%
  mutate_if(is.numeric, list(~(. + 1) / (sum(.) + 1))) %>%
  mutate(logratio = log(cacho / transicao)) %>%
  arrange(desc(logratio))


uso_palavras %>% 
  arrange(abs(logratio))

#Plotar palavras mais usadas por cada grupo
uso_palavras %>%
  group_by(logratio < 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  ylab("Taxa log odds (cacho/transicao)") +
  scale_fill_discrete(name = "", labels = c("cacho", "transicao"))

# Voltar a usar uma única base #Fazer a limpeza
library(tidytext)

head(cacho$text)


# remove elementos e limpa 
cacho$stripped_text <- gsub("http.*","",  cacho$text)

cacho$stripped_text <- gsub("https.*","", cacho$stripped_text)

cacho_clean <- cacho %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

# Limpar stopwords
cacho_clean <- cacho_clean %>%
  anti_join(get_stopwords(language = "pt"))

#Plota palavras únicas

cacho_clean %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Palavras únicas",
       title = "Contagem de Palavras únicas tuitadas sobre #todecacho",
       subtitle = "Gráfico com stopwords removidas")


# Verificar par de palavras 

cacho_pair <- cacho %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(paired_words, stripped_text, token = "ngrams", n = 2)

cacho_pair %>%
  count(paired_words, sort = TRUE)

#carregar pacote
library(tidyr)

cacho_separar_palavras <- cacho_pair %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

cacho_filtro <- cacho_separar_palavras %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# Nova contagem de bigramns 
cacho_freq <- cacho_filtro %>%
  count(word1, word2, sort = TRUE)

head(cacho_freq)

library(igraph)
library(ggraph)

# (Plotagem de pontes esta quebrada)
cacho_freq %>%
  filter(n >= 3) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n))+
  #geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "#todecacho: pares de palavras associadas em Tweets",
       subtitle = "Indica a frequência de uso associado das palavras",
       x = "", y = "")


##


salon_fav<- get_favorites("SalonLineBrasil", n = 3000)
ts_plot(salon_fav, "1 days")

salon_fav_2<-salon_fav %>% 
  select(text, favorite_count, retweet_count) %>% 
  arrange(desc(favorite_count))

write_as_csv(salon_fav_2, "salon_fav")

cacho_fav<-cacho %>% 
  select(text, favorite_count, retweet_count) %>% 
  arrange(desc(favorite_count))

write_as_csv(cacho_fav, "cacho_fav")

tran_fav<-transicao %>% 
  select(text, favorite_count, retweet_count) %>% 
  arrange(desc(favorite_count))

write_as_csv(tran_fav, "tran_fav")


salon_fav %>% 
  arrange(-retweet_count) %>%
  slice(1) %>% 
  select(created_at, screen_name, text, retweet_count)

salon_fav %>% 
  count(screen_name, sort = TRUE) %>%
  top_n(10) %>%
  mutate(screen_name = paste0("@", screen_name))


cacho %>% 
  unnest_tokens(hashtag, text, "tweets", to_lower = FALSE) %>%
  filter(str_detect(hashtag, "^#"),
         hashtag != "#ClimateEmergency") %>%
  count(hashtag, sort = TRUE) %>%
  top_n(10)


words <- cacho %>%
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

library(wordcloud)

words2 = words[-5,]

words2 %>% 
  with(wordcloud(word, n, random.order = FALSE, max.words = 100, colors = "#F29545"))



