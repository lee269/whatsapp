library(rwhatsapp)
library(here)
library(dplyr)
library(ggplot2)
library(forcats)
library(stringr)
library(ggimage)
library(tidyr)
library(tidytext)
library(lubridate)

# https://medium.com/analytics-vidhya/how-i-analyzed-whatsapp-chat-in-r-using-rwhatsapp-and-ggplot-912ba9439026
# https://levelup.gitconnected.com/text-and-sentiment-analysis-of-whatsapp-messages-1eebc983a58

chat <- rwa_read(here("data", "raw", "WhatsApp Chat - Dream Team.zip"))

chat <- chat %>% 
  mutate(media = str_detect(text, "image omitted") | str_detect(text, "video omitted"),
         link = str_detect(text,"www.")| str_detect(text,"http:")|str_detect(text,"https:")|str_detect(text,"youtu.be"))


# filter(media == TRUE) %>% 
#   count(author, text)

chat %>%
  filter(author != "Dream Team") %>%
  count(author) %>% 
  mutate(author = fct_reorder(author, n, .desc = TRUE)) %>% 
  ggplot() +
  geom_col(aes(x = author, y = n), fill = "green") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  labs(title = "Top posters",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())


emoji_data <- rwhatsapp::emojis %>% # data built into package
  mutate(hex_runes1 = gsub("\\s[[:alnum:]]+", "", hex_runes)) %>% # ignore combined emojis
  mutate(emoji_url = paste0("https://abs.twimg.com/emoji/v2/72x72/", 
                            tolower(hex_runes1), ".png"))

chat %>% 
  select(author, emoji) %>% 
  tidyr::unnest(emoji) %>% 
  count(author, emoji,sort = TRUE) %>% 
  group_by(author) %>% 
  top_n(n = 5, n) %>%
  left_join(emoji_data) %>% 
  ggplot() +
  geom_col(aes(x = reorder_within(emoji, n, author), y = n), fill = "green") +
  scale_x_reordered() +
  facet_wrap(vars(author),scales = "free_x") +
  labs(title = "Top emojis",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none")

var<-chat %>% mutate(date = date(time)) %>% count(date) %>% top_n(1)
title<-paste0("Most Active day was ",var %>% pull(date),"\n with ",var %>% pull(n)," messages")

chat %>%
  mutate(day = date(time)) %>%
  count(day) %>%
  ggplot(aes(x = day, y = n)) +
  geom_bar(stat = "identity", fill="red") +
  xlab("Messages across time") + ylab("") +
  ggtitle(title)+
  theme(axis.text.x = element_text(color = "grey20", size = 13, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 9, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        plot.title = element_text(color = "grey20", size = 17, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "grey20", size = 15, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 13, angle = 90, hjust = .5, vjust = .5, face = "plain")) 





chat_text <- chat %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  count(word, sort = TRUE)

wordcloud::wordcloud(words = chat_text$word,freq = chat_text$n,min.freq = 5)
