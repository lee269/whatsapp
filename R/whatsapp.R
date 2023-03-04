library(rwhatsapp)
library(here)
library(dplyr)
library(ggplot2)
library(forcats)
library(stringr)
library(ggimage)
library(tidyr)
library(tidytext)

# https://medium.com/analytics-vidhya/how-i-analyzed-whatsapp-chat-in-r-using-rwhatsapp-and-ggplot-912ba9439026
# https://levelup.gitconnected.com/text-and-sentiment-analysis-of-whatsapp-messages-1eebc983a58

chat <- rwa_read(here("data", "raw", "WhatsApp Chat - Dream Team.zip"))

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
