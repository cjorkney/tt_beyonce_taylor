library(dplyr)
library(tidyr)
library(stringr)
library(stopwords)
library(ggplot2)
library(forcats)





# Import data
tuesdata <- tidytuesdayR::tt_load('2020-09-29')

lyrics_b <- tuesdata$beyonce_lyrics
lyrics_t <- tuesdata$taylor_swift_lyrics

# TS: Tidy data to one word per line

punct_regex <- '[!,\\.\\:"\\?\\)\\\\]'

tidy_tay <- lyrics_t %>%
  select(-`Artist `) %>%
  rename(
    title = `Title `,
    lyrics = Lyrics,
    album = Album
    ) %>%
  mutate(
    lyrics = str_to_lower(lyrics),
    word = {
      lyrics %>%
        str_squish() %>%
        str_replace_all(punct_regex, "") %>%
        str_split(' ')
    }
  ) %>% 
  unnest(word) %>%
  select(-lyrics)

tidy_tay_redux <- tidy_tay %>%
  filter(!(word %in% stopwords()))

# Explore the words

word_count_simple <- tidy_tay_redux %>%
  count(word) %>%
  arrange(desc(n))

topn <- 20

topn_words <- word_count_simple[1:topn, ]

topn_plot <- topn_words %>%
  ggplot(aes(x = fct_reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "pink", colour = "grey") +
  labs(
    title = "Most frequently used words in Taylor Swift songs",
    x = "Word",
    y = "Frequency"
  ) +
  coord_flip()

topn_plot  

# Split by album

word_count_album <- tidy_tay_redux %>%
  group_by(album) %>%
  count(word) %>%
  arrange(desc(n)) %>%
  ungroup()

topn_by_album <- word_count_album %>%
  group_by(album) %>%
  top_n(topn, n) %>%
  ungroup()

# Make "reorder_within" function?

topn_album_plot <- topn_by_album %>%
  ggplot(aes(x = fct_reorder(word, n), y = n)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Most frequently used words in Taylor Swift songs",
    x = "Word",
    y = "Frequency"
  ) +
  coord_flip() +
  facet_wrap(~ album, scales = "free")

topn_album_plot


