library(dplyr)
library(tidyr)
library(stringr)
library(stopwords)
library(ggplot2)
library(forcats)
library(tidytext)




# Import data
tuesdata <- tidytuesdayR::tt_load('2020-09-29')

lyrics_b <- tuesdata$beyonce_lyrics
lyrics_t <- tuesdata$taylor_swift_lyrics

# TS: Tidy data to one word per line

punct_regex <- '[!,\\.\\:"\\?\\(\\)\\\\]'

extra_stopwords <- c('can', 'know', 'ooh', 'yeah', 'see',
                     'back', "'cause", 'oh-oh', 'oh', 'ha',
                     'say', 'said', 'got', 'tell', 'look')


# Clean and explore Taylor ------------------------------------------------

tidy_tay <- lyrics_t %>%
  rename(
    artist = `Artist `,
    song = `Title `,
    lyrics = Lyrics,
    album = Album
    ) %>%
  mutate(
    artist = str_trim(artist),
    lyrics = str_to_lower(lyrics),
    word = {
      lyrics %>%
        str_squish() %>%
        str_remove_all(punct_regex) %>%
        str_split(' ')
    }
  ) %>% 
  unnest(word) %>%
  select(-lyrics)

tidy_tay_redux <- tidy_tay %>%
  filter(!(word %in% stopwords()),
         !(word %in% extra_stopwords))

# Explore the words

word_count_tay <- tidy_tay_redux %>%
  count(artist, word, sort = TRUE) %>% 
  mutate(prop = n / sum(n))

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


topn_album_plot <- topn_by_album %>%
  ggplot(aes(x = reorder_within(word, n, album),
             y = n)) +
  geom_bar(aes(fill = album),
           stat = "identity") +
  labs(
    title = "Most frequently used words in Taylor Swift songs by album",
    x = "Word",
    y = "Frequency"
  ) +
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(~ album, scales = "free")

topn_album_plot


# Clean and explore Beyonce -----------------------------------------------

tidy_bey <- lyrics_b %>% 
  rename(
    song = song_name,
    artist = artist_name
  ) %>% 
  mutate(
    line = str_to_lower(line),
    word = {
      line %>% 
        str_squish() %>% 
        str_remove_all(punct_regex) %>%
        str_split(' ')
    }
  ) %>%
  unnest(word)

tidy_bey_redux <- tidy_bey %>%
  filter(
    !(word %in% stopwords()),
    !(word %in% extra_stopwords)
    )

word_count_bey <- tidy_bey_redux %>%
  count(artist, word, sort = TRUE) %>%
  mutate(prop = n / sum(n))

topn_bey <- word_count_bey %>%
  top_n(topn, wt = n)

ggplot(topn_bey, aes(x = fct_reorder(word, n), y = n)) +
  geom_bar(stat = 'identity') +
  coord_flip()


# Combine two word count tables and compare -------------------------------

word_count <- rbind(word_count_tay, word_count_bey)

# Plot top_n bar charts side by side

word_count %>%
  group_by(artist) %>% 
  top_n(30, wt = n) %>% 
  ungroup() %>%
  ggplot(aes(x = reorder_within(word, n, artist), y = n)) +
    geom_bar(stat = 'identity',
             aes(fill = artist)) +
    scale_x_reordered() +
    coord_flip() +
    facet_wrap(~ artist, scales = 'free')

unique_words <- word_count %>%
  group_by(artist) %>% 
  summarise(n_unique = n_distinct(word))

ggplot(unique_words, aes(x = artist, y = n_unique)) +
  geom_bar(stat = "identity",
           aes(fill = artist)) +
  labs(
    title = 'Beyonce has used more unique words than Taylor Swift',
    subtitle = 'Excludes stop words',
    x = NULL,
    y = 'Unique word count'
  )

# Scale number of unique words by total number of words ever used?
#   - gives some measure of how often words are repeated
#   - calculate from prop, or sum of n?


