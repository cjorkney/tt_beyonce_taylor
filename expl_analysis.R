library(dplyr)
library(tidyr)
library(stringr)








# Import data
tuesdata <- tidytuesdayR::tt_load('2020-09-29')

lyrics_b <- tuesdata$beyonce_lyrics
lyrics_t <- tuesdata$taylor_swift_lyrics

# TS: Tidy data to one word per line

tidy_tay <- lyrics_t %>%
  select(-`Artist `) %>%
  rename(
    title = `Title `,
    lyrics = Lyrics
    ) %>%
  mutate(
    lyrics = str_to_lower(lyrics),
    word = {
      lyrics %>%
        str_squish() %>%
        str_split(' ')
    }
  ) %>% 
  unnest(word) %>%
  select(-lyrics)
         
