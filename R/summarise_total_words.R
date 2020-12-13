#' A function to summarise the total number of words used by an artist
#' inputs ...
#' outputs: a summarised dataframe consisting of one row showing the artist and the total number of words
#' 
#' 

summarise_total_words <- function(df) {
  df_new <- df %>%
    group_by(artist) %>%
    summarise(total_words = length(word)) %>%
    ungroup()
  
  return(df_new)
}
