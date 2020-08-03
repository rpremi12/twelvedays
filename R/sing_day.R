#' Takes a noun and makes it plural
#'
#' @param dataset A data frame containing information about gifts
#' @param line The number of the line for the day you want to sing about
#' @param phrase_col The variable name for the column in the dataset that
#' contains the gift phrases
#'
#' @return A string singing the line of the song with all gifts for the given day.
#'
#' @import stringr
#' @import dplyr
#' @import glue
#' @import purrr
#' @import english
#'
#' @export

library(dplyr)
library(glue)
library(purrr)
library(stringr)
library(english )

sing_day <- function(dataset, line, phrase_col){

  phrases <- dataset %>% pull({{phrase_col}})
  phrases <- phrases[1:line]

  if(substr(phrases[1],1,3) !="and"){
    phrases <- rev(phrases)
  }

  nms <- c("first", "second", "third", "fourth", "fifth", "sixth" ,"seventh", "eighth", "ninth",
           "tenth", "eleventh", "twelfth")

  phrases <- phrases %>%
    str_replace("^a", "and a")

  days <- paste(phrases, collapse='\n')
  res <- glue("On the {nms[line]} day of Christmas, my true love sent to me,","\n{days}")
  return(res)

}


