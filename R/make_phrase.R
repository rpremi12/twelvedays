#' Takes a noun and makes it plural
#'
#' @param num An integer
#' @param num_word A string corresponding to the integer
#' @param item A string
#' @param verb A string
#' @param adjective A string
#' @param location A string
#'
#' @return A string containing the words in grammatical order.
#'
#' @import stringr
#' @import glue
#' @import dplyr
#' @import purrr
#' @import english
#' @export

library(dplyr)
library(glue)
library(purrr)
library(stringr)
library(english)

source(here::here("R/pluralize_gift.R"))

is_first <- function(a1, a2){
  if(a1=="a"){
    return(a2)
  }
  else{
    return(pluralize_gift(a2))
  }
}

make_phrase <- function(num, num_word, item, verb, adjective, location){

  verb <- str_replace_na(verb, "")
  adjective <- str_replace_na(adjective, "")
  location <- str_replace_na(location, "")

  num <- str_trim(as.english(num))

  num <- num %>%
    str_replace("one", "a")

  item <- map2_chr(item, num, ~is_first(.y , .x ) )

  ret <- paste(num, adjective, item, verb, location)
  ret <- str_replace_all(ret,"  ", " ")
  ret <- str_replace_all(ret, " $", "")

  return(ret)
}

xmas <- read.csv("https://www.dropbox.com/s/e584pryn8evm1gz/xmas.csv?dl=1")
xmas <- xmas %>%
  mutate(
    Full.Phrase = make_phrase(xmas$Day, xmas$Day.in.Words, xmas$Gift.Item, xmas$Verb, xmas$Adjective, xmas$Location)
  ) %>%
  arrange(desc(Day))

print(xmas$Full.Phrase)



