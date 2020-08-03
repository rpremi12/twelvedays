#' Takes a noun and makes it plural
#'
#' @param gift A string or vector of strings
#'
#' @return A string or vector of strings with the pluralized words
#'
#' @import stringr
#' @import dplyr
#' @import glue
#' @import purrr
#'
#' @export
#'
#'

library(dplyr)
library(glue)
library(purrr)
library(stringr)

pluralize_gift <- function(gift){


gift <- gift %>%
  str_replace("s$", "ses") %>%
  str_replace("o$", "oes") %>%
  str_replace("x$", "xes") %>%
  str_replace("z$", "zes") %>%
  str_replace("[iy]$", "ies") %>%
  str_replace("ch$", "ches") %>%
  str_replace("sh$", "shes") %>%
  str_replace("fe$", "ves") %>%
  str_replace("oot", "eet") %>%
  str_replace("eaf$", "eaves")


gift <- gift %>%
  str_replace("e$", "es") %>%
  str_replace("n$", "ns") %>%
  str_replace("d$", "ds") %>%
  str_replace("g$", "gs") %>%
  str_replace("r$", "rs") %>%
  str_replace("t$", "ts")

gift <- gift %>%
  str_replace("ooses$", "eese") %>%
  str_replace("lf$", "lves") %>%
  str_replace("ief$", "ieves") %>%
  str_replace("af$", "aves") %>%
  str_replace("ato$", "atoes") %>%
  str_replace("eus$", "ei") %>%
  str_replace("us$", "i") %>%
  str_replace("mans$", "men")

return(gift)

}
# xmas <- read.csv("https://www.dropbox.com/s/e584pryn8evm1gz/xmas.csv?dl=1")
# print(xmas$Gift.Item)
# test1 <- c("boat", "house", "cat", "river",
#           "bus", "wish", "pitch", "box" ,"fizz",
#           "penny", "baby","tooth", "leaf", "goose", "half", "knife",
#           "life","elf", "loaf", "potato", "focus", "thief","tornado", "woman")
# print(pluralize_gift(xmas$Gift.Item))
# print(pluralize_gift(test1))
