library(testthat)
library(devtools)
library(twelvedays)

test_check("twelvedays")

test_that("Testing pluralize_gift", {

  test1 <- c("boat", "house", "cat", "river",
             "bus", "wish", "pitch", "box" ,"fizz",
             "penny", "baby","tooth", "leaf", "goose", "half", "knife",
             "life","elf", "loaf", "potato", "focus", "thief","tornado", "woman")
  answer <- c( "boats" ,    "houses"  ,  "cats"   ,   "rivers"   ,
 "buses"   ,  "wishes" ,   "pitches" ,  "boxes"  , "fizzes" ,  "pennies"  , "babies" ,   "teeth",
"leaves",    "geese" ,    "halves",    "knives"   ,
 "lives" ,    "elves"  ,   "loaves"    ,"potatoes",
 "focuses"   ,"thieves"   ,"tornadoes" ,"women")
  print()

  expect_equal(pluralize_gift(test1), answer)
})



test_that("Testing make_phrase", {


   xmas <- read.csv("https://www.dropbox.com/s/e584pryn8evm1gz/xmas.csv?dl=1")
  xmas <- xmas %>%
    mutate(
      Full.Phrase = make_phrase(xmas$Day, xmas$Day.in.Words, xmas$Gift.Item, xmas$Verb, xmas$Adjective, xmas$Location)
    ) %>%
    arrange(desc(Day))

  answer <- c(
     "twelve drummers drumming" ,
     "eleven pipers piping",
     "ten lords a-leaping",
     "nine ladies dancing",
     "eight maids a-milking",
     "seven swans a-swimming",
     "six geese a-laying",
     "five golden rings",
     "four calling birds",
     "three french hens",
     "two turtle doves" ,
     "a partridge in a pear treed"
  )

  expect_equal(xmas$Full.Phrase, answer)
})

test_that("Testing sing_day", {

  xmas <- read.csv("https://www.dropbox.com/s/e584pryn8evm1gz/xmas.csv?dl=1")
  xmas <- xmas %>%
    mutate(
      Full.Phrase = make_phrase(xmas$Day, xmas$Day.in.Words, xmas$Gift.Item, xmas$Verb, xmas$Adjective, xmas$Location)
    ) %>%
    arrange(desc(Day))

  expect_equal(sing_line(xmas,2, Full.Phase), "On the second day of Christmas, my true love sent to me,
two turtle doves
and a partridge in a pear tree.")

})
