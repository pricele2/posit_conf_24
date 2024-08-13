######################################################
# Emma Rand (univ of york) and Ian
# LPRICE5
# Workshop 12 Aug 2024
######################################################

## still didn't get this part solved
# usethis::use_course("posit-conf-2024/programming-r-exercises")

library(tidyverse)
library(palmerpenguins)
data(penguins)
glimpse(penguins)

# can transform one at a time to make Z scores ----
# Z = (x = mean(x)/sd)

penguins <- penguins |>
  mutate(
    z_bill_length_mm = (bill_length_mm - mean(bill_length_mm, na.rm = TRUE)) / sd(bill_length_mm, na.rm = TRUE),
    z_bill_depth_mm = (bill_depth_mm - mean(bill_depth_mm, na.rm = TRUE)) / sd(bill_depth_mm, na.rm = TRUE),
    z_flipper_length_mm = (flipper_length_mm - mean(flipper_length_mm, na.rm = TRUE)) / sd(flipper_length_mm, na.rm = TRUE),
    z_body_mass_g = (body_mass_g - mean(body_mass_g, na.rm = TRUE)) / sd(body_mass_g, na.rm = TRUE))
# tedious, error prone

# form for a function:
# name as a verb = function(arguments){
#   code body
#   }

# if you are having a hard time naming the fucntion you may be trying to do too much inside one function
# btter practice is to build smaller, more modular snippets

# verb = function(x) {what you do to X }

to_z = function(x) {
  (x - mean(x, na.rm = T)) / sd(x, na.rm = T)
}

penguins <- penguins |>
  mutate(
    z_bill_length_mm = to_z(bill_length_mm),
    z_bill_depth_mm = to_z(bill_depth_mm),
    z_flipper_length_mm = to_z(flipper_length_mm),
    z_body_mass_g = to_z(body_mass_g)
  )

# adding a modification that has TWO arguments in the input

to_z <- function(x, middle) {
  trim = (1 - middle)/2
  (x - mean(x, na.rm = TRUE, trim = trim)) / sd(x, na.rm = TRUE)
}

penguins = penguins |> mutate(z_bill_depth_mm = to_z(bill_depth_mm, 0.9))

# but if you miss out on setting the second variable it'll complain, so we write a default into the function

to_z <- function(x, middle = 1) {
  trim = (1 - middle)/2
  (x - mean(x, na.rm = TRUE, trim = trim)) / sd(x, na.rm = TRUE)
}

# now we write another, but this time the output is not a vector but a single value ----
sd_error = function(x) {
  sd(x, na.rm = TRUE / sqrt(sum(!is.na(x))))
  }

penguins |> summarise(se = sd_error(bill_length_mm))

# Write a function to compute the sums of squares ----
sum_sq <- function(x){
  sum((x[!is.na(x)] - mean(x[!is.na(x)]))^2)
}
sum_sq(penguins$bill_length_mm)

# Now we can use all of them to input a dataframe and return a dataframe
# do it one at a time, like a chump :-p
penguins |>
  summarise(mean = mean(bill_length_mm, na.rm = TRUE),
            n = sum(!is.na(bill_length_mm)),
            sd = sd(bill_length_mm, na.rm = TRUE),
            se = sd_error(bill_length_mm))

# OR write a function that does all at once : first attempt
my_summary <- function(df, column){
  df |>
    summarise(mean = mean(column, na.rm = TRUE),
              n = sum(!is.na(column)),
              sd = sd(column, na.rm = TRUE),
              se = sd_error(column))
}

# lets try it!
my_summary(penguins, bill_length_mm)

Error in `summarise()`:
  ℹ In argument: `mean = mean(column, na.rm = TRUE)`.
Caused by error:
  ! object 'bill_length_mm' not found
# doesn't work because it's looking for a column called column
# see https://posit-conf-2024.github.io/programming-r/01-functions-01.html#/tidy-evaluation

# So you have to embrace extra squiggle brackets inside the function
my_summary <- function(df, column){
  df |>
    summarise(mean = mean({{ column }}, na.rm = TRUE),
              n = sum(!is.na({{ column }})),
              sd = sd({{ column }}, na.rm = TRUE),
              se = sd_error({{ column }}),
              .groups = "drop")
}

NB include .groups = "drop" to avoid message and leave the data in an ungrouped state

# and now it will work!
my_summary(penguins, bill_length_mm)

# what if you need to group by different vars? add a third var in the fn(x y z) setup

my_summary <- function(df, summary_var, group_var){
  df |>
    group_by({{ group_var }}) |>
    summarise(median = median({{summary_var  }}, na.rm = TRUE),
              minimum = min({{summary_var  }}, na.rm = TRUE),
              maximum = max({{summary_var  }}, na.rm = TRUE),
              .groups = "drop")
}

my_summary(penguins, bill_length_mm, species)

# use pick( ) with the group_var setup to be able to add grouping vars iteratively

my_summary <- function(df, summary_var, group_var = NULL){
  df |>
    group_by(pick({{ group_var }})) |>
    summarise(median = median({{summary_var  }}, na.rm = TRUE),
              minimum = min({{summary_var  }}, na.rm = TRUE),
              maximum = max({{summary_var  }}, na.rm = TRUE),
              .groups = "drop")
}
my_summary(penguins, bill_length_mm, c(species, island))

Shortcuts:
-  put cursor on a function call and press F2 to find its definition
- Ctrl+. opens section/file search

there is more content as 'advanced' at
https://posit-conf-2024.github.io/programming-r/01-functions-01-advanced.html


## PART TWO before lunch ----
# https://posit-conf-2024.github.io/programming-r/02-functions-02.html

I sort of missed the first 10 minutes trying to find the damn slides

# `...` passed to `geom_histogram()`

histogram <- function(df, var, ..., binwidth = NULL) {
  df |>
    ggplot(aes(x = {{var}})) +
    geom_histogram(binwidth = binwidth, ...)
}
# functions will make some things harder and some easier: just be deliberate about it

# eg Labeling

temp <- function(varname, value) {
  rlang::englue("You chose varname: {{ varname }} and value: {value}")
}

temp(val, 0.4)

# you can write a function that depends only on its inputs
#  Easier to test!

add <- function(x, y) {
  x + y
}

# uses side effects: not necessarily bad but can be computationally costly

x <- prod(1, 2, 3) # pure

x <- print("Hello") # side effect

x <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()  # side effect

x <- sort(c("apple", "Banana", "candle")) #

# After lunch! Iteration-01
We get iteration for free in R, sort of
https://posit-conf-2024.github.io/programming-r/03-iteration-01.html

We have:

  group_by() with summarize()

facet_wrap()

across() and purrr()

. . . - the apply() family

# usethis::use_r("iteration-01")
library(tidyverse)
library(palmerpenguins)
data(penguins)
glimpse(penguins)

# generate the se function from this morning
sd_error <- function(x){
  sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
}

temp0 = penguins |> summarize(se_bill_depth = sd_error(bill_depth_mm))
# summarizes just one column, so using ACROSS()

temp1 = penguins |> summarize(across(bill_length_mm:body_mass_g, sd_error))
# only works if the cols are next to each other

.cols() uses the same specification as select(): starts_with(), ends_with(), contains(), matches()

temp1 = penguins |> summarize(across(ends_with("mm"), sd_error))

temp2 = penguins |>
  select(-year) |>
  group_by(species, sex, island) |>
  summarise(across(everything(), sd_error))  # got 'must be vector not function then fixed ( placement
# does everything that isn't already included in group_by

temp3 = penguins |>
  select(-year) |>
  summarise(across(where(is.numeric), sd_error)) # across calls sd_error

# the \ and the ~ are short for FUNCTION, when they're written inside something else as anonymous (as in, not named)
temp4 = penguins |>
  summarise(across(ends_with("mm"),
                   function(x) mean(x, na.rm = TRUE)))
is the same as

temp4 = penguins |>
  summarise(across(ends_with("mm"),
                   \(x) mean(x, na.rm = TRUE)))

temp4 = penguins |>
  summarise(across(ends_with("mm"),
                   ~ mean(.x, na.rm = TRUE)))

# so what if you need to do more than one? make a list

\(x) is base syntax new in 4.1.0 Recommended
~ .x is fine but only works in tidyverse functions

temp5 = penguins |>
  summarise(across(ends_with("mm"), _MORE THAN ONE FUNCTION_))

temp6 = penguins |>
  summarise(across(where(is.numeric), list(
    sd_error,
    length)))

Or with anon functions

temp7 = penguins |>
  summarise(across(ends_with("mm"), list(
    \(x) mean(x, na.rm = TRUE),
    \(x) sd(x, na.rm = TRUE))))

# produces with suffix -1 and -2

temp7 = penguins |>
  summarise(across(ends_with("mm"), list(
    mean = \(x) mean(x, na.rm = TRUE),
    sd = \(x) sd(x, na.rm = TRUE))))

So the column name is {.col}_{.fn}: bill_length_mm_mean

temp7 = penguins |>
  summarise(across(ends_with("mm"),
                   list(mean = \(x) mean(x, na.rm = TRUE),
                        sdev = \(x) sd(x, na.rm = TRUE)),
                   .names = "{.fn}_of_{.col}"))

Super useful with mutate()

to_z <- function(x, middle = 1) {
  trim = (1 - middle)/2
  (x - mean(x, na.rm = TRUE, trim = trim)) / sd(x, na.rm = TRUE)
}

# this morning did it this way
penguins |>
  mutate(
    z_bill_length_mm = to_z(bill_length_mm),
    z_bill_depth_mm = to_z(bill_depth_mm),
    z_flipper_length_mm = to_z(flipper_length_mm)
  ) |>
  glimpse()

temp8 = penguins |>
  mutate(across(ends_with("mm"),
                to_z,
                .names = "z_{.col}")
  ) |>
  glimpse()

my_summary <- function(df, cols) {
. . . .
}

temp9 <- function(df, cols) {
  df |>
    summarise(across({{ cols }},
                     list(mean = \(x) mean(x, na.rm = TRUE),
                          sdev = \(x) sd(x, na.rm = TRUE))),
              .groups = "drop")
}

temp10 = my_summary <- function(df, cols = where(is.numeric)) {
  df |>
    summarise(across({{cols}},
                     list(mean = \(x) mean(x, na.rm = TRUE),
                          sdev = \(x) sd(x, na.rm = TRUE))),
              .groups = "drop")
} #includes

# PART FOUR --- purrr and here()
# aka the whole reason I came to Seattle this week

using purrr::map() to read a bunch of files
using purrr::walk() to write a bunch of files
functional programming, more generally
here::here()

## What's next?

library(fs)
library(stringr)

paths <-
  # get the filepaths from the directory
  fs::dir_ls(here("data/gapminder")) |>
  # convert to list
  # extract the year as names
  print()

# so the solutino is to write a funciton that will gET the naming convention

get_year = function(x) {
  x |> basename()
}
get_year("taylor/swift/1989.txt")

## or rewrite it to use a regex to exclude .txt

get_year = function(x) {
  x |> basename() |> str_extract("^\\d+")
}

get_year("taylor/swift/1989.txt")

# Slide 8
data <-
  paths |>
  # read each file from excel, into data frame
  map(read_excel) |>
  # keep only non-null elements
  # set list-names as column `year`
  # bind into single data-frame
  list_rbind() |>
  # convert year to number
  mutate(year = parse_number(year))
  print()

# PURRR HAS A FUNCTION CALLED POSSIBLY()
library("purrr")

poss_read_csv <- possibly(read_csv, otherwise = NULL, quiet = FALSE)

# Have now forked this but not downloaded it :
https://github.com/pricele2/posit-programming-r-exercises

# now on iteration -02 -02 -writing -files. R


