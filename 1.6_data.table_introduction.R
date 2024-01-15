# Description: {data.table} is a great package for making your table transformations
# orders of magnitude faster and more efficient. While this may feel unnecessary
# for smaller datasets, it can be a game-changer for more complex tasks in
# environments where RAM usage and operation times are critical, such as
# in R shiny apps.

# Other Resources:
# 1. Cheat sheet: https://images.datacamp.com/image/upload/v1653830846/Marketing/Blog/data_table_cheat_sheet.pdf
# 2. {data.table} 's official intro vignette: https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html
# 3. A 'gentle introduction to data.table': https://atrebas.github.io/post/2020-06-17-datatable-introduction/

# Script Author: Chris Madsen
# Contact info:
# i. email = chris.madsen@gov.bc.ca
# ii. Github = chrispmad

# Script last modified: 2024-01-15

# Throughout this tutorial, I will compare base R methods to tidyverse methods
# (specifically {dplyr} and {tidyr}) to {data.table} methods.

library(data.table)
library(tidyverse) # For comparison to data.table code
library(palmerpenguins) # For a fun dataset to play with

# Based on a tutorial found here: https://www.machinelearningplus.com/data-manipulation/datatable-in-r-complete-guide/

dat = palmerpenguins::penguins_raw # Grab the raw form of the dataset in {palmerpenguins}
# If you have a dataset (e.g. a table), you can read it into R directly as a
# data.table object using the function 'fread()'

# You can also make a data.table from scratch like this:
dt_from_scratch = data.table(A = c(1:5),
                             B = sample(letters, 5))

# Just to make typing easier, I am going to replace all the column names
# with 'snakecase' column names, because currently they have spaces and
# require ` ` to call. Ugh!
names(dat) = snakecase::to_snake_case(names(dat))
# We can also do this in a tidyverse way:
dat = dat |>
  purrr::set_names(snakecase::to_snake_case)

setDT(dat) #This converts a data.frame IN PLACE to a data.table!

#To convert from data.table back to data.frame, use:
setDF(dat)

dat_base = as_tibble(dat)
# =======================
#  Basics
# =======================

# 1. Filtering (using the 'i' slot)

# We can filter data.table objects using similar syntax to SQL. If that is unfamiliar
# to you, no worries; it looks like this if we read it aloud:

# "Include the rows where i is true, do j with them, grouped by the following columns"
DT[i,j,by]

# Filter by one condition
dat[clutch_completion == 'Yes',]

# Filter by multiple conditions
dat[body_mass_g >= 3600 & flipper_length_mm >= 195,]

# This is equivalent to:
dat_base[dat_base$body_mass_g >= 3600 & dat_base$flipper_length_mm >= 195,] # base R

dat_base |>
  dplyr::filter(body_mass_g >= 3600,
                flipper_length_mm >= 195) # tidyverse

# 2. Working with columns (using the 'j' slot)

dat[, individual_id] # Get a single column

dat[, study_name:region] # Get multiple columns (in order)

# If you are feeding your column selection in as characters, you'll need
# to state 'with = F'. This is especially useful when you are abstracting your
# code to make it more useful in generalized situations, e.g. writing a function.
# Also, this can be quite useful when designing code for a Shiny app, since you
# will have inputs from the user to work with that are often strings.
myvar = "region"

dat[, myvar, with=F]

columns <- c('sample_number', 'delta_13_c_o_oo', 'delta_15_n_o_oo')

dat[, columns, with = F]

# Drop columns.
dat[, !"island", with = F]
# Or
dat[, !columns, with = F]

# This is equivalent to:
dat_base[,columns]
dat_base[,columns] <- NULL

dat_base |> dplyr::select(all_of(columns)) # Select columns in 'columns' vector.
dat_base |> dplyr::select(-any_of(columns)) # Drop columns in 'columns' vector.

# 3. Calculating new columns

# {data.table} uses slightly different syntax, specifically the ':='
# When using this little guy, it makes variables 'in place', meaning that
# the code modifies the dataset immediately, so there is no need to save
# the output over the original. This is distinct from how base R and tidyverse function,
# and imparts some efficiency and memory saving benefits (and a bit of risk!)

dat[, culmen_volume := culmen_length_mm * culmen_depth_mm]

# Check out new variable
dat$culmen_volume

# Some more examples with our custom made data.tables
dt_from_scratch[, letter_above := shift(B, 1, type = "lag")]
dt_from_scratch[, letter_below := shift(B, 1, type = "lead")]

# Tidyverse equivalent
dt_from_scratch |>
  dplyr::mutate(letter_above = dplyr::lag(B),
                letter_below = dplyr::lead(B))

# We can make multiple new columns in one line; note the use of
# `:=` AND '=' inside the parentheses.
dat[, `:=`(island_sex = paste0(island,'_',sex),
           big_penguins = body_mass_g > mean(body_mass_g, na.rm=T))]

dat$island_sex
dat$big_penguins

# Tidyverse equivalent
dat_base |>
  dplyr::mutate(island_sex = paste0(island,'_',sex),
                big_penguins = body_mass_g > mean(body_mass_g, na.rm=T))

# To delete columns, select them using quoted names and set them to NULL.
dat[, c('island_sex', 'big_penguins') := NULL]

# Tidyverse - just drop columns with 'select()'
dat |>
  dplyr::select(-c(island_sex, big_penguins))

# This example is incomplete.
dat[,
    .SD, # When we want to select ALL columns, when we have a 'by = ' argument, use '.SD'
]

# 4. Chaining together commands

# We can chain together commands in data.table, similar to using pipes (either %>% or |>)

# If we don't want to permanently add a column to our data.table, we can
# put the new column inside .(), which is a selector for columns to keep.
dat[culmen_length_mm > 40,
    .(median_flipper_length = median(flipper_length_mm, na.rm=T)),
    by = c('sex','island')][     # Start of 2nd data.table command
      median_flipper_length < 200 & !is.na(sex),
    ]
# A bit hard to read. But maybe as we become more familiar with data.table,
# it improves?

# Tidyverse equivalent
dat |>
  dplyr::filter(culmen_length_mm > 40) |>
  dplyr::group_by(sex,island) |>
  dplyr::mutate(median_flipper_length = median(flipper_length_mm, na.rm=T)) |>
  dplyr::filter(median_flipper_length < 200,
                !is.na(sex))
# Pretty readable! But... this can be slow when our data is big!

# 5. Data.table specific functions that can speed up your code

# Data table functions are famously fast. When possible, consider using:
# 'fread()' instead of read.csv / read_excel etc.
# In filtering logic, use %chin% instead of %in% for exact matching of strings
# e.g.
library(janeaustenr)
fake_dat = tibble(A = sample(janeaustenr::prideprejudice, 10000, replace = F),
                      X = sample(c(1:1000), 10000, replace = T))

fake_dat |>
  dplyr::filter(stringr::str_detect(A,'Darcy'),
                X >= 500)
# In filtering logic, use %flike% instead of stringr::str_detect() with regex patterns
# 'merge()' instead of left_join() and its cousins.
# 'frank()' instead of rank()
# 'frollmean() and its cousins to calculate rolling averages
# 'setkey()' and its cousins to maximize efficiency when performing joins / merges / filters

# And many more!

# But adopting new functions, syntax and packages is hard... is this really worth
# the hassle? Tune in to R tutorial 1.7_data.table_method_comparison.R to find out!
