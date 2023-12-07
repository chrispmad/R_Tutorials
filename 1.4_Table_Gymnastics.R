# The {tidyverse} packages' functions are great for:
  #1. Organizing a table
  #2. Combining tables
  #3. Summarizing tables
  #4. Reshaping tables (e.g. from long to wide format)

# I've made a sample dataset that consists of two excel sheets.
# 1. Field sample data.
# 2. Sample ID 'lab' data.

library(tidyverse)
library(openxlsx)

# Let's read them in!

samples = read.xlsx('data/toy_bee_sample_data.xlsx')

ids = read.xlsx('data/toy_bee_id_data.xlsx')

# If we look on the right side of the screen, we can see how many rows
# (observations) and columns (variables) each table has.

# We could also use functions to pull out specific info. E.g.
# 1. Number of rows.
nrow(samples)
# 2. Column names.
names(samples)
# 3. Get a glimpse of the first (or last) 6 rows.
head(samples) #(or use tail() to see the last 6)
# 4. View() to see the whole table. Word of warning: if your data table
#    includes a complex polygon geometry column, this View() call can take
#    many minutes!
View(samples)
# 5. Just call the object name to see it all in the console.
samples

# Ugh, I can't see the column names! Let's upgrade to using a {tidyverse}
# "tibble".
samples = as_tibble(samples)
ids = as_tibble(ids)

samples
# Nice, now we see the variable data types, see the first 10 rows,
# negative numeric columns are printed in red, and we see the dimensions
# of the table at the very top.

# I notice that the sample_date column, which was a functional excel date column,
# is now garbled. It's actually a number representing the number of days passed
# since Jan 1st, 1970 (why that date? Maybe it was the start of computers? Not sure!)
# We can use {openxlsx}'s function 'convertToDate()' to translate this into
# a date column!
samples$sample_date = convertToDate(samples$sample_date)

samples

# =========================================
# = = = = = = = = = #
# Organizing Tables #
# = = = = = = = = = #

# Now that we have a date column, let's reorder our data, from oldest to newest.
samples = samples |>
  arrange(sample_date)
# If we would like to reverse this ordering, we can wrap our ordering variable
# in a 'desc()'.
samples |>
  arrange(desc(sample_date))

# We could also filter out certain rows, e.g. if we didn't want to keep
# any samples from before 2020. First, we could add a new column that
# is the year of the sample, then we could filter our records based on
# that column.

recent_records = samples |>
  mutate(the_year = lubridate::year(sample_date)) |>
  filter(the_year >= 2020)

# # # # # # # # # #
# EXERCISE TIME!
# # # # # # # # # #

# We can use the function 'slice()' to grab certain rows from our dataset.

# Can you subset the 'samples' dataset for only the 10 most recent observations?






# =========================================
# = = = = = = = = = #
#   Joining Tables  #
# = = = = = = = = = #

# This is a very important skill, as data is frequently stored in multiple tables
# that share certain columns in common (these are typically called 'keys').

# We have at least four join types possible with the {dplyr} package that comes
# in {tidyverse}:
# 1. left_join (the most common, in my opinion).
#    Keep all observations in the first table, and add on records from
#    second table that match ALL COLUMNS IN COMMON (usually just one)
bee_dat = samples |>
  left_join(ids)

# We don't have to specify which columns to join on, IF they are named identically.

# 2. right_join; this is a reverse left_join, so it keeps all records of the
#    second data table and only those that match from the first table.

# 3. full_join; keeps ALL rows from both tables.
samples |>
  full_join(ids) |>
  # Let's take a look at rows where the 'family' column is NA.
  filter(is.na(family))

# 4. anti_join; the reverse of a full_join, keeps only rows from first table
#    that DON'T match any rows in the second table.
samples |>
  anti_join(ids)


# # # # # # # # # #
# EXERCISE TIME!  #
# # # # # # # # # #

# 1. Our supervisor just asked for the 'bee_dat' dataset - but only for the 20 most
# recent records! Try to produce the dataset.



# 2. What are some other ways you could produce this dataset?



# 3. Going further: we can use the package {lubridate} (included in tidyverse) to
#    pull year (or month / day / hour / second!) from a date column. Can you
#    produce a bee_dat dataset for just 2021? How about for 2020:2022?




# =========================================
# = = = = = = = = = = #
#  Summarizing Tables #
# = = = = = = = = = = #

# When we want to summarize our tables of data, three functions really
# come to our aid:
# 1. count()
# 2. group_by()
# 3. summarise() (or "summarize()")

bee_dat |>
  mutate(the_year = lubridate::year(sample_date)) |>
  count(the_year)
# The results will automatically be ordered based on the variable
# we ran the count() on.

# If we would like the output to be ordered big -> small,
# we can do that too.

bee_dat |>
  mutate(the_year = lubridate::year(sample_date)) |>
  count(the_year, sort = T)

# We can also do a count on multiple columns simultaneously.
bee_dat |>
  mutate(the_year = lubridate::year(sample_date)) |>
  count(the_year,family)

# Let's imagine we had a 'number_specimen' column that indicated
# how many bees of a certain species were caught at that location and date.
# We could group_by() any of our columns to then summarise whatever statistic we would like.
bee_dat |>
  mutate(number_specimen = sample.int(n = 5,
                                      size = 84,
                                      replace = TRUE)) |>
  # We can group by a single column or by multiple columns.
  group_by(family,subfamily) |>
  summarise(
    total_specimen = sum(number_specimen),
    standard_dev = sd(number_specimen)
  )


# =========================================
# = = = = = = = = = #
# Reshaping Tables  #
# = = = = = = = = = #

# Sometimes we have a table of data set up so that we have many columns;
# this is known as a 'wide' format. If we have fewer columns and many rows,
# this is known as a 'long' format. I fairly frequently have to switch
# between these modes to calculate a new column or maybe to make a ggplot.

# Let's imagine a particular situation: we want to take our bee dataset from its
# 'wide' format and put ALL of the columns relating to taxonomy into just two columns:
# One could be 'tax_level', the other 'tax_name'. For example:
bee_dat |>
  pivot_longer(cols = c(family, subfamily, genus, subgenus, scientific_name),
               # What is the name of the column that will receive the column names?
               names_to = 'tax_level',
               # What is the name of the column that will receive the info in each row?
               values_to = 'tax_name')

# Our data is now a LOT longer than before! Makes sense: we have fewer columns.

# We could do the same thing with our latitude and longitude columns, if we wanted to.
bee_dat |>
  pivot_longer(cols = c(lat,lon),
               names_to = 'coordinate_type',
               values_to = 'coordinate_val')

# Let's imagine we wanted to make a bar plot showing the spread of each level of the taxonomic info.
# We could start by pivotting our table to be long. This gives us the new columns 'tax_level' and
# 'tax_name' that can be very easily sent to ggplot for plotting. The alternative would be
# adding a geom_bar() to our ggplot for every single taxonomic related variable, which would be
# a lot of extra typing, would prevent us from making a legend easily, and would prevent us
# from easily splitting our plot into several facets. E.g.

bee_dat |>
  pivot_longer(cols = -c(sample_date,sample_id,lat,lon),
               # Notice I actually specify which columns to EXCLUDE in the line above!
               # If you're excluding fewer columns than including, this can save you typing.
               names_to = 'tax_level',
               values_to = 'tax_name') |>
  ggplot() +
  geom_bar(aes(tax_name)) +
  labs(y = 'Number of Specimens',
       x = 'Taxonomy') +
  # Split our plot into separate facets based on the 'tax_level' variable.
  # Setting scales to 'free' allows our x and y axis levels to be facet-specific.
  facet_wrap( ~ tax_level, scales = 'free') +
  # I'd like angled x-axis text labels, since they can be so long!
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# Maybe I'd like to exclude those 2 rows that didn't have a matching
# ID. I could do that "in the pipe" as we modify our data temporarily and
# then plot it, like this:
bee_dat |>
  pivot_longer(cols = -c(sample_date,sample_id,lat,lon),
               names_to = 'tax_level',
               values_to = 'tax_name') |>
  filter(!is.na(tax_name)) |>
  ggplot() +
  geom_bar(aes(tax_name)) +
  labs(y = 'Number of Specimens',
       x = 'Taxonomy') +
  facet_wrap( ~ tax_level, scales = 'free',
              # We can specify the layout of the facets like so:
              ncol = 5, nrow = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# One more thing before we leave this plot! If you want to specify
# an order to your plot facets, we can convert the tax_level variable
# to a factor (i.e. an ordered categorical variable)
bee_dat |>
  pivot_longer(cols = -c(sample_date,sample_id,lat,lon),
               names_to = 'tax_level',
               values_to = 'tax_name') |>
  filter(!is.na(tax_name)) |>
  # New line below here:
  mutate(tax_level = factor(tax_level,levels = c('family',
                                                 'subfamily',
                                                 'genus',
                                                 'subgenus',
                                                 'scientific_name'))) |>
  ggplot() +
  geom_bar(aes(tax_name)) +
  labs(y = 'Number of Specimens',
       x = 'Taxonomy') +
  facet_wrap( ~ tax_level, scales = 'free',
              ncol = 5, nrow = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
