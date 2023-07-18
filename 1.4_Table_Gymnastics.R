# The {tidyverse} packages' functions are great for:
  #1. Organizing a table
  #2. Combining tables
  #3. Reshaping tables (e.g. from long to wide format)
  #4. Summarizing tables

library(tidyverse)

# Let's use a beaver dataset that comes with R. I'd done a little bit of
# pre-cleaning to the dataset - I'll include the code at the bottom of this script
# in a kind of appendix.

beav = read_csv('data/beavers.csv')

beav
# The dataset is currently organized by time. We could adjust this organization
# with the arrange() function.

beav = beav |>
  arrange(temp)
# Now the dataset is arranged by temperature (from low to high). If we want to
# arrange by high to low, we can also do that.

beav = beav |>
  arrange(desc(temp))

# What if we just want to get a subset of our data, e.g. where temp is greater
# than or equal to the mean temperature?

beav_high_temp = beav |>
  # We put some filtering condition as the argument to filter(); it should
  # evaluate to either TRUE or FALSE.
  filter(temp >= mean(temp))

# Let's take a quick glance at our data - I'm curious about which months
# we have data for.

beav |>
  ggplot() +
  geom_histogram(aes(obs_datetime))

# Looks like it's just early November and mid December.

# First, pull the month out of our datetime column.
beav = beav |>
  mutate(Month = lubridate::month(obs_datetime)) |>
  mutate(Day = lubridate::day(obs_datetime))

# We can 'pivot' our tables, either from 'long' format to 'wide', or from 'wide'
# to 'long'. In this case, I've proposed a 'long-to-wide' pivot, since we will
# increase the number of columns and shorten the number of total rows in the table.

# Let's summarise our data for each day...
beav_sum = beav |>
  # Replace the 0 and 1 for 'activ' column with text values.
  mutate(activ = ifelse(activ == 1, 'Active', 'Not_Active')) |>
  group_by(Month,Day) |>
  count(activ)

beav_sum

# And now let's put the counts for 'active' and 'not active' into new columns.
beav_sum = beav_sum |>
  ungroup() |>
  pivot_wider(names_from = activ, values_from = n, values_fill = 0)
















# =========================================
# Appendix

# i. Beaver dataset cleaning code

b1 = as_tibble(beaver1)
b2 = as_tibble(beaver2)

# What do these tables look like?
b1
# 114 rows of data
b2
# 100 rows of data



# We could combine these tables vertically, with "bind_rows()"
# i.e.:

#   ##########
#   #Table 1 #                 #########
#   ##########                 #       #
#                     ===>     # Combo #
#                              # table #
#   ##########                 #       #
#   #Table 2 #                 #########
#   ##########

beavers_combined = b1 |>
  bind_rows(b2)

# The day and time columns could also be formatted better by
# creating a 'datetime' column. To do this, we'll
# have to use base R's "as.Date()" and "paste0()" functions,
# plus a bit of the {stringr} and {lubridate} packages.
beavers_clean = beavers_combined |>
  # mutate (from {dplyr}) to create a new column...
  mutate(the_date = as.Date(day, origin = '2013-12-31')) |>
  # I see that some values for the 'time' column lack any mention of an hour...
  # Correct those here.
  mutate(time = as.character(time)) |>
  mutate(time = case_when(
    str_length(time) == 2 ~ paste0("0",time),
    str_length(time) == 1 ~ paste0("00",time),
    T ~ time
  )) |>
  mutate(the_time = paste0(
    # To get hours, strip away the final 2 characters...
    str_remove(time, '[0-9]{2}$'),
    # Add a : in the paste0...
    ":",
    # And now extract the final two digits to get minutes
    str_extract(time, '[0-9]{2}$')
  )) |>
  # Create another new column that puts the date and time
  # together to make a single 'datetime' column.
  mutate(my_datetime = lubridate::ymd_hm(paste0(the_date," ",the_time)))|>
  dplyr::select(obs_datetime = my_datetime, temp, activ)

write.csv(beavers_clean, 'data/beavers.csv', row.names = F)
