# Description: {data.table} is a great package for making your table transformations
# orders of magnitude faster and more efficient. While this may feel unnecessary
# for smaller datasets, it can be a game-changer for more complex tasks in
# environments where RAM usage and operation times are critical, such as
# in R shiny apps.

# Script Author: Chris Madsen
# Contact info:
# i. email = chris.madsen@gov.bc.ca
# ii. Github = chrispmad

# Script last modified: 2024-01-15

# Throughout this tutorial, I will compare base R methods to tidyverse methods
# (specifically {dplyr} and {tidyr}) to {data.table} methods.

library(tidyverse)
library(data.table)

# Let's make some data to play with. Our task here is to sort a very large
# table into categories via a string search, then count the number of rows
# in each group for each PAT_KEY (patient ID number). This kind of task can take a
# very long time if we use inefficent methods for a large dataset.

# i. string-to-category group lookup table.
group_categ = tibble(
  name_vector = c(
    '(Staphylococcus aureus|Resistant Staphylococcus aureus|Staphylococcus aureus suffix)',
    '(Enterococcus zoop|Sticky Enterococcus zoop)',
    '(Garbage|Random|Blah)'),
  group = c(
    "Staphylococcus aureus",
    "Enterococcus zoop",
    "remove!")
)

# It might be nice to have a 'long' version of this look-up table.
# This will be easier to join to our dataset.
group_categ_long = tidyr::separate_longer_delim(group_categ, cols = name_vector, delim = "|") |>
  # Remove a ( or an ) in the values of the 'name_vector' column.
  dplyr::mutate(name_vector = stringr::str_remove(name_vector, "(\\(|\\))"))

# ii. Tiny dataset
dat_tiny = tibble(pat_key = c(329138,329138,329138,100201,100201,100201),
             OBSERVATION = c('Staphylococcus aureus', 'Resistant Staphylococcus aureus',
                             'Staphylococcus zoozoo','Enterococcus zoop','Sticky Enterococcus zoop','Random'))

# Okay, we're going to try to classify our data and then do a count by bacterial
# infection group. Let's try out a range of methods.
# 1. Base R for loop with separate lines for count function.
# 2. Base R lapply with separate lines for count function.
# 3. Tidyverse dplyr mutate function with complicated logic inside the mutate
# 4. Tidyverse dplyr that first left_joins the look-up table then summarizes
# 5. Data.table that first merges the look-up table then summarizes

# Base R:
dat_base = dat_tiny

# Assign groups!

# The following code doesn't work!
# group_categ[str_detect(group_categ$name_vector,dat_tiny$OBSERVATION),]$group

# Method 1. Base R for loop with separate lines for count function.

# Since we have multiple bacteria strains that fall into the same group,
# doing an exact string match by row is a bit tough. We could do it as a for loop:

# Create a new column that we will fill in with our loop
dat_base$group = 'unknown'
for(row in 1:nrow(dat_base)){
  # Test if the row in a given loop run has a strain in our group look-up table.
  strain_in_lookup_tbl = str_detect(group_categ$name_vector,dat_base[row,]$OBSERVATION)

  strain_in_lookup_tbl

  # If 1+ row of the look-up table had a match...
  if(sum(strain_in_lookup_tbl) > 0){
    # Fetch the group from that row of the group_categ table and assign it to this row
    # of our dataset.
    dat_base[row,]$group = group_categ[strain_in_lookup_tbl,]$group
  } else {
    # No match in our look-up table! Label this row as 'other_group'
    dat_base[row,]$group = 'other_group'
  }
}

# Method 2. Base R lapply with separate lines for count function.
# We could do it as an lapply with unlist on the results.
grps = unlist(
  lapply(
    dat_base$OBSERVATION,
    \(x) {
      grp = group_categ[str_detect(group_categ$name_vector, x),]$group
      ifelse(length(grp) != 0, grp, 'other_group')
    }
  )
)

dat_base$group = grps

dat_base = dat_base[dat_base$group != 'remove!',] # Drop 'remove!' rows.

# Do the count!
dat_base_c = as.data.frame(table(subset(dat_base, select = c(pat_key,group))))
dat_base_c = dat_base_c[dat_base_c$Freq > 0,] # Drop combos of pat_key and group that were 0 count.
dat_base_c = dat_base_c[order(-dat_base_c$Freq),] # Re-order rows to be descending.

dat_base_c
# Everything is done in short-ish commands, but I personally don't find them
# to always be very readable. This could be thought of as complex slam poetry.



# Tidyverse:
dat_tidy = dat_tiny

# Method 3. Tidyverse dplyr mutate function with complicated logic inside the mutate
dat_tidy |>
  # Change the workflow of dplyr from a column orientation to
  # 'rowwise' - it will now treat each row as a group, kind of like
  # a for loop.
  rowwise() |>
  mutate(group = ifelse(
    # Does the observation value match at least one of the grouping_table's name vectors?
    sum(str_detect(group_categ$name_vector, OBSERVATION)) > 0,
    # If so, get the group that matches from the grouping table.
    group_categ[str_detect(group_categ$name_vector, OBSERVATION),]$group,
    # If not, label as 'other group'
    'other_group'
  )) |>
  # Drop records labelled 'remove!'
  filter(group != 'remove!') |>
  distinct(pat_key,group) |>
  add_count(pat_key)
# Nice, it's all done in one chunk - like a short story versus

# Method 4. Tidyverse dplyr that first left_joins the look-up table then summarizes
dat_tidy |>
  dplyr::left_join(
    # Make sure we have at least one column name in common, otherwise join
    # can't work
    group_categ_long,
    by = dplyr::join_by(OBSERVATION == name_vector)
    ) |>
  # Replace non-matches with 'other_group'
  dplyr::mutate(group = ifelse(is.na(group), 'other_group',group)) |>
  # Drop certain rows
  dplyr::filter(! group %in% c('remove!',NA)) |>
  # Do the count
  dplyr::count(pat_key, group)

# data.table
dat_dt = setDT(dat_tiny)

# Method 5. Data.table that first merges the look-up table then summarizes
merge(
  dat_dt,group_categ_long,
  by.x = 'OBSERVATION',by.y = 'name_vector',
  all = TRUE, # Do a full join so that we get all matches between the two tables
  )[group != 'remove!', # Set filter condition on data.table
    .N, # Choose which columns to make; .N is a specific function in data.table like
    #     dplyr's n() counting function.
    by = .(pat_key,group)] # Choose columns to group by.

# COMPARISONS OF SPEED AND MEMORY USAGE #
library(microbenchmark)

# Let's make a huge dataset to really highlight differences between our methods
datb = tibble(
  pat_key = sample(c(10000:50000), 100000, replace = T),
  OBSERVATION = sample(dat_tiny$OBSERVATION, 100000, replace = T)
)

# Method 1.
base_r_for_loop = function(dat,group_categ){
  dat$group = 'unknown'
  for(row in 1:nrow(dat)){
    # Test if the row in a given loop run has a strain in our group look-up table.
    strain_in_lookup_tbl = str_detect(group_categ$name_vector,dat[row,]$OBSERVATION)

    strain_in_lookup_tbl

    # If 1+ row of the look-up table had a match...
    if(sum(strain_in_lookup_tbl) > 0){
      # Fetch the group from that row of the group_categ table and assign it to this row
      # of our dataset.
      dat[row,]$group = group_categ[strain_in_lookup_tbl,]$group
    } else {
      # No match in our look-up table! Label this row as 'other_group'
      dat[row,]$group = 'other_group'
    }
  }
  dat = dat[dat$group != 'remove!',] # Drop 'remove!' rows.

  # Do the count!
  dat_c = as.data.frame(table(subset(dat, select = c(pat_key,group))))
  dat_c = dat_c[dat_c$Freq > 0,] # Drop combos of pat_key and group that were 0 count.
  dat_c = dat_c[order(-dat_c$Freq),] # Re-order rows to be descending.

  dat_c
}

# Method 2.
base_r_lapply = function(dat,group_categ){
  grps = unlist(
    lapply(
      dat$OBSERVATION,
      \(x) {
        grp = group_categ[str_detect(group_categ$name_vector, x),]$group
        ifelse(length(grp) != 0, grp, 'other_group')
      }
    )
  )

  dat$group = grps

  dat = dat[dat$group != 'remove!',] # Drop 'remove!' rows.

  # Do the count!
  dat_c = as.data.frame(table(subset(dat, select = c(pat_key,group))))
  dat_c = dat_c[dat_c$Freq > 0,] # Drop combos of pat_key and group that were 0 count.
  dat_c = dat_c[order(-dat_c$Freq),] # Re-order rows to be descending.

  dat_c
}

# Method 3.
dplyr_complex_mutate = function(dat,group_categ){
  dat |>
    rowwise() |>
    mutate(group = ifelse(
      sum(str_detect(group_categ$name_vector, OBSERVATION)) > 0,
      group_categ[str_detect(group_categ$name_vector, OBSERVATION),]$group,
      'other_group'
    )) |>
    filter(group != 'remove!') |>
    distinct(pat_key,group) |>
    add_count(pat_key)
}

# Method 4.
dplyr_join_then_summarise = function(dat,group_categ_long){
  dat |>
    dplyr::left_join(
      group_categ_long,
      by = dplyr::join_by(OBSERVATION == name_vector)
    ) |>
    dplyr::mutate(group = ifelse(is.na(group), 'other_group',group)) |>
    dplyr::filter(! group %in% c('remove!',NA)) |>
    dplyr::count(pat_key, group)
}

# Method 5.
datatable_join_then_summarise = function(dat,group_categ_long){
  dat_dt = setDT(dat)
  merge(
    dat_dt,group_categ_long,
    by.x = 'OBSERVATION',by.y = 'name_vector',
    all = TRUE,
  )[group != 'remove!', .N, by = .(pat_key,group)]
}

if(file.exists('output/data_wrangling_method_comparison.csv')){
  results = read.csv('output/data_wrangling_method_comparison.csv')
} else {
  results <- microbenchmark(
    baseR_forLoop = base_r_for_loop(datb, group_categ),
    baseR_lapply= base_r_lapply(datb, group_categ),
    tidyverse_mutate = dplyr_complex_mutate(datb, group_categ),
    tidyverse_join = dplyr_join_then_summarise(datb, group_categ_long),
    data_table = datatable_join_then_summarise(datb, group_categ_long),
    times = 5  # Number of iterations
  )
  print(results)
# Need to convert results to dataframe...
  write.csv(results, 'output/data_wrangling_method_comparison.csv', row.names = F)
}

results |>
  mutate(run_time = median/1000) |>
  mutate(run_time = ifelse(run_time >= 60,
                           paste0(round(run_time/60,2),' minutes'),
                           paste0(round(run_time,2), ' seconds'))) |>
  ggplot() +
  geom_col(aes(x = reorder(expr,-median), y = median, fill = expr)) +
  geom_errorbar(aes(x = reorder(expr,-median), ymin = min, ymax = max)) +
  scale_y_log10() +
  labs(y = 'Log10 of Median Method Time',
       x = 'Method',
       title = 'Comparison of Data Wrangling Methods') +
  geom_text(aes(x = reorder(expr,-median), y = median*0.5, label = run_time)) +
  theme(legend.position = 'none')

