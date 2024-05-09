# This script is a little exploration of what Cindy wants for the 'dat' table.

# dat = vroom::vroom('data/cindy_test_data.csv') |>
#   dplyr::ungroup() |>
#   dplyr::select(Date = End_date, Longitude_DD, Species, plant_id = Animal_ID,
#                 Station_ID,
#                 Age, Longitude_DD, Latitude_DD, occasion)
#
# # Obfuscate species
# dat = dat |>
#   dplyr::mutate(Species = ifelse(Species != "NULL", sample(c("Pumpkin","Cucumber","Zucchini","Squash"), size = nrow(dat), replace = T), Species))
#
# dat$occasion = as.numeric(stringr::str_remove_all(dat$occasion,'TRUE,'))
#
# write.csv(dat, 'data/cindy_dat_obsc.csv', row.names = F)

library(tidyverse)
library(vroom)
library(plotly)

# Cindy's data wasn't originally split up into 11 chunks, but let's see
# how we can greatly reduce the amount of code we need to write by using lists.

# Path to data files.
dlist = list.files('data/cindyH',
           # Optionally, we can add in a text pattern for the files.
           # This "regular expression" matches any files that contain
           # 'chunk_', one or more numbers, then '.csv'
           pattern = '.*chunk_[0-9]+.csv',
           # Include the path to each file with "full.names = T"
           full.names = T)

dlist

# Read in our data files.
dlist |>
  purrr::map( ~ read.csv(.x))

# Cool! But each element of the list is a separate table... let's stack these
# tables together.
dat = dlist |>
  purrr::map( ~ read.csv(.x)) |>
  dplyr::bind_rows() |>
  # I like to work with a particular kind of table called a 'tibble'
  as_tibble()

head(dat)

# How many 'occasions' do we have?
unique(dat$occasion)
#11! Note that the order of these occasions is not correct, though...

# Looks like the 'Age' column has no data! Let's drop it.
dat |>
  dplyr::filter(!is.na(Age))

dat = dat |>
  dplyr::select(-Age)

# Simplify some column names.
dat = dat |>
  dplyr::rename(plant_id = Plant_ID)

# We have lat, long, and time... therefore, we MUST make a space-time cube!!!
dat |>
  # Drop rows with "NULL" for species
  dplyr::filter(Species != 'NULL') |>
  plot_ly(x = ~Longitude_DD, y = ~Latitude_DD, z = ~occasion, color = ~Species,
          mode = 'markers', type = 'scatter3d')

# Cindy needed a matrix with three dimensions for her complicated model. The
# original data had observations of multiple species, but Cindy needed a matrix
# that only included observations of one particular species: Cucumber.
# The matrix dimensions are as follows:
# X: Plant ID (unique plant identifier - derived from sample IDs, which aren't in this table)
# Y: Station ID (unique station identifier)
# Z: Occasion (i.e. sampling date)
# Together, the 3D matrix's dimensions are 148 x 1312 x 11

# To make this, she needed to:
# 1. Conditionally replace the values in the 'Species' column with 1 for Cucumber
#    and 0 for everything else.
# 2. Make sure each occasion's 2D matrix had the same rows and columns, in the same order.

# The first step can be completed using typical tidyverse functions like mutate(),
# group_by(), and case_when().
# The second step was a bit more complicated, so we opted for a "map" function.

# What is map()?

1:5

output = 1:5 |>
  map( ~ {
    # We put code inside the curly braces that we want to run once for each element
    # that we feed in. In this case, once for 1, once for 2, once for 3, etc.

    # We refer to the element with ".x". If we are passing in 2 elements (to a map2()), we
    # refer to the second element with ".y". I personally rarely use map2().

    # map() statements return a list.
    print(paste0(".x is ",.x)) # Print statement executed when we run the function.
    .x ** 2 # element to the power of 2

    # The last thing we put in the map() statement is what gets 'exported'.
    # In this case, the .x ** 2
  })

output

# Since our output is a list of single-length vectors, we can't do what I usually do:
# use dplyr::bind_rows() to combine everything into one table, like we did with
# cindy's fragmented data files. Here, we can use unlist().

unlist(output)

# Quick aside: why use map() instead of a for loop?
# 1. Street cred
# 2. Unlike a for loop, which just receives a vector of integers (e.g. 1:10),
# you can pass in a list of objects such as tables. For example...


# Generating some fake data... 50 tables of data
list_of_tbls = 1:50 |>
  map( ~ {
    tibble(tbl_id = .x,
               categ = sample(mpg$manufacturer, size = 100, replace = T),
               values = runif(n = 100, min = 30, max = 50) + .x ** (9/8) + runif(n = 100, min = -0.1, max = 0.1)
               )
  })

list_of_tbls[[1]]

sum_tbl = list_of_tbls |>
  map( ~ {
    .x |>
      # Carry out some data filtering etc...
      dplyr::filter(categ != 'hyundai') |>
      # Summarise a numeric variable for each combination of grouping variables...
      dplyr::group_by(tbl_id, categ) |>
      dplyr::summarise(mean_val = mean(values), .groups = 'drop')
  }) |>
  # Combine the list of outputs into a single table.
  dplyr::bind_rows()

sum_tbl |>
  ggplot(aes(x = tbl_id, y = mean_val, col = categ)) +
  geom_point() +
  geom_path()




# Back to Cindy's data! #



# Let's break the problem down a bit: let's check out the 2D table for occasion 1.
# Note: this isn't a matrix yet, just a table.
dat |>
  dplyr::filter(occasion == 1) |>
  dplyr::filter(!is.na(plant_id)) |>
  dplyr::select(plant_id, Station_ID, Species) |>
  tidyr::pivot_wider(names_from = Station_ID, values_from = Species,names_prefix = 'st_') |>
  # Depending on the species that fills in each cell, let's replace Pekania p. with 1, and everything
  # else we'll replace with 0.
  dplyr::mutate(across(-plant_id, \(x) dplyr::case_when(
    is.na(x) ~ 0,
    x == 'Cucumber' ~ 1,
    T ~ 0)))

# But this 'matrix' needs to also have rows for all plant_IDs and columns for all stations,
# not just for those plant ID / station combinations that were present in that occasion.

# This is the full table - it has ALL stations and Plant IDs, filled in with all 0s.
matrix_frame = dat |>
  dplyr::mutate(st_filler = 'filler') |>
  dplyr::select(Station_ID, plant_id, st_filler) |>
  dplyr::distinct() |>
  tidyr::pivot_wider(names_from = Station_ID, values_from = st_filler, names_prefix = 'st_') |>
  dplyr::filter(!duplicated(plant_id)) |>
  dplyr::filter(!is.na(plant_id)) |>
  dplyr::mutate(dplyr::across(-plant_id, \(x) x = 0))

# Our mission:

# For each occasion (1 to 11), make a table showing 1s and 0s by unique plant ID as rows
# and stations as columns. Also, join on the remaining plant IDs and stations
# that were not initially present in each occasion's observations.
tbls_by_occasion_list = 1:11 |>
  map( ~ {

    # Let's us know which iteration the map statement is at...
    print(.x)

    dat_for_occ = dat |>
      dplyr::filter(occasion == .x) |>
      dplyr::ungroup() |>
      sf::st_drop_geometry() |>
      dplyr::filter(!duplicated(plant_id)) |>
      dplyr::filter(!is.na(plant_id)) |>
      dplyr::select(plant_id, Station_ID, Species) |>
      tidyr::pivot_wider(names_from = Station_ID, values_from = Species,names_prefix = 'st_') |>
      dplyr::mutate(across(-plant_id, \(x) dplyr::case_when(
        is.na(x) ~ 0,
        x == 'Cucumber' ~ 1,
        T ~ 0)))

    # Let's make a list of stations that are present in this results table.
    # We can then find which stations are MISSING from this list and go about
    # adding those to our results table for the occasion we are processing.
    stations_already_in_this_occasion = names(dat_for_occ)[-1]

    # Here are the rows and columns to add to our table...
    tbl_to_join = matrix_frame |>
      dplyr::select(-all_of(stations_already_in_this_occasion)) |>
      dplyr::mutate(across(-plant_id, \(x) x = 0))

    # This last thing in the map statement is what gets 'exported' to our results list
    dat_for_occ |>
      # Add on the station name columns from our 'tbl_to_join'. These are
      # added on the right side of our original table. The left_join uses
      # the plant IDs to join the two tables
      dplyr::left_join(tbl_to_join, by = join_by(plant_id)) |>
      # Add on the plant IDs - these are added on the bottom of our table.
      dplyr::bind_rows(tbl_to_join[!tbl_to_join$plant_id %in% dat_for_occ$plant_id,]) |>
      # Replace any NA values in the new columns with 0.
      dplyr::mutate(across(-plant_id, \(x) ifelse(is.na(x), 0, x)))
  })

length(tbls_by_occasion_list)
# 11 tables! That checks out.

# Here's how we can pull out the first element in our list: i.e., the table for
# occasion 1.
tbls_by_occasion_list[[1]]

# Better yet, we can give names to our list elements.
names_for_list = paste0('occ_',c(1:length(tbls_by_occasion_list)))

names_for_list

names(tbls_by_occasion_list) <- names_for_list

# And now we can use names to refer to the elements of the list
tbls_by_occasion_list$occ_1
# tbls_by_occasion_list$occ_8
tbls_by_occasion_list$occ_9

# Let's combine this list into one large table. We can use the element names
# to inform a new column that we can name when stacking the list elements into one table.
all_dat = tbls_by_occasion_list |>
  dplyr::bind_rows(.id = 'occasion')

all_dat

# Note: the order of the plant IDs and station IDs were different in each list element!
# If we prefer the order of these guys from our original matrix_frame, we can apply
# its order within each occasion chunk of our massive table like so:
all_dat = all_dat |>
  # Arrange the plant ID alphabetically...
  dplyr::arrange(occasion,plant_id) |>
  # Arrange the order of columns (i.e. station names) based on our matrix_frame...
  dplyr::select(occasion, plant_id, names(matrix_frame)[-1])

# Let's pivot our data long - we have to do this to make our 3D matrix,
# as well as for any nefarious plotting we might do later... >:)
dat_long = all_dat |>
  # Pivot all columns except occasion and plant_id
  tidyr::pivot_longer(cols = -c(occasion, plant_id)) |>
  dplyr::arrange(occasion,plant_id,name)

dat_long
# 2 million rows! Yikes.

# Reorder dat_long so that occ_2 comes before occ_10
dat_long = dat_long |>
  dplyr::mutate(occasion = factor(occasion, levels = paste0('occ_',1:11))) |>
  dplyr::arrange(occasion,plant_id,name)

# Little check of number of Cucumber dots by occasion!
dat_long |>
  dplyr::group_by(occasion) |>
  dplyr::summarise(total_by_occasion = sum(value))
# I know from chatting with Cindy that these numbers check out.

# Let's make the 3D matrix!

# Pull out the unique levels for the 3 dimensions
unique_occs = unique(dat_long$occasion)
unique_x = unique(all_dat$plant_id)
unique_y = names(all_dat[,-c(1,2)])

# Make some columns into factors.
dat_long = dat_long |>
  dplyr::mutate(plant_id = factor(plant_id, levels = unique_x),
                name = factor(name, levels = unique_y)) |>
  dplyr::arrange(occasion, plant_id, name)

dimensions_for_array = c(length(unique_x),
                         length(unique_y),
                         length(unique_occs))

dimension_names = list(unique_x,
                       unique_y,
                       unique_occs)

array_3d = array(data = dat_long$value,
                 dim = dimensions_for_array,
                 dimnames = dimension_names)

# We can access certain slices of the array using a 3-fold selection like so:

# 1. Entire table for occasion 1...
array_3d[,,'occ_1'] |>
  # As a tibble...
  as_tibble()

# 2. Occasion 2, for just 3 plant IDs in particular...
array_3d[c("2019-0001","2019-0013","2019-0015"),,'occ_2'] |>
  as_tibble()

# Let's tempt fate... can we make a cool 3D plot from this array?

# Start a blank plotly canvas.
p = plot_ly()

# Make a 'base' layer at some dummy level of occasion: 0.
# This seems to be necessary in plotly to ensure it plots all of the
# rows and columns, since all other occasions only have observations
# at some small number of stations.
p = p |>
  add_trace(
    data = dat_long |>
      dplyr::filter(occasion == 'occ_1') |>
      dplyr::mutate(value = 0),
    mode = 'markers', type = 'scatter3d',
    x = ~plant_id, y = ~name, z = 0,
    marker = list(size = 1,
                  color = 'grey')
  )

p

number_of_occasions = length(unique(dat_long$occasion))

# Since we want to modify the object 'p', which lives in our global environment,
# I believe we need to use a for() loop. I could be wrong! But it works.

for(i in 1:number_of_occasions){

    t_long_no_zero = dat_long |>
      # Zoom in on certain occasion...
      dplyr::filter(occasion == paste0('occ_',i)) |>
      # Keep only non-zero values
      dplyr::filter(value != 0)

    # Let's us know how many dots are being added to plotly figure in each iteration.
    print(nrow(t_long_no_zero))

    p = p |>
      add_trace(
        data = t_long_no_zero,
        mode = 'markers', type = 'scatter3d',
        x = ~plant_id, y = ~name, z = i,
        marker = list(color = rainbow(11)[i])
      )
}

p
