# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# ============= Welcome to my collection of R tutorials ============= #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Author/Maintainer: Chris Madsen
# Contact email(s): chris.madsen@gov.bc.ca; cpmadsenviking@gmail.com

# Please note: this will be the least exciting, least intuitive of the
# tutorials; however, it's all probably useful foundational knowledge.
# The tutorials should be more exciting and feel more worthwhile after this one!



# Commands in R are run in succession, i.e. one line at a time.
# Press 'Control' + 'Enter' to run the line that the blinking vertical line is
# currently on.

# Note: If you run code that assigns a value to a variable (see below),
#       that change will be permanent (unless you assign a new value to
#       that variable!); if you just do something like '1 + 2',
#       the results will show but no change to a or b will take place.

a = 2
b = 3

a * b

# The results, or output, of R commands are printed to the Console (typically,
# the lower left panel of RStudio's 4 panels).

# Note: you can also run code in the console!

# As you saw above, we can assign values (numbers, strings of letters, tables,
# or even more complicated data types) to variables. Variables defined
# as we have above are stored in the 'Global Environment', and can be accessed
# by writing their names. E.g.

b

# Most of the time in R, we will be dealing with tables of information.
# These tables can contain numbers, text, or even interesting things like
# statistical models, images, plots, etc.

# If a variable contains a table (also called a 'data-frame'), we can access
# individual rows or columns (called 'subsetting') using square brackets ( '[' and ']')
# and numbers.
# Note: if x and y are numbers, and we subset our table as follows: my_table[x,y]
# The number 'x' will select the row(s) to include, and the number 'y' the columns.
# E.g.

my_table = data.frame(dish = c("bread","kimchi","ghost pepper"),
                      spice_level = c("non-existent","a bit spicy","why did I eat this?"))

my_table # This prints out the entire table.

my_table[1,1] # This pulls out a single cell from our table: row 1, column 1.

my_table[1,] # If we don't include a number after the comma, that signifies we want ALL COLUMNS.

my_table[,1] # Here, we get all rows for column 1.

# A synonymous (but clearer) way to pull out a single column is to use '$'
my_table$spice_level

# We can use either of those two subsetting methods to create a new variable.
my_table$fallout = c("potential sugar crash","spicy lips","intestinal torment")

# Let's take a look at our table again.
my_table

# There are now 3 columns!

# A realistic scenario: we usually make new columns as combinations of existing ones.
# Let's make a new table.
fav_season = data.frame(season = c("spring","summer","fall","winter"),
                        responses = c(18,42,38,7))

# Perhaps we want to calculate the percentage of respondents that chose each season
# as their favourite. We can use the sum() function to add together all numbers in the responses column.
fav_season$total_n = sum(fav_season$responses)

# Let's check out our result.
fav_season

# Now let's make that new column.
fav_season$percent = 100*(fav_season$responses / fav_season$total_n)

fav_season
# Brilliant.

# Let's write our 'results' to our computer. We could write them
# as a text file, as a .CSV (comma separated values - very common to use),
# as an excel file, etc. Here we write an .CSV file.
write.csv(fav_season, "output/fav_season_results.csv", row.names = FALSE)

# Note: the 'row.names = FALSE' indicates that we do NOT want the write.csv
# function to add a numeric index column (i.e. a column that goes
# 1 -> the numer of rows we have).

# Let's get a bit more complicated:
# It's very convenient to be able to subset data while creating a new column.
# e.g.
fav_season$favourite = FALSE
fav_season[fav_season$responses == max(fav_season$responses),]$favourite = TRUE
fav_season

# One more example.
CO2

# Calculate average uptake
mean_up = median(CO2$uptake)

CO2$result = 'Standard'
CO2[CO2$Treatment == 'nonchilled' & CO2$uptake >= mean_up , ]$result = 'Exceeded'
CO2[CO2$Treatment == 'chilled' & CO2$uptake >= (0.8*mean_up), ]$result = 'Exceeded'
CO2

# We can also use this kind of subsetting to correct data values, e.g. perhaps you
# know that a dataset was read incorrectly and sampling locations were incorrectly
# recorded as William Lake instead of William's Lake. It would be fairly simple
# to fix this.

my_data = data.frame(
  location = c("William Lake","William Lake","William Lake","William's Lake"),
  sample = c(1.0,2.1,1.8,7.1)
)

my_data[ my_data$location == 'William Lake', ]$location = "William's Lake"

my_data

# There are simpler ways of doing this, thanks to 'packages' of functions that
# have been coded by other people but are available to us for free. We'll
# take a look at those in the following tutorials.

# Done! In the next tutorial, we'll read in our data, import a package to
# help us plot the data, and make our first plot.
