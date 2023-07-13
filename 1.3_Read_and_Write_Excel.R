# It's quite common to have data stored in an excel file (or several!).
# Hopefully, the excel files have been structured in such a way as to have
# each row be a unique record and each column a different variable. Sometimes,
# excel sheets look more like a cluttered desk - up top, there's a table
# of info, but below, there are little hand-made summaries and possibly
# unrelated data... this style of excel sheet is not your friend in R...

# To read and write excel files, we can use the {openxlsx} package (other options
# exist - but I've found success with this package).

library(tidyverse)

install.packages('openxlsx')

library(openxlsx)

# Let's read in a 'toy' dataset of three species of iris flowers and their
# measurements.
iris = read.xlsx('data/iris_data.xlsx')

# Let's check out the data.
iris

# Note that all 150 rows are printed to the console. This is not very
# helpful if we are curious about what the column names are, or the data types.

# We can convert this data.frame to a 'tibble', {tidyverse}'s answer
# to this inconvenience.
iris = as_tibble(iris)

iris

# Now we get a snapshot of the top 10 rows, plus the column names and data types.

# Let's make a quick histogram and a boxplot to explore our data visually.

ggplot(iris) +
  geom_histogram(aes(Sepal.Length, fill = Species))


ggplot(iris) +
  geom_boxplot(aes(x = Species, y = Sepal.Length))

# Very cool - virginica has the largest sepals, setosa the smallest (these
# differences might or might not be statistically significant - more on
# this later!)

# Note that there are 4 columns with measurements... it would be nice
# if we could get a glance at all four variables in one go, no?
# This will require that we transform our data - specifically, from its
# current 'wide' format to a 'long' format: observe!

iris %>%
  # Select all columns except Species for the pivot...
  pivot_longer(cols = -Species)


# Now our table has 600 rows and 3 columns; the column names we had before
# have been placed into a new column called 'name', and the measurement
# values have been placed into a new column named 'value'. We can set
# those new columns' names to be more meaninful, but for now, they're ok.

# This time, I'm going to take our original dataset, pipe it into
# a pivot_longer(), and then pipe the result of that code directly
# into a ggplot!

iris %>%
  pivot_longer(-Species) %>%
  ggplot() +
  geom_boxplot(aes(x = Species, y = value, fill = name))

# Getting there... but still hard to read! Let's separate each measurement
# type into its own plot 'facet'

iris %>%
  pivot_longer(-Species) %>%
  ggplot() +
  geom_boxplot(aes(x = Species, y = value, fill = Species)) +
  facet_wrap( ~ name, scales = 'free')

# Note: the "scales = 'free'" in our facet_wrap call means the x and y axes
# of each facet get automatically adjusted for the data in the facet.
# If we don't set this argument to be 'free', the plots can be a little
# hard to read. Try it out, though!



# Let's make a new column: estimated area of flower petals and sepals.
# We'll use one of the {tidyverse} methods for making new columns: mutate()
iris = iris %>%
  mutate(petal_area = Petal.Length * Petal.Width) %>%
  mutate(sepal_area = Sepal.Length * Sepal.Width)

# Quick visual check...
ggplot(aes(x = petal_area, y = sepal_area, col = Species), data = iris) +
  geom_point()

# Setosa has small petals, big sepals, versicolor has moderately sized petals
# and sepals, and virginica has pretty big flower parts!

# Let's write out these results...

openxlsx::write.xlsx(iris, 'output/iris_results.xlsx')

# Note that the {openxlsx} package gives us 2 ways to write an excel file:
# 1. The write.xlsx() function we used above, which is easy and efficient.
# 2. The ability to create an excel workbook INSIDE R, which allows us to
#    add sheets to the workbook, set formatting rows and columns inside the sheets,
#    etc., and then write the workbook out to our computer. Cool but complicated!
