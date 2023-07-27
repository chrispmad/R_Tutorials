# Now that we've blasted through some examples of plotting, let's try to put
# this new knowledge into practice.

# Load in the tidyverse packages.
library(tidyverse)

install.packages("openxlsx") # Run this line if you have not yet installed this package.

library(openxlsx) #Helps us read and write excel files.

# ======================================================
# Step 1. Choose a dataset (or multiple!) - either one of your own,
# or one of the datasets that come with R.

# e.g. reading our data file "data_file.xlsx" into R and naming the object 'dat'.
dat = read_excel('C:/Users/CMADSEN/Downloads/data_folder/data_file.xlsx')
# Replace the file path to your dataset of interest, if you have any!

# Some datasets that come with R that you could try: CO2, iris, mtcars, ToothGrowth, etc.


# ======================================================
# Step 2: Exercises!

# Tips:
# 1. Type a portion of a function such as "geom_" to see a list of options from
#    packages you have loaded in with the library() function.
#
# 2. To connect lines of ggplot2 plotting logic,
#    use the '+' symbol.
#
# 3. If you want to connect two lines of R code so that
#    the result of the first line gets fed in to the code on line 2,
#    you can typically use this 'pipe' symbol: |>
#    E.g. my_data |>
#            ggplot() +
#              geom_point(aes(x = x, y = y))







# - - - - - -
# Can you make a histogram of your plot? Check out what geoms are available to you
# through ggplot2. Don't forget to initialize your plot by calling ggplot()!







# - - - - - -
# Take your plotting code and, if you haven't already, save it to some named object
# e.g. 'main_plot','my_plot','plot_one', or any other name that makes sense to you.
# Now you can quickly call up the basic plot you have already set up above!






# - - - - - -
# Can you make a plot (any geometry) and make the colour (or fill) depend on
# some numerical value in your dataset? How about a categorical variable?






# - - - - - -
# Now add your own colour palette with 3 separate colours (hint: scale_colour_gradient2() )
# Pick your colours from any website that helps you find the hex codes for special colours,
# like this one: https://htmlcolorcodes.com/color-picker/






# - - - - - -
# Try adding a title, label, and caption to your plot. What function do you use to do this?






# - - - - - -
# If you have one or more categorical variables in your dataset (e.g. a grouping variable),
# can you split your plot into separate facets based on that variable?





# - - - - - -
# Try saving your plot to a folder on your computer; can you save it with a width
# of 10 inches and a height of 6.5? Can you figure out how to change the units
# for width and height to centimeters?





# - - - - - -
#   Big challenge!
#
# Can you take the iris dataset and make the species variable into an ordered factor so
# that the order is: virginica, versicolor, setosa? Then, make a plot that uses
# this ordering for the species variable.
# Note: Here's a way to make a new variable that is a factor:

example_dat = data.frame(some_letters = c("D","E","A","B","C")) #Making example dataset.

example_dat # Run to see current order. One variable named 'some_letters'. Bad order though!

# 'mutate()' is a function that makes a new column.
example_dat = example_dat |>
  mutate(ordered_letters = factor(some_letters, # Here I specify which variable to make a factor from.
                                  levels = c("A","B","C","D","E") # Here I specify the levels by hand.
                                  ))

example_dat$ordered_letters # The variable ordered letters has ordered levels.

# We can re-order our rows using our new column and save the outcome to a new variable like this:
example_dat_ordered = example_dat |>
  arrange(ordered_letters)

# Ok, back to the plot!



