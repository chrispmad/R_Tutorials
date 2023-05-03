# As an open-source coding community, we R users nearly always 'stand on the
# shoulders of giants' to carry out our tasks - in other words, we download
# libraries of code that other people have written and we use the imported
# functions to greatly speed up our own work.

# One of the most commonly used packages is actually a package of ... packages.
# The {tidyverse} consists of ~ 8 to 10 packages, each of which is very useful
# for typical data cleaning / transformation etc.

# In brief, the packages do the following:
# 1. dplyr: lots of functions for working with tables of data.
# 2. readr: good for reading in .CSV files.
# 3. forcats: good for working with factors (i.e. ordered character
#             labels, e.g. the four seasons, since spring -> summer -> fall etc.)
# 4. stringr: super useful for working with text / character strings.
# 5. ggplot2: probably the most commonly used plotting package in R
# 6. tibble: gives an updated and superior kind of dataframe.
# 7. lubridate: good for working with dates.
# 8. tidyr: more functions for working with tables of data.
# 9. purrr: good for applying customized functions to data in interesting ways;
#           it's a bit complex.

# If you have not previously installed the {tidyverse} package,
# run the following line of code.

install.packages('tidyverse')

# Once that's finished, you can now import it to your R session:

library(tidyverse)

# Let's read in our data from last session.
fav_s = read_csv('output/fav_season_results.csv')

# Want to see what it looks like, to jog your memory? We can either
# just call the variable's name and check it out in the console,
# or we can use 'View()' to look at the entire table.

fav_s

View(fav_s)

# Let's kick of a bar plot. We create a blank ggplot2 canvas with 'ggplot()'
# Then, we add things to this plot using the functions named in this way:
# "geom_X" (e.g. geom_point(), geom_bar(), geom_ribbon(), etc.)

ggplot(data = fav_s) +
  geom_col(aes(x = season, y = responses))

# Note the '+' that connects lines of ggplot2 code. Also note the
# 'aes()' inside the geom_col() - this stands for 'aesthetic', and we need
# to use that when setting plotting arguments to depend on columns in our data.

# If we want to set a plotting argument to a value NOT in our dataset,
# we can do it outside the aes() piece. E.g.

ggplot(data = fav_s) +
  geom_col(fill = 'lightblue',
           col = 'blue',
           aes(x = season, y = responses))

# Looking a bit better! In addition to adding "geoms" to our ggplot,
# we can also customize the theme of the plot: text size,
# background colours, legend position, axis labels, etc.
# Some of these options can be found in scale_x_y (e.g. scale_colour_manual,
# scale_y_continuous, etc.), or in theme(), which has seemingly limitless
# options...

ggplot(data = fav_s) +
  geom_col(fill = 'lightblue',
           col = 'blue',
           size = 1,
           aes(x = season, y = responses)) +
  labs(title = 'Here is a Title!',
       subtitle = '(and a subtitle)',
       y = 'Number of Votes',
       x = 'Season of the Year') +
  scale_y_continuous(limits = c(0,60)) +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        panel.background = element_rect(fill = 'white'),
        panel.grid = element_line(color = 'black'))

# There are also a range of pre-made themes that we can easily apply to
# our plots, e.g. theme_minimal(), theme_bw(), theme_classic(), etc.

# In addition, we can save the code to render a ggplot in a variable.
# This makes it easy to save the plot as a JPG or PNG on our computer.

basic_plot = ggplot(data = fav_s) +
  geom_col(fill = 'lightblue',
           col = 'blue',
           size = 1,
           aes(x = season, y = responses)) +
  labs(title = 'Here is a Title!',
       subtitle = '(and a subtitle)',
       y = 'Number of Votes',
       x = 'Season of the Year') +
  scale_y_continuous(limits = c(0,60))

basic_plot

# Let's apply a pre-made theme.
basic_plot +
  theme_bw()

# And let's write this ggplot figure to our computer.
ggsave('output/basic_ggplot.jpg', basic_plot, height = 6, width = 8, dpi = 150)

