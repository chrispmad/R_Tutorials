# As an open-source coding community, we R users nearly always 'stand on the
# shoulders of giants' to carry out our tasks - in other words, we download
# libraries of code that other people have written and we use the imported
# functions to greatly speed up our own work.

# We can use the analogy of a toolbox, containing tools that help us
# perform specific tasks. The toolbox is the package, the tools are the functions.

# One of the most commonly used packages is actually a package of ... packages.
# The {tidyverse} consists of ~ 8 to 10 packages, each of which is very useful
# for typical data cleaning / transformation etc.

# In brief, the packages do the following:
# 1. dplyr: lots of functions for working with tables of data.
# 2. readr: good for reading in (small to medium) .CSV files.
# 3. forcats: good for working with factors (i.e. ordered character
#             labels, e.g. the four seasons, since spring -> summer -> fall -> winter etc.)
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
# This is analogous to takin the toolbox down from the shelf, placing it on your
# workbench, and opening it up to get access to the tools inside.

library(tidyverse)

# Quick aside: if we want to see what the tools are that a certain package gives us,
# we can write the name of the package, followed by ::

# E.g.
#readr::
# As we type this, Rstudio gives us a dropdown box listing all the functions (and data)
#included in the package.

# If we type in a part of a function name, then mouse over the box that pops up,
# we'll be able to get a glimpse of the documentation and the arguments we could
# set for the function.

readr::read_csv()

# Let's read in our data from last session.
fav_s = read_csv(file = 'output/fav_season_results.csv')

# Want to see what it looks like, to jog your memory? We can either
# just call the variable's name and check it out in the console,
# or we can use 'View()' to look at the entire table.

fav_s

View(fav_s)

# Let's kick off a bar plot. We create a blank ggplot2 canvas with 'ggplot()'
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

# One powerful thing to be able to do is to split plots
# by some variable. For example, there are 3 species of iris flower
# in the 'iris' dataset.

# All 3 species on the same panel.
ggplot(iris) +
  geom_point(aes(x = Petal.Width, y = Petal.Length, col = Species))

# Facetting based on the Species column.
ggplot(iris) +
  geom_point(aes(x = Petal.Width, y = Petal.Length, col = Species)) +
  facet_wrap( ~ Species)

# We can specify some things about the plot facets:
# 1. The layout, i.e. how many columns and rows.
# 2. If the x and/or y axes should be 'free'
ggplot(iris) +
  geom_point(aes(x = Petal.Width, y = Petal.Length, col = Species)) +
  facet_wrap( ~ Species, nrow = 1, ncol = 3, scales = 'free')
# Note: setting scales to be 'free' can be a bit misleading to readers.

# Another powerful thing to do is to extend ggplot with another package called
# 'plotly'. We can make some rad, interactive figures with plotly; note that
# the file we produce must be an HTML file for these plotly figures to work -
# if we're producing a PDF or any other kind of non-interactive report, these
# guys will be inert.

# Check out this website to see all of the amazing plots that you can make with
# plotly: https://plotly.com/r/

install.packages("plotly")

library(plotly)

ggplotly(basic_plot)

# volcano is a numeric matrix that ships with R
fig <- plot_ly(z = ~volcano)
fig <- fig |> add_surface()

fig
