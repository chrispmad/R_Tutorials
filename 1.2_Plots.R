
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
# This is analogous to taking the toolbox down from the shelf, placing it on your
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
# "geom_X" (e.g. geom_point(), geom_bar(), geom_ribbon(), etc.). We need
# to specify what dataset we would like to visualize; we can do this either
# in the ggplot() function call, or in the geom_col(). If we specify the
# dataset in the former, that dataset is then 'inherited' by all subsequent
# ggplot arguments (e.g. a geometry like our geom_col), whereas if we specify
# the dataset in the geom_col, it's only specified inside that geometry call.

ggplot(data = fav_s) +
  geom_col(aes(x = season, y = responses))

# Note the '+' that connects lines of ggplot2 code. Also note the
# 'aes()' inside the geom_col() - this stands for 'aesthetic', and we need
# to use that when setting plotting arguments to depend on columns in our data.
ggplot(data = fav_s) +
  geom_col(
    aes(x = season,
        y = responses,
        color = season, # This is the colour of the outline.
        fill =  percent), # This is the colour of the column fill.
    # Here I specify the width of the border line. Note that setting
    # this plotting variable to a fixed variable that is NOT in our
    # dataset, it goes outside the aes() call.
    size = 1
  )

# Again, if we want to set a plotting argument to a value NOT in our dataset,
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
# options... We will break down each part of this complicated
# plotting code block, so don't worry about trying to understand
# every line at this point!

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

# Let's go through each of these subtopics one by one.

#======================================
# Colour palettes

# To colour plot elements based on their value for some variable,
# you must specify either "col = x" or "fill = x" inside the aes() argument
# of either the ggplot() call that starts a plot, or inside the geom_x() argument
# that you use to add some geometry to your plot. E.g.

ggplot(CO2) +
  geom_point(aes(
    x = conc,
    y = uptake,
    col = Type #The colour of the points now depends on the Type column.
  ))

# One quick way to give a plot an identifiable style is to change the
# colour palette. Ggplot starts off with a good palette, but once you've
# seen those colours 100 times, you may want some novelty...
# First, check out the different colour palettes included with ggplot.

RColorBrewer::display.brewer.all()


# There are monodirectional palettes at the top (white to saturated colour)
# unordered colour palettes in the middle (I quite like Dark2)
# and diverging palettes at the bottom.

# One thing to be cautious of: is the variable you're assigning colour to
# continuous (i.e. numerical values like, e.g., tree diameter) or discrete
# (i.e. categorical, like size classes for trees). If the former, use
# scale_colour_continuous (or scale_fill_continuous if adjusting the fill colour),
# if the latter, use scale_colour_discrete.

# Note: for colour-blind-friendly palettes, try out the 'viridis' options
# scale_colour_viridis_d() for discrete values (categorical),
# scale_colour_viridis_c() for continuous values (numerical)
ggplot(CO2) +
  geom_point(aes(
    x = conc,
    y = uptake,
    col = Type #The colour of the points now depends on the Type column.
  )) +
  scale_colour_viridis_d()

# We can also draw from that RColorBrewer set of colour palettes
# with the scale_colour_brewer() and/or scale_fill_brewer() functions
ggplot(CO2) +
  geom_point(aes(
    x = conc,
    y = uptake,
    col = Type #The colour of the points now depends on the Type column.
  ),
  size = 3) +
  scale_colour_brewer(palette = 'Dark2')

# Can you change the palette above to Set2? How about another palette
# from the RColorBrewer::display.brewer.all()? Try it out!

# You can make a customized colour palette with a
# gradient with scale_colour_gradientn() or scale_colour_gradient2(), or their
# fill equivalents.

ggplot(data = fav_s) +
  geom_col(size = 1,
           aes(x = season,
               y = responses,
               fill = responses)) +
  scale_fill_gradient2(low = 'purple', mid = 'beige', high = '#61C46E', midpoint = 25)

# Notice the colour I've assigned to the argument 'high'? This is a hex colour,
# hex standing for 'hexadecimal code'.
# They are extremely useful when customizing your colour palette.
# Try making your own colour(s) here: https://htmlcolorcodes.com/color-picker/

#======================================
# Labels

# You can set almost any kind of label with the labs() function.
# In addition to title, subtitle, and axis titles, you can also change the
# legend title for a variable. For example:
ggplot(iris) +
  geom_point(aes(x = Petal.Width, y = Petal.Length, size = Petal.Width * Petal.Length)) +
  labs(title = 'Iris Petal Dimensions',
       subtitle = 'Flowers are great',
       x = 'Petal Width (mm)',
       y = 'Petal Length (mm)')

# Let's say I'd like to change that legend title to 'Petal Area'
ggplot(iris) +
  geom_point(aes(x = Petal.Width, y = Petal.Length, size = Petal.Width * Petal.Length)) +
  labs(title = 'Iris Petal Dimensions',
       subtitle = 'Flowers are great',
       x = 'Petal Width (mm)',
       y = 'Petal Length (mm)',
       size = 'Petal Area (mm^2)')

#======================================
# Reordering plot elements

# There are 2 ways to reorder plot elements:
# 1. use the 'reorder()' function when calling a geom_x().
# 2. Organize your data in whatever way you choose before starting the plot and
#    convert the ordered variable into a factor so that it's order is explicit.

# E.g. 1
ggplot(data = fav_s) +
  geom_col(size = 1,
           aes(x = reorder(season,-responses),
               y = responses,
               fill = responses))

# E.g. 2
fav_s |>
  arrange(desc(responses)) |>
  mutate(season = forcats::fct_inorder(season)) |>
  ggplot() +
  geom_col(size = 1,
           aes(x = season,
               y = responses,
               fill = responses))
# This second method is preferable when your organizing logic is quite complicated
# or illogical. This method depends on temporarily changing our dataset (e.g.
# in how it's organized) and then 'piping' that altered dataset directly
# into our ggplot code chunk.

#======================================
# Themes

# There are also a range of pre-made themes that we can easily apply to
# our plots, e.g. theme_minimal(), theme_bw(), theme_classic(), etc.

# Here we first save our ggplot code to an object named 'basic_plot'
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

# We can see the result of our plotting code by calling the object's name.
basic_plot

# And we can apply a pre-made theme by attaching it to this object.
basic_plot +
  theme_bw()
# Note that we can also add themes in to the original ggplot code block;
# this was merely to demonstrate that you can create partially completed
# plots, save them to object(s), and then add more ggplot code to the
# object later on in your script!

# If you would like more themes, one package to consider using is {ggthemes},
# which comes with theme_map(), theme_economist(), theme_excel(), etc.
# Another option is {ggpubr}, which has theme_cleveland() and theme_pubr().

# Beyond these pre-made themes, we can alter almost any aspect of our plots
# using the theme() function.

# Note: if you would like to use some other font, we can use the {extrafont}
# pacakge.
library(extrafont)

# font_import() # Do this line just once on your computer.

loadfonts(device='win', quiet=TRUE) # You must run this line every R session.

# Note that we are also specifying some elements of the plot through the
# theme() function call. There are so many options in here! It can be
# a little tricky to find exactly the element you mean. Here are a few examples.
basic_plot +
  theme(plot.title = element_text(size = 20, colour = '#A0BDFC', family = 'MV Boli'),
        plot.subtitle = element_text(size = 14, colour = '#BACFFB'),
        axis.title = element_text(colour = 'white', size = 16, family = 'Comic Sans MS'),
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1, vjust = 1, colour = '#E8EEFB'),
        plot.background = element_rect(fill = 'black'),
        panel.background = element_rect(fill = '#050D1E'),
        panel.grid.major = element_line(colour = 'lightgrey', linetype = 1),
        panel.grid.minor = element_line(colour = 'lightgrey', linetype = 2))



#======================================
# Custom colours and hypertext in plots

# We can also add hypertext(?) stuff like superscript, subscript, and colours.
# To do so, we can rely on the {ggtext} package and a teensy bit of HTML...
# install.packages('ggtext')
library(ggtext)

# To add hypertext, it is a two-step process:
# 1. Add HTML to some label in your plot
# 2. Use ggtext in the theme() function to specify that you would like that
#    aspect of the plot to be rendered using ggtext's "element_markdown()"

# Superscript and subscript are simple: <sup> X </sup>

ggplot(iris) +
  geom_point(aes(x = Petal.Width, y = Petal.Length, size = Petal.Width * Petal.Length)) +
  labs(title = 'Iris Petal Dimensions',
       subtitle = 'Flowers are great',
       x = 'Petal Width (mm)',
       y = 'Petal Length (mm)',
       size = 'Petal Area (mm<sup>2</sup>)') +
  theme(legend.title = element_markdown())

# Colouring individual words in the title relies on writing some HTML...

ggplot(iris) +
  geom_point(aes(x = Petal.Width, y = Petal.Length, size = Petal.Width * Petal.Length)) +
  labs(title = "Iris Petal Dimensions of <span style='color:#5F4EEE;'>setosa</span>,
       <span style='color:#8726DD;'>versicolor</span>, and
       <span style='color:#6E31EC;'>virginica</span>",
       subtitle = 'Flowers are great',
       x = 'Petal Width (mm)',
       y = 'Petal Length (mm)',
       size = 'Petal Area (mm<sup>2</sup>)') +
  theme(legend.title = element_markdown(),
        plot.title = element_markdown())


#======================================
# Saving Plots to disk programmatically

# We can save the code to render a ggplot in a variable.
# This makes it easy to save the plot as a JPG or PNG on our computer.

# Let's write this ggplot figure to our computer.
ggsave('output/basic_ggplot.jpg', basic_plot, height = 6, width = 8, dpi = 150)

#======================================
# Facetting

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

# install.packages("plotly")

library(plotly)

ggplotly(basic_plot)

# Or you can skip ggplot entirely and make plots directly with {plotly}
# It's a bit confusing, but the function to start a plotly figure is "plot_ly()"
# volcano is a numeric matrix that ships with R
fig <- plot_ly(z = ~volcano)
fig <- fig |> add_surface()

fig
