# One data format in R that is very useful is a list. In R, a list contains
# one or more elements, each of which can be virtually anything: a single number,
# a string of characters, a table, a model (e.g. a linear model), a plot, or
# even another list!

# Some nice things that lists can help us with:

# 1. Reading in multiple datasets in a single (or very few!) lines.

my_data_files = list.files(path = 'data/',
                           pattern = '.*\\.csv',
                           full.names = T)

my_data_list = map(my_data_files, ~ {
  read_csv(.x)
})

# The object 'my_data' is a list of length 3. Each element is
# a table of data.
my_data_list

# If we want to combine these tables into a single table, we can use
# a lovely function called 'bind_rows()'.
all_dat = my_data_list |>
  bind_rows()

# However, if we want to run the same model for each dataset separately,
# we can keep the tables as separate elements in a list and apply
# a model to each.

aov_results_l = map(my_data_list, ~ {

  # Code inside this 'map()' function can have multiple lines. It can
  # also create temporary objects that won't be saved to your global environment.
  # Whichever object is called at the end of the map will be returned to
  # your global environment; in this case, that's the summary of ANOVA.
  aov_results = aov(result ~ pred_a + pred_b + pred_c, data = .x)

  aov_results

})

# This is just one example of how you can use a list to run complicated code blocks
# for each section of your data separately. There are many other potential uses,
# though they tend to be a little complicated and harder to conceptualize.
# Lists can save you a lot of typing!

# We can convert the summary results into a more 'R-friendly' format, i.e. a table.
# To do this, let's use the {broom} package. Here's another way to use a function
# on each element of a list: lapply()

results_tidy = lapply(aov_results_l, broom::tidy)

results_tidy

# Looking pretty good... Let's combine the tables into one object so we can
# write out all the results to a single excel file. First, we name the elements.

names(results_tidy) <- c("Turkey","Fisher","Goldfish")

results_tidy

# And let's combine the elements into one table, adding a column that
# indicates the dataset.
results_for_excel = results_tidy |>
  bind_rows(.id = 'dataset')

# Now we can write the results out to an excel file. I like to use the package
# {openxlsx}, as it allows us to read and write excel files.
openxlsx::write.xlsx(results_for_excel,'output/anova_results.xlsx')
