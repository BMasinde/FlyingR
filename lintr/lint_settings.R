# lintr settings for the package flying ########
library(lintr)

# line length settings
my_linters <- with_defaults(line_length_linter = line_length_linter(120))

# add camelCase setting
my_linters <- with_defaults(default = my_linters,
                            +object_name_linter("camelCase"))

# you only need to run this file
#file_path = "/Users/masinde/Documents/R projects/Flight project/flying/R/migrate.R"

lint(file_path, linters = my_linters)
