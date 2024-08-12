######################################################
# emma ? and ??
# LPRICE5
# Workshop 12 Aug 2024
######################################################

# Define packages to load ----
some_packages <- c('tibble', 'dplyr', 'tidyr', 'readr', 'stringr', 'forcats', 'purrr', 'rmarkdown','lubridate', 'janitor', 'openxlsx', 'writexl', 'readxl', 'fs', 'palmerpenguins', 'devtools', 'tidyverse','here')

# Run these first time ----
install.packages(some_packages)
# renv::status()
# renv::snapshot()

# Load all packages at once ----
lapply(some_packages, library, character.only = TRUE)

usethis::use_course("posit-conf-2024/programming-r-exercises")

## still didn't get this part solved
