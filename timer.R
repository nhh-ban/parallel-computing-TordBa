library(tictoc)
library(tidyverse)

printTicTocLog <-
  function() {
    tic.log() %>%
      unlist %>%
      tibble(logvals = .) %>%
      separate(logvals,
               sep = ":",
               into = c("Function type", "log")) %>%
      mutate(log = str_trim(log)) %>%
      separate(log,
               sep = " ",
               into = c("Seconds"),
               extra = "drop")
  }

# Task 1
tic.clearlog()
tic("hw4_solution script")
source("scripts/hw4_solution.R")
toc(log = TRUE)

# Task 2
tic("hw4_solution2 script")
source("scripts/hw4_solution2.R")
toc(log = TRUE)

# Task 3
tic("hw4_solution3 script")
source("scripts/hw4_solution3.R")
toc(log = TRUE)

# Print results
printTicTocLog() %>%
  knitr::kable()
