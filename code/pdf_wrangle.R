library(pdftools)
library(stringr)
library(readr)
library(tidyverse)
#reading in the file
temp_file <- tempfile()
url <- paste0("https://github.com/c2-d2/pr_mort_official/raw/master/data/Mortalidad-RegDem-2015-17-NYT-part1.pdf")
download.file(url, temp_file, mode = "wb")
txt <- pdf_text(temp_file)
file.remove(temp_file)
#wrangling

wrangling_sep <- function(n, month) {
  page <- txt[n]
  tab <- str_split(page, "\n+")
  tab <- tab[[1]]
  first_line <- tab[3]
  first_line
  the_month <- first_line %>%   str_trim() |>
    str_replace_all(",\\s.", "") |>
    str_split("\\s{2,}", simplify = TRUE)
  header <- the_month[1:2] %>%  str_trim() |>
    str_split("\\s+", simplify = TRUE)
  header
  the_names <- c(header[1,1], header[2,1], header[2,2], header[2,3], "Diff.")
  new_table <- tab[4:37] |>
    str_trim() |>
    str_split("\\s{2,}", simplify = TRUE) |>
    data.frame() |>
    setNames(the_names) 
  new_table
  new_table <- new_table %>% select(c("SEP", "Y2015", "Y2016", "Y2017", "Diff.")) %>% 
    filter(Y2015 != "" ,Y2015 != "Y2015")
  return (new_table) }