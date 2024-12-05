library("readr")
library("stringr")
library("purrr")

# I don't like this problem because the input is crafted so that all
# possible comparisons between two numbers appear in the rules, but
# the statement does not say this. This makes the problem much easier.
# But why not just mention it in the statement...
# A solution for general inputs would require topological sorting on graphs.

inp <- read_file("2024/d05/input.txt") |>
  str_split("\n\n") |>
  map(~ str_split(.x, "\n"))

rules <- inp[[1]][[1]]

pages <- inp[[1]][[2]] |>
  head(-1) |>
  str_split(",") |>
  map(as.integer)

get_order <- \(x, l) -sum(map_int(l, \(y) paste(x, y, sep = "|") %in% rules))
sort_pages <- \(l) sort_by(l, map_int(l, ~ get_order(.x, l)))
ordered_pages <- map(pages, sort_pages)
is_sorted <- mapply(identical, pages, ordered_pages)

result <- \(pages) sum(map_int(pages, ~ .x[length(.x) %/% 2 + 1]))

print(str_glue("Star 1: {result(ordered_pages[is_sorted])}"))
print(str_glue("Star 2: {result(ordered_pages[!is_sorted])}"))
