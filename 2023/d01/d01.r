library("stringr")
library("stringi")
library("purrr")

find_first <- function(inp, nums) {
  pattern <- paste(nums, collapse = "|")
  inp |>
    str_extract_all(pattern) |>
    map_int(~ match(.x, nums)[1] %% 10)
}

solve <- function(inp, nums) {
  map2_int(
    find_first(inp, nums),
    find_first(stri_reverse(inp), stri_reverse(nums)),
    ~ 10 * .x + .y
  ) |> sum()
}

digits <- as.character(1:9)
all_nums <- c(
  "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
  digits
)

inp <- readLines("2023/d01/input.txt")
print(str_glue("Star 1: {solve(inp, digits)}"))
print(str_glue("Star 2: {solve(inp, all_nums)}"))
