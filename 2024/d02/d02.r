library("purrr")
library("stringr")

inp <- readLines("2024/d02/input.txt") |>
  strsplit(" ") |>
  map(as.integer)

is_valid <- function(levels) {
  dist <- diff(levels)
  (all(dist >= 0) || all(dist <= 0)) &&
    all(abs(dist) >= 1) &&
    all(abs(dist) <= 3)
}

is_almost_valid <- function(levels) {
  levels |>
    seq() |>
    some(~ is_valid(levels[-.x]))
}

star1 <- inp |>
  map_int(is_valid) |>
  sum()

star2 <- inp |>
  map_int(is_almost_valid) |>
  sum()

print(str_glue("Star 1: {star1}"))
print(str_glue("Star 2: {star2}"))
