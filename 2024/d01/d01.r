library("tibble")
library("tidyr")
library("dplyr")
library("stringr")

df <- tibble(line = readLines("2024/d01/input.txt")) |>
  separate_wider_delim(line, "   ", names = c("n1", "n2")) |>
  mutate_all(as.integer)

l1 <- sort(pull(df, n1))
l2 <- sort(pull(df, n2))
star1 <- tibble(l1 = l1, l2 = l2) |>
  mutate(abs(l1 - l2), .keep = "none") |>
  sum()

star2 <- df |>
  inner_join(df, join_by(n1 == n2)) |>
  pull(n1) |>
  sum()

print(str_glue("Star 1: {star1}"))
print(str_glue("Star 2: {star2}"))
