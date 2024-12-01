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
  select(n1 = n2) |>
  group_by(n1) |>
  summarise(cnt = n()) |>
  inner_join(df, by = "n1") |>
  mutate(n1 * cnt, .keep = "none") |>
  sum()

print(str_glue("Star 1: {star1}"))
print(str_glue("Star 2: {star2}"))
