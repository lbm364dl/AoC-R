library("stringr")
library("dplyr")
library("tibble")
library("tidyr")

inp <- readLines("2023/d02/input.txt")

games <- tibble("game" = inp) |>
  separate_wider_delim(game, ": ", names = c("idx", "game")) |>
  mutate(idx = as.integer(str_extract(idx, "\\d+"))) |>
  separate_longer_delim(game, "; ") |>
  mutate(subset = row_number()) |>
  separate_longer_delim(game, ", ") |>
  separate_wider_delim(game, " ", names = c("num_cubes", "color")) |>
  mutate(num_cubes = as.integer(num_cubes))

df_max_cubes <- tibble(
  max_cubes = c(12, 13, 14), color = c("red", "green", "blue")
)

star1 <- games |>
  inner_join(df_max_cubes, by = "color") |>
  group_by(idx) |>
  filter(all(num_cubes <= max_cubes)) |>
  distinct(idx) |>
  sum()

star2 <- games |>
  group_by(idx, color) |>
  summarise(mx_of_color = max(num_cubes)) |>
  group_by(idx) |>
  summarise(mult = prod(mx_of_color)) |>
  select(mult) |>
  sum()

print(str_glue("Star 1: {star1}"))
print(str_glue("Star 2: {star2}"))
