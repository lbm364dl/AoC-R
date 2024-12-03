library("readr")
library("stringr")
library("purrr")

inp <- read_file("2024/d03/input.txt")

reduce_fun <- function(acc, x) {
  is_do_dont <- x %in% c("do", "don't")
  new_last <- if (is_do_dont) x else acc$last_order
  maybe_new_op <- if (!is_do_dont && acc$last_order == "do") x
  list(last_order = new_last, ops = c(acc$ops, maybe_new_op))
}

compute_result <- function(mul_operations) {
  mul_operations |>
    str_extract_all("\\d+") |>
    map_int(compose(prod, as.integer)) |>
    sum()
}

star1 <- inp |>
  str_extract_all("mul\\((\\d+),\\d+\\)", simplify = TRUE) |>
  compute_result()


star2 <- inp |>
  str_extract_all("don't|do|mul\\((\\d+),\\d+\\)", simplify = TRUE) |>
  reduce(reduce_fun, .init = list(last_order = "do", ops = c())) |>
  pluck("ops") |>
  compute_result()

print(str_glue("Star 1: {star1}"))
print(str_glue("Star 2: {star2}"))
