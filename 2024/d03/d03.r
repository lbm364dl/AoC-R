library("readr")
library("stringr")
library("purrr")

inp <- paste0("do()", read_file("2024/d03/input.txt"), "don't()")

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
  str_extract_all("do\\(\\)(\n|.)*?don't\\(\\)", simplify = TRUE) |>
  str_flatten() |>
  str_extract_all("mul\\((\\d+),\\d+\\)", simplify = TRUE) |>
  compute_result()

print(str_glue("Star 1: {star1}"))
print(str_glue("Star 2: {star2}"))
