solve <- function(new_y, new_x) {
  antennas |>
    dplyr::inner_join(
      antennas,
      by = "letter",
      relationship = "many-to-many",
      suffix = c("_1", "_2")
    ) |>
    dplyr::filter(col_1 != col_2 | row_1 != row_2) |>
    dplyr::rowwise() |>
    dplyr::reframe(
      dx = col_2 - col_1,
      dy = row_2 - row_1,
      new_x = {{ new_x }},
      new_y = {{ new_y }}
    ) |>
    dplyr::filter(1 <= new_x, new_x <= width, 1 <= new_y, new_y <= height) |>
    dplyr::distinct(new_y, new_x) |>
    nrow()
}

lines <- readr::read_lines("2024/d08/input.txt")
height <- length(lines)
width <- stringr::str_length(lines[1])

antennas <- lines |>
  tibble::enframe(name = "row", value = "letter") |>
  dplyr::mutate(letter = stringr::str_split(letter, "")) |>
  tidyr::unnest(letter) |>
  dplyr::mutate(col = dplyr::row_number(), .by = row) |>
  dplyr::filter(letter != ".")

solve(
  new_x = col_2 + dx,
  new_y = row_2 + dy
)

solve(
  new_x = seq(from = col_2, by = dx, length.out = width),
  new_y = seq(from = row_2, by = dy, length.out = height)
)
