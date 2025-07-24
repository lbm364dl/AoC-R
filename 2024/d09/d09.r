inp <- "2024/d09/input.txt" |>
  readr::read_file() |>
  stringr::str_split_1("") |>
  head(-1) |>
  purrr::map_int(as.integer)

arr <- inp |>
  purrr::imap(
    function(x, i) {
      if (i %% 2 == 1) {
        rep(i %/% 2, x)
      } else {
        rep(NA, x)
      }
    }
  ) |>
  purrr::list_c()

df <- tibble::tibble(
  pos = as.integer(seq(arr) - 1),
  val = as.integer(arr)
)

empty <- df |>
  dplyr::filter(is.na(val))

filled <- df |>
  dplyr::filter(!is.na(val))

tibble::tibble(
  pos = empty |>
    dplyr::pull(pos),
  val = filled |>
    dplyr::pull(val) |>
    rev() |>
    head(nrow(empty))
) |>
  dplyr::bind_rows(filled) |>
  dplyr::arrange(pos) |>
  dplyr::slice_head(n = sum(!is.na(arr))) |>
  dplyr::mutate(pos * val) |>
  dplyr::pull() |>
  sum() |>
  format(scientific = FALSE)
