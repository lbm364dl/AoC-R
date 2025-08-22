# d07_recursive.r is much faster

concatenation <- function(a, b) {
  k <- floor(log10(b)) + 1
  a * (10^k) + b
}

solve <- function(equations, ops) {
  terms <- equations |> dplyr::select(starts_with("term"))
  results <- equations |> dplyr::pull(result)
  repeated_ops <- rep(list(ops), length(terms) - 1)
  some_terms <- terms |> dplyr::select(-term_1)
  term_1 <- equations$term_1

  is_valid <- tidyr::expand_grid(!!!repeated_ops) |>
    purrr::pmap(c) |>
    purrr::map(
      ~ purrr::reduce2(
        some_terms,
        .x,
        \(acc, term, op) op(acc, term),
        .init = term_1
      ),
      .progress = TRUE
    ) |>
    do.call(rbind, args = _) |>
    t() |>
    (`==`)(results) |>
    apply(1, any)

  equations |>
    dplyr::filter(is_valid) |>
    dplyr::pull(result) |>
    sum()
}

equations <- "2024/d07/input.txt" |>
  readr::read_delim(
    delim = ": ",
    col_names = c("result", "term")
  ) |>
  tidyr::separate_wider_delim(
    term,
    delim = " ",
    names_sep = "_",
    too_few = "align_start"
  ) |>
  dplyr::mutate(
    across(everything(), ~ tidyr::replace_na(as.double(.x), 0)),
  )

solve(equations, ops = c(`+`, `*`)) |>
  format(scientific = FALSE)

solve(equations, ops = c(`+`, `*`, concatenation)) |>
  format(scientific = FALSE)
