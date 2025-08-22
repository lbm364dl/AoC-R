# d07_expand_grid.r is slower but has interesting approach
# to generate all operation combinations without recursion

# A safer solution would include:
# - Using bit64::integer64 types (here assuming correct answer in double)
#   (this is correct if answer is up to 1e15 approx.)
# - Using paste0 concatenation or computing logs/powers without doubles
# Both of these safety choices make the code slower, and were not included.
concatenation <- function(a, b) {
  k <- floor(log10(b)) + 1
  a * (10^k) + b
}

compute <- function(terms, results, ops) {
  if (length(terms) == 1) {
    return(terms[[1]] == results)
  }
  t1 <- terms[[1]]
  t2 <- terms[[2]]
  rest <- terms[-c(1, 2)]
  purrr::reduce(
    ops,
    \(acc, op) acc | compute(c(list(op(t1, t2)), rest), results, ops),
    .init = FALSE
  )
}

solve <- function(equations, ops) {
  is_valid <- compute(
    terms = equations |> dplyr::select(starts_with("term")),
    results = equations |> dplyr::pull(result),
    ops = ops
  )

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
