find_first <- function(inp, nums) {
  pattern <- paste(nums, collapse = "|")
  inp |>
    stringr::str_extract_all(pattern) |>
    lapply(\(x) match(x, nums)[1] %% 10)
}

solve <- function(inp, nums) {
  mapply(
    \(x, y) 10 * x + y,
    find_first(inp, nums),
    find_first(stringi::stri_reverse(inp), stringi::stri_reverse(nums))
  ) |> sum()
}

digits <- as.character(1:9)
all_nums <- c(
  "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
  digits
)

inp <- readLines("2023/d01/input.txt")
print(stringr::str_glue("Star 1: {solve(inp, digits)}"))
print(stringr::str_glue("Star 2: {solve(inp, all_nums)}"))
