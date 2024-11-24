library("stringr")
library("stringi")
library("purrr")

find_first <- function(inp, nums) {
  # create regex with all possible matches ("match1|match2|...")
  # "|" means "OR" in regex
  pattern <- paste(nums, collapse = "|")

  inp |>
    # find first match for each string
    str_extract(pattern) |>
    # find its position in nums vector for each found result
    match(nums) |>
    # find the number it represents by obtaining modulo 10
    # nums is especially crafted for this to work, e.g.
    # "one" is at position 1, "nine" is at position 9,
    # "1" is at position 11 (which is 1 modulo 10),
    # "9" is at position 19 (which is 9 modulo 10)...
    (`%%`)(10)
}

# regex match with str_extract_all might not work as you think
# e.g. in "oneight" it would only return "one" because it looks for
# other matches after the end of current match, i.e.,
# it finds "one" and then checks the rest of the string "ight",
# so it does not find "eight". This is why we use a different strategy:
#   - Find first match in original string ("one" in "oneight")
#   - Find first reversed match in reversed string ("thgie" in "thgieno")
# This way we would correctly obtain both "one" and "eight" from "oneight"
solve <- function(inp, nums) {
  map2_int(
    find_first(inp, nums),
    find_first(stri_reverse(inp), stri_reverse(nums)),
    ~ 10 * .x + .y
  ) |> sum()
}

digits <- as.character(1:9)
all_nums <- c(
  "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
  digits
)

inp <- readLines("2023/d01/input.txt")
print(str_glue("Star 1: {solve(inp, digits)}"))
print(str_glue("Star 2: {solve(inp, all_nums)}"))
