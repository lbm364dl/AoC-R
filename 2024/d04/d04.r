library("stringr")
library("purrr")
library("stringi")

inp <- readLines("2024/d04/input.txt")

res <- matrix(inp)

n <- length(inp)
m <- str_length(inp[1])

occs <- function(text) {
  text |>
    str_extract_all("XMAS") |>
    map_int(length) |>
    sum()
}

mat <- matrix("", nrow = 2 * n, ncol = 2 * m)
for (i in seq(inp)) {
  row <- str_split(inp[i], "")[[1]]
  for (j in seq(row)) {
    mat[i - j + m, i + j] <- row[j]
  }
}

mat <- apply(mat, 1, \(x) paste0(x, collapse = ""))

vert_mat <- do.call(rbind, str_split(inp, "")) |>
  t() |>
  apply(1, str_flatten)


hor1 <- occs(inp)
hor2 <- inp |>
  stri_reverse() |>
  occs()

ver1 <- occs(vert_mat)
ver2 <- vert_mat |>
  stri_reverse() |>
  occs()

diag1 <- occs(mat)
diag2 <- mat |>
  stri_reverse() |>
  occs()

mat <- matrix("", nrow = 2 * n, ncol = 2 * m)
for (i in seq(inp)) {
  row <- str_split(inp[i], "")[[1]]
  for (j in seq(row)) {
    mat[i + j, i - j + m] <- row[j]
  }
}
mat <- apply(mat, 1, \(x) paste0(x, collapse = ""))

diag3 <- occs(mat)
diag4 <- mat |>
  stri_reverse() |>
  occs()

star1 <- hor1 + hor2 + ver1 + ver2 + diag1 + diag2 + diag3 + diag4

star2 <- 0
mat <- do.call(rbind, str_split(inp, ""))
for (i in 1:(n - 2)) {
  for (j in 1:(m - 2)) {
    d1 <- str_flatten(c(mat[i, j], mat[i + 1, j + 1], mat[i + 2, j + 2]))
    d2 <- str_flatten(c(mat[i + 2, j], mat[i + 1, j + 1], mat[i, j + 2]))
    if (all(c(d1, d2) %in% c("MAS", "SAM"))) {
      star2 <- star2 + 1
    }
  }
}

print(str_glue("Star 1: {star1}"))
print(str_glue("Star 2: {star2}"))
