future::plan(
  future::multisession,
  workers = as.integer(parallelly::availableCores() * 0.5)
)

inp <- "2024/d06/input.txt" |>
  readLines() |>
  stringr::str_split("", simplify = TRUE)

n <- dim(inp)[1]
m <- dim(inp)[2]
pos <- which(inp == "^", arr.ind = TRUE)

dy <- c(-1, 0, 1, 0)
dx <- c(0, 1, 0, -1)

is_inside <- function(y, x) {
  1 <= y && y <= n && 1 <= x && x <= m
}

solve <- function(grid) {
  y <- pos[1]
  x <- pos[2]
  delta <- 1
  vis <- array(FALSE, c(n, m, 4))

  while (!vis[y, x, delta]) {
    vis[y, x, delta] <- TRUE
    for (k in 0:3) {
      try_delta <- (delta + k - 1) %% 4 + 1
      ny <- y + dy[try_delta]
      nx <- x + dx[try_delta]
      if (!is_inside(ny, nx)) {
        return(
          vis |>
            apply(c(1, 2), any) |>
            which(arr.ind = TRUE)
        )
      }
      if (grid[ny, nx] != "#") {
        delta <- try_delta
        break
      }
    }
    y <- y + dy[delta]
    x <- x + dx[delta]
  }

  -1
}

squares <- solve(inp)
star1 <- nrow(squares)

can_make_loop <- function(square) {
  i <- square[1]
  j <- square[2]
  if (pos[1] == i && pos[2] == j) {
    return(FALSE)
  }
  prev <- inp[i, j]
  inp[i, j] <- "#"
  res <- solve(inp)
  inp[i, j] <- prev
  return(identical(res, -1))
}

message("Computing number of possible loops...")
star2 <- squares |>
  purrr::array_branch(1) |>
  furrr::future_map_lgl(can_make_loop, .progress = TRUE) |>
  sum()

print(stringr::str_glue("Star 1: {star1}"))
print(stringr::str_glue("Star 2: {star2}"))
