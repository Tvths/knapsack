# ===== R/check_input.R =====
#' Check knapsack input (internal)
#' @param x data.frame with columns v and w
#' @param W knapsack capacity
#' @keywords internal
check_input <- function(x, W) {
  if (!is.data.frame(x)) stop("x must be a data.frame")
  if (ncol(x) != 2 || !all(c("v", "w") %in% names(x))) stop("x must have exactly the columns v and w")
  if (any(x$v <= 0) || any(x$w <= 0)) stop("v and w must be positive")
  if (!is.numeric(W) || length(W) != 1 || W <= 0) stop("W must be a single positive number")
}

# ===== R/brute_force_knapsack.R =====
#' Brute force knapsack
#'
#' Solves the 0/1 knapsack problem by checking all 2^n combinations.
#' @param x A data.frame with positive numeric columns \code{v} (value) and \code{w} (weight).
#' @param W Knapsack capacity (positive number).
#' @return A list with \code{value} (maximum total value) and \code{elements} (row indices chosen).
#' @examples
#' x <- data.frame(w = c(10, 20, 30), v = c(60, 100, 120))
#' brute_force_knapsack(x, W = 50)
#' @export
brute_force_knapsack <- function(x, W) {
  check_input(x, W)
  n <- nrow(x)
  best_value <- 0
  best_elements <- integer(0)
  for (i in 1:(2^n - 1)) {
    chosen <- which(intToBits(i)[1:n] == 1)
    total_w <- sum(x$w[chosen])
    if (total_w <= W) {
      total_v <- sum(x$v[chosen])
      if (total_v > best_value) {
        best_value <- total_v
        best_elements <- chosen
      }
    }
  }
  list(value = round(best_value), elements = best_elements)
}

# ===== R/knapsack_dynamic.R =====
#' Dynamic programming knapsack
#'
#' Solves the 0/1 knapsack problem exactly in O(W n) time (weights must be integers).
#' @inheritParams brute_force_knapsack
#' @return A list with \code{value} and \code{elements}.
#' @examples
#' x <- data.frame(w = c(10, 20, 30), v = c(60, 100, 120))
#' knapsack_dynamic(x, W = 50)
#' @export
knapsack_dynamic <- function(x, W) {
  check_input(x, W)
  n <- nrow(x); w <- x$w; v <- x$v
  # row i+1 = using the first i items, column j+1 = capacity j
  m <- matrix(0, nrow = n + 1, ncol = W + 1)
  for (i in 1:n) {
    m[i + 1, ] <- m[i, ]
    if (w[i] <= W) {
      idx <- (w[i] + 1):(W + 1)                       # vectorized inner loop
      m[i + 1, idx] <- pmax(m[i, idx], m[i, idx - w[i]] + v[i])
    }
  }
  # backtrack to find which items were used
  elements <- integer(0); j <- W
  for (i in n:1) {
    if (m[i + 1, j + 1] != m[i, j + 1]) {
      elements <- c(i, elements)
      j <- j - w[i]
    }
  }
  list(value = round(m[n + 1, W + 1]), elements = elements)
}

# ===== R/greedy_knapsack.R =====
#' Greedy knapsack heuristic
#'
#' Sorts items by value per weight and adds them until the next item does not fit.
#' Guaranteed to give at least half the optimum (with the modified version), runs in O(n log n).
#' @inheritParams brute_force_knapsack
#' @return A list with \code{value} and \code{elements}.
#' @examples
#' x <- data.frame(w = c(10, 20, 30), v = c(60, 100, 120))
#' greedy_knapsack(x, W = 50)
#' @export
greedy_knapsack <- function(x, W) {
  check_input(x, W)
  ord <- order(x$v / x$w, decreasing = TRUE)
  k <- sum(cumsum(x$w[ord]) <= W)
  chosen <- ord[seq_len(k)]
  list(value = round(sum(x$v[chosen])), elements = chosen)
}
