# ===== R/check_input.R =====
#' Check knapsack input (internal)
#' @param x A data.frame with exactly two finite, positive numeric columns v and w.
#' @param W One finite, positive numeric knapsack capacity.
#' @keywords internal
check_input <- function(x, W) {
  if (!is.data.frame(x)) stop("x must be a data.frame")
  if (ncol(x) != 2 || !all(c("v", "w") %in% names(x))) stop("x must have exactly the columns v and w")
  if (!is.numeric(x$v) || is.complex(x$v) || !is.null(dim(x$v)) ||
      !is.numeric(x$w) || is.complex(x$w) || !is.null(dim(x$w))) {
    stop("v and w must be numeric vectors")
  }
  if (any(!is.finite(x$v)) || any(!is.finite(x$w)) ||
      any(x$v <= 0) || any(x$w <= 0)) stop("v and w must be finite and positive")
  if (!is.numeric(W) || is.complex(W) || length(W) != 1 ||
      !is.finite(W) || W <= 0) stop("W must be a single finite positive number")
}

#' Brute force knapsack
#'
#' Solves the 0/1 knapsack problem by checking all non-empty subsets.
#' This exponential search supports at most 30 items and is intended for small inputs.
#' @param x A data.frame with finite positive numeric columns \code{v} (value) and \code{w} (weight).
#' @param W Knapsack capacity (one finite positive number).
#' @param parallel Logical. If TRUE, the search is split over all detected cores.
#' @return A list with \code{value} (maximum total value, without rounding) and
#'   \code{elements} (row indices chosen). Empty or infeasible inputs return zero
#'   value and no elements.
#' @examples
#' x <- data.frame(w = c(10, 20, 30), v = c(60, 100, 120))
#' brute_force_knapsack(x, W = 50)
#' @export
brute_force_knapsack <- function(x, W, parallel = FALSE) {
  check_input(x, W)
  if (!is.logical(parallel) || length(parallel) != 1L || is.na(parallel)) {
    stop("parallel must be TRUE or FALSE")
  }
  n <- nrow(x); w <- x$w; v <- x$v
  if (n == 0L) return(list(value = 0, elements = integer(0)))
  if (n > 30L) stop("brute force supports at most 30 items")

  best_in_range <- function(range) {
    best_value <- 0; best_elements <- integer(0)
    for (i in range) {
      chosen <- which(intToBits(i)[1:n] == 1)
      if (sum(w[chosen]) <= W) {
        total_v <- sum(v[chosen])
        if (total_v > best_value) { best_value <- total_v; best_elements <- chosen }
      }
    }
    list(value = best_value, elements = best_elements)
  }

  all_i <- 1:(2^n - 1)
  if (!parallel) {
    res <- best_in_range(all_i)
  } else {
    cores <- suppressWarnings(parallel::detectCores())
    if (is.na(cores)) cores <- 1L
    if (nzchar(Sys.getenv("_R_CHECK_LIMIT_CORES_"))) cores <- min(cores, 2L)
    cores <- min(cores, length(all_i))

    chunks <- split(all_i, ceiling(seq_along(all_i) / ceiling(length(all_i) / cores)))
    if (.Platform$OS.type == "windows") {
      cl <- parallel::makeCluster(cores)
      on.exit(parallel::stopCluster(cl))
      results <- parallel::parLapply(cl, chunks, best_in_range)
    } else {
      results <- parallel::mclapply(chunks, best_in_range, mc.cores = cores)
    }
    values <- vapply(results, function(r) r$value, numeric(1))
    res <- results[[which.max(values)]]
  }
  list(value = res$value, elements = res$elements)
}

# ===== R/knapsack_dynamic.R =====
#' Dynamic programming knapsack
#'
#' Solves the 0/1 knapsack problem exactly in O(W n) time (weights must be integers).
#' @inheritParams brute_force_knapsack
#' @details Both weights and capacity must be integers. The matrix uses O(n W)
#'   memory, so large capacities can require substantial memory.
#' @return A list with \code{value} and \code{elements}.
#' @examples
#' x <- data.frame(w = c(10, 20, 30), v = c(60, 100, 120))
#' knapsack_dynamic(x, W = 50)
#' @export
knapsack_dynamic <- function(x, W) {
  check_input(x, W)
  if (W != floor(W) || W > .Machine$integer.max - 1 || any(x$w != floor(x$w))) {
    stop("dynamic programming requires integer weights and an integer W within R's matrix limits")
  }
  n <- nrow(x); w <- x$w; v <- x$v
  if (n == 0L) return(list(value = 0, elements = integer(0)))
  # row i+1 = using the first i items, column j+1 = capacity j
  m <- matrix(0, nrow = n + 1, ncol = W + 1)
  for (i in 1:n) {
    m[i + 1, ] <- m[i, ]
    if (w[i] <= W) {
      idx <- (w[i] + 1):(W + 1)
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
  list(value = m[n + 1, W + 1], elements = elements)
}

# ===== R/greedy_knapsack.R =====
#' Greedy knapsack heuristic
#'
#' Sorts feasible items by value per weight and takes the prefix that fits.
#' Returns the better of that prefix and the best feasible single item, which
#' guarantees at least half the optimum. Runs in O(n log n).
#' @inheritParams brute_force_knapsack
#' @return A list with \code{value} and \code{elements}.
#' @examples
#' x <- data.frame(w = c(10, 20, 30), v = c(60, 100, 120))
#' greedy_knapsack(x, W = 50)
#' @export
greedy_knapsack <- function(x, W) {
  check_input(x, W)
  feasible <- which(x$w <= W)
  if (length(feasible) == 0L) return(list(value = 0, elements = integer(0)))
  ord <- feasible[order(x$v[feasible] / x$w[feasible], decreasing = TRUE)]
  k <- sum(cumsum(x$w[ord]) <= W)
  chosen <- ord[seq_len(k)]
  best_single <- feasible[which.max(x$v[feasible])]
  if (x$v[best_single] > sum(x$v[chosen])) chosen <- best_single
  list(value = sum(x$v[chosen]), elements = chosen)
}
