#' Validate inputs shared by the knapsack solvers
#'
#' @param x A data frame with exactly two numeric columns, `v` (value) and
#'   `w` (weight). Values and weights must be finite and strictly positive.
#' @param W A finite, strictly positive numeric capacity.
#' @keywords internal
check_input <- function(x, W) {
  if (!is.data.frame(x)) {
    stop("`x` must be a data frame.", call. = FALSE)
  }
  if (ncol(x) != 2L || anyDuplicated(names(x)) ||
      !setequal(names(x), c("v", "w"))) {
    stop("`x` must have exactly the columns `v` and `w`.", call. = FALSE)
  }
  if (!is.numeric(x$v) || is.complex(x$v) || !is.null(dim(x$v)) ||
      !is.numeric(x$w) || is.complex(x$w) || !is.null(dim(x$w))) {
    stop("Columns `v` and `w` must be numeric.", call. = FALSE)
  }
  if (anyNA(x$v) || anyNA(x$w) || any(!is.finite(x$v)) ||
      any(!is.finite(x$w)) || any(x$v <= 0) || any(x$w <= 0)) {
    stop("Columns `v` and `w` must contain finite positive values.",
         call. = FALSE)
  }
  if (!is.numeric(W) || is.complex(W) || length(W) != 1L || is.na(W) ||
      !is.finite(W) || W <= 0) {
    stop("`W` must be one finite positive number.", call. = FALSE)
  }

  invisible(TRUE)
}

#' Solve the 0/1 knapsack problem by brute force
#'
#' Checks every non-empty subset and returns an exact solution. This method
#' takes O(2^n) time and is intended for small inputs (at most 30 items).
#' When `parallel = TRUE`, subset ranges are distributed across the detected
#' CPU cores using a cross-platform socket cluster.
#'
#' @inheritParams check_input
#' @param parallel Logical; distribute the search across detected cores when
#'   `TRUE`. The default is `FALSE`.
#' @return A list with `value`, the maximum feasible total value, and
#'   `elements`, the row indices of the selected items.
#' @examples
#' x <- data.frame(w = c(10, 20, 30), v = c(60, 100, 120))
#' brute_force_knapsack(x, W = 50)
#' @export
brute_force_knapsack <- function(x, W, parallel = FALSE) {
  check_input(x, W)
  if (!is.logical(parallel) || length(parallel) != 1L || is.na(parallel)) {
    stop("`parallel` must be TRUE or FALSE.", call. = FALSE)
  }

  n <- nrow(x)
  if (n == 0L) {
    return(list(value = 0, elements = integer(0)))
  }
  if (n > 30L) {
    stop("Brute force supports at most 30 items; use `knapsack_dynamic()` for larger inputs.",
         call. = FALSE)
  }

  weights <- x$w
  values <- x$v
  number_of_subsets <- 2^n - 1

  best_in_range <- function(bounds, weights, values, item_count, capacity) {
    best_value <- 0
    best_elements <- integer(0)
    for (mask in seq.int(bounds[[1L]], bounds[[2L]])) {
      chosen <- which(as.integer(intToBits(as.integer(mask)))[seq_len(item_count)] != 0L)
      total_weight <- sum(weights[chosen])
      if (total_weight <= capacity) {
        total_value <- sum(values[chosen])
        if (total_value > best_value) {
          best_value <- total_value
          best_elements <- chosen
        }
      }
    }
    list(value = best_value, elements = best_elements)
  }
  environment(best_in_range) <- baseenv()

  worker_count <- 1L
  if (parallel) {
    detected_cores <- suppressWarnings(parallel::detectCores(logical = FALSE))
    if (length(detected_cores) != 1L || is.na(detected_cores) || detected_cores < 1L) {
      detected_cores <- 1L
    }
    if (nzchar(Sys.getenv("_R_CHECK_LIMIT_CORES_"))) {
      detected_cores <- min(detected_cores, 2L)
    }
    worker_count <- min(as.integer(detected_cores), as.integer(number_of_subsets))
  }

  chunk_size <- ceiling(number_of_subsets / worker_count)
  chunk_starts <- seq.int(1, number_of_subsets, by = chunk_size)
  chunks <- lapply(chunk_starts, function(start) {
    c(start, min(start + chunk_size - 1, number_of_subsets))
  })

  if (worker_count > 1L) {
    cluster <- parallel::makeCluster(worker_count)
    on.exit(parallel::stopCluster(cluster), add = TRUE)
    results <- parallel::parLapply(
      cluster, chunks, best_in_range,
      weights = weights, values = values, item_count = n, capacity = W
    )
  } else {
    results <- lapply(
      chunks, best_in_range,
      weights = weights, values = values, item_count = n, capacity = W
    )
  }

  values_by_chunk <- vapply(results, `[[`, numeric(1), "value")
  results[[which.max(values_by_chunk)]]
}

#' Solve the 0/1 knapsack problem by dynamic programming
#'
#' Returns an exact solution for integer weights and an integer capacity.
#' Runtime is O(n W), with O(n W) logical backtracking storage and O(W)
#' numeric value storage.
#'
#' @inheritParams check_input
#' @return A list with `value`, the maximum feasible total value, and
#'   `elements`, the row indices of the selected items.
#' @examples
#' x <- data.frame(w = c(10, 20, 30), v = c(60, 100, 120))
#' knapsack_dynamic(x, W = 50)
#' @export
knapsack_dynamic <- function(x, W) {
  check_input(x, W)
  if (W != floor(W)) {
    stop("`W` must be an integer for dynamic programming.", call. = FALSE)
  }
  if (W > .Machine$integer.max - 1) {
    stop("`W` is too large to use as an R matrix dimension.", call. = FALSE)
  }
  if (any(x$w != floor(x$w))) {
    stop("Weights in `x$w` must be integers for dynamic programming.",
         call. = FALSE)
  }

  n <- nrow(x)
  capacity <- as.integer(W)
  # Clamp infeasible weights before integer conversion to avoid overflow for
  # finite weights larger than R's integer range.
  weights <- as.integer(pmin(x$w, capacity + 1))
  values <- x$v
  best <- numeric(capacity + 1L)
  take_item <- matrix(FALSE, nrow = n, ncol = capacity + 1L)

  for (i in seq_len(n)) {
    item_weight <- weights[[i]]
    if (item_weight <= capacity) {
      current_indices <- seq.int(item_weight + 1L, capacity + 1L)
      previous_indices <- current_indices - item_weight
      candidate_values <- best[previous_indices] + values[[i]]
      improves <- candidate_values > best[current_indices]
      if (any(improves)) {
        selected_indices <- current_indices[improves]
        best[selected_indices] <- candidate_values[improves]
        take_item[i, selected_indices] <- TRUE
      }
    }
  }

  selected <- logical(n)
  remaining_capacity <- capacity
  if (n > 0L) {
    for (i in rev(seq_len(n))) {
      if (take_item[i, remaining_capacity + 1L]) {
        selected[[i]] <- TRUE
        remaining_capacity <- remaining_capacity - weights[[i]]
      }
    }
  }

  list(value = best[capacity + 1L], elements = which(selected))
}

#' Solve the 0/1 knapsack problem with a greedy approximation
#'
#' Sorts feasible items by value-to-weight ratio, takes the feasible prefix,
#' and returns the better of that prefix and the best feasible single item.
#' This standard 0/1 knapsack heuristic is O(n log n) and returns at least
#' half of the optimal value.
#'
#' @inheritParams check_input
#' @return A list with `value`, the total value selected, and `elements`, the
#'   selected row indices. If no item fits, the value is zero and `elements`
#'   is empty.
#' @examples
#' x <- data.frame(w = c(10, 20, 30), v = c(60, 100, 120))
#' greedy_knapsack(x, W = 50)
#' @export
greedy_knapsack <- function(x, W) {
  check_input(x, W)
  feasible <- which(x$w <= W)
  if (length(feasible) == 0L) {
    return(list(value = 0, elements = integer(0)))
  }

  ordered <- feasible[order(-x$v[feasible] / x$w[feasible], feasible)]
  cumulative_weight <- cumsum(x$w[ordered])
  prefix_length <- sum(cumulative_weight <= W)
  prefix <- if (prefix_length > 0L) ordered[seq_len(prefix_length)] else integer(0)
  best_single <- feasible[[which.max(x$v[feasible])]]

  if (x$v[best_single] > sum(x$v[prefix])) {
    elements <- best_single
  } else {
    elements <- prefix
  }

  list(value = sum(x$v[elements]), elements = elements)
}
