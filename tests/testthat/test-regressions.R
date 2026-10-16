test_that("greedy handles heavy items and compares with the best single item", {
  heavy <- data.frame(w = c(11, 10), v = c(110, 80))
  expect_equal(greedy_knapsack(heavy, 10), list(value = 80, elements = 2L))
  single <- data.frame(w = c(6, 10), v = c(7, 10))
  expect_equal(greedy_knapsack(single, 10), list(value = 10, elements = 2L))
})

test_that("dynamic matches brute force and greedy reaches half of the optimum", {
  set.seed(615)
  for (iteration in seq_len(20)) {
    items <- data.frame(w = sample(1:25, 10, replace = TRUE), v = runif(10, 0.1, 100))
    capacity <- sample(5:50, 1)
    exact <- brute_force_knapsack(items, capacity)
    dynamic <- knapsack_dynamic(items, capacity)
    greedy <- greedy_knapsack(items, capacity)
    expect_equal(dynamic$value, exact$value)
    expect_equal(sum(items$v[dynamic$elements]), dynamic$value)
    expect_lte(sum(items$w[dynamic$elements]), capacity)
    expect_lte(sum(items$w[greedy$elements]), capacity)
    expect_gte(greedy$value + 1e-10, exact$value / 2)
  }
})

test_that("empty, infeasible, and fractional-value inputs have consistent results", {
  solvers <- list(brute_force_knapsack, knapsack_dynamic, greedy_knapsack)
  for (solver in solvers) {
    expect_equal(solver(data.frame(w = numeric(), v = numeric()), 10),
                 list(value = 0, elements = integer(0)))
    expect_equal(solver(data.frame(w = 11, v = 20), 10),
                 list(value = 0, elements = integer(0)))
    expect_equal(solver(data.frame(w = c(1, 2), v = c(1.25, 3.375)), 2)$value, 3.375)
  }
})

test_that("invalid inputs are rejected before running a solver", {
  for (solver in list(brute_force_knapsack, knapsack_dynamic, greedy_knapsack)) {
    for (bad in list(NA_real_, NaN, Inf, -Inf, 0, -1)) {
      expect_error(solver(data.frame(w = 1, v = bad), 10), "finite and positive")
      expect_error(solver(data.frame(w = bad, v = 1), 10), "finite and positive")
      expect_error(solver(data.frame(w = 1, v = 1), bad), "finite positive")
    }
    expect_error(solver(data.frame(w = "one", v = 1), 10), "numeric vectors")
    expect_error(solver(data.frame(w = 1, v = 1 + 1i), 10), "numeric vectors")
    expect_error(solver(data.frame(w = 1, value = 1), 10), "columns v and w")
    expect_error(solver(data.frame(w = 1, v = 1), numeric()), "single finite positive")
  }
  expect_error(knapsack_dynamic(data.frame(w = 1.5, v = 2), 10), "integer")
  expect_error(knapsack_dynamic(data.frame(w = 1, v = 2), 10.5), "integer")
  expect_error(brute_force_knapsack(data.frame(w = rep(1, 31), v = rep(1, 31)), 10),
               "at most 30")
  expect_error(brute_force_knapsack(data.frame(w = 1, v = 1), 10, parallel = NA),
               "parallel")
})

test_that("parallel search handles a single subset", {
  items <- data.frame(w = 1, v = 2.5)
  expect_equal(brute_force_knapsack(items, 1, parallel = TRUE),
               brute_force_knapsack(items, 1))
})

test_that("the second large greedy example from the lab is reproduced", {
  suppressWarnings(RNGversion(min(as.character(getRversion()), "3.5.3")))
  set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
  items <- data.frame(w = sample(1:4000, 2000, replace = TRUE), v = runif(2000, 0, 10000))
  result <- greedy_knapsack(items[1:1200, ], 2000)
  expect_equal(round(result$value), 212337)
  expect_lte(sum(items$w[result$elements]), 2000)
})
