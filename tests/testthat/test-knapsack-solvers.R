test_that("the solvers return the expected structure and known optimum", {
  items <- data.frame(w = c(10, 20, 30), v = c(60, 100, 120))

  brute <- brute_force_knapsack(items, W = 50)
  dynamic <- knapsack_dynamic(items, W = 50)
  greedy <- greedy_knapsack(items, W = 50)

  expect_named(brute, c("value", "elements"))
  expect_named(dynamic, c("value", "elements"))
  expect_named(greedy, c("value", "elements"))
  expect_equal(brute$value, 220)
  expect_equal(dynamic$value, 220)
  expect_setequal(brute$elements, c(2L, 3L))
  expect_setequal(dynamic$elements, c(2L, 3L))
  expect_lte(sum(items$w[greedy$elements]), 50)
  expect_gte(greedy$value, 0.5 * dynamic$value)
})

test_that("course fixtures still produce their documented results", {
  bf <- brute_force_knapsack(knapsack_objects[1:8, ], W = 3500)
  expect_equal(round(bf$value), 16770)
  expect_setequal(bf$elements, c(5L, 8L))

  bf_small_capacity <- brute_force_knapsack(knapsack_objects[1:8, ], W = 2000)
  expect_equal(round(bf_small_capacity$value), 15428)
  expect_setequal(bf_small_capacity$elements, c(3L, 8L))

  bf_12 <- brute_force_knapsack(knapsack_objects[1:12, ], W = 3500)
  expect_equal(round(bf_12$value), 16770)
  expect_setequal(bf_12$elements, c(5L, 8L))

  bf_12_small_capacity <- brute_force_knapsack(knapsack_objects[1:12, ], W = 2000)
  expect_equal(round(bf_12_small_capacity$value), 15428)
  expect_setequal(bf_12_small_capacity$elements, c(3L, 8L))

  dynamic <- knapsack_dynamic(knapsack_objects[1:12, ], W = 3500)
  expect_equal(round(dynamic$value), 16770)
  expect_setequal(dynamic$elements, c(5L, 8L))

  dynamic_small_capacity <- knapsack_dynamic(knapsack_objects[1:8, ], W = 2000)
  expect_equal(round(dynamic_small_capacity$value), 15428)
  expect_setequal(dynamic_small_capacity$elements, c(3L, 8L))

  dynamic_12_small_capacity <- knapsack_dynamic(knapsack_objects[1:12, ], W = 2000)
  expect_equal(round(dynamic_12_small_capacity$value), 15428)
  expect_setequal(dynamic_12_small_capacity$elements, c(3L, 8L))

  greedy_800 <- greedy_knapsack(knapsack_objects[1:800, ], W = 3500)
  expect_equal(round(greedy_800$value), 192647)
  expect_lte(sum(knapsack_objects$w[greedy_800$elements]), 3500)

  greedy_1200 <- greedy_knapsack(knapsack_objects[1:1200, ], W = 2000)
  expect_equal(round(greedy_1200$value), 212337)
  expect_lte(sum(knapsack_objects$w[greedy_1200$elements]), 2000)
})

test_that("dynamic programming agrees with brute force on small inputs", {
  set.seed(121)
  for (iteration in seq_len(10)) {
    items <- data.frame(
      w = sample(1:25, size = 10, replace = TRUE),
      v = runif(10, min = 0.1, max = 100)
    )
    capacity <- sample(10:60, size = 1)
    expect_equal(
      knapsack_dynamic(items, W = capacity),
      brute_force_knapsack(items, W = capacity)
    )
  }
})

test_that("the greedy result is feasible and at least half optimal", {
  set.seed(514)
  for (iteration in seq_len(20)) {
    items <- data.frame(
      w = sample(1:40, size = 30, replace = TRUE),
      v = runif(30, min = 0.1, max = 100)
    )
    capacity <- sample(20:150, size = 1)
    greedy <- greedy_knapsack(items, W = capacity)
    optimum <- knapsack_dynamic(items, W = capacity)

    expect_lte(sum(items$w[greedy$elements]), capacity)
    expect_gte(greedy$value + 1e-10, 0.5 * optimum$value)
  }
})

test_that("parallel brute force returns the sequential result", {
  items <- knapsack_objects[1:12, ]
  expect_equal(
    brute_force_knapsack(items, W = 2000, parallel = TRUE),
    brute_force_knapsack(items, W = 2000)
  )
})

test_that("the solvers handle empty and infeasible inputs", {
  empty <- data.frame(v = numeric(0), w = numeric(0))
  expect_equal(brute_force_knapsack(empty, W = 10),
               list(value = 0, elements = integer(0)))
  expect_equal(knapsack_dynamic(empty, W = 10),
               list(value = 0, elements = integer(0)))
  expect_equal(greedy_knapsack(empty, W = 10),
               list(value = 0, elements = integer(0)))

  too_heavy <- data.frame(v = c(10, 20), w = c(11, 12))
  expect_equal(brute_force_knapsack(too_heavy, W = 10)$value, 0)
  expect_equal(knapsack_dynamic(too_heavy, W = 10)$value, 0)
  expect_equal(greedy_knapsack(too_heavy, W = 10),
               list(value = 0, elements = integer(0)))

  beyond_integer_range <- data.frame(
    v = 10,
    w = as.numeric(.Machine$integer.max) + 100
  )
  expect_equal(knapsack_dynamic(beyond_integer_range, W = 10)$value, 0)
})

test_that("fractional values are returned without rounding", {
  items <- data.frame(w = c(1, 2), v = c(1.25, 3.375))
  expect_equal(brute_force_knapsack(items, W = 2)$value, 3.375)
  expect_equal(knapsack_dynamic(items, W = 2)$value, 3.375)
  expect_equal(greedy_knapsack(items, W = 2)$value, 3.375)
})

test_that("greedy compares its density prefix with the best single item", {
  items <- data.frame(w = c(9, 10), v = c(9, 10))
  result <- greedy_knapsack(items, W = 10)
  expect_equal(result$value, 10)
  expect_equal(result$elements, 2L)
})

test_that("invalid inputs are rejected with useful errors", {
  expect_error(brute_force_knapsack("items", 10), "data frame")
  expect_error(greedy_knapsack(data.frame(w = 1:2, value = 2:3), 3),
               "exactly the columns")
  expect_error(knapsack_dynamic(data.frame(w = c(1, NA), v = 1:2), 3),
               "finite positive")
  expect_error(brute_force_knapsack(data.frame(w = 1, v = Inf), 3),
               "finite positive")
  expect_error(greedy_knapsack(data.frame(w = 1, v = 1 + 1i), 3),
               "numeric")
  expect_error(greedy_knapsack(data.frame(w = 0, v = 1), 3),
               "finite positive")
  expect_error(brute_force_knapsack(data.frame(w = 1, v = 2), NA_real_),
               "finite positive number")
  expect_error(brute_force_knapsack(data.frame(w = 1, v = 2), 3 + 1i),
               "finite positive number")
  expect_error(brute_force_knapsack(data.frame(w = 1, v = 2), 3,
                                    parallel = NA),
               "parallel")
  expect_error(knapsack_dynamic(data.frame(w = 1.5, v = 2), 3),
               "Weights.*must be integers")
  expect_error(knapsack_dynamic(data.frame(w = 1, v = 2), 3.5),
               "W.*must be an integer")
  expect_error(brute_force_knapsack(data.frame(w = rep(1, 31), v = rep(1, 31)),
                                    W = 31),
               "at most 30 items")
})
