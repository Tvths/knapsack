suppressWarnings(RNGversion(min(as.character(getRversion()), "3.5.3")))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <- data.frame(
  w = sample(1:4000, size = n, replace = TRUE),
  v = runif(n = n, 0, 10000)
)

test_that("Correct object is returned", {
  expect_silent(dk <- knapsack_dynamic(x = knapsack_objects[1:8, ], W = 3500))
  expect_named(dk, c("value", "elements"))
})

test_that("functions rejects errounous input.", {
  expect_error(knapsack_dynamic("hej", 3500))
  expect_error(knapsack_dynamic(x = knapsack_objects[1:8, ], W = -3500))
})

test_that("Function return correct results.", {
  dk <- knapsack_dynamic(x = knapsack_objects[1:8, ], W = 3500)
  expect_equal(round(dk$value), 16770)
  expect_true(all(round(dk$elements) %in% c(5, 8)))

  dk <- knapsack_dynamic(x = knapsack_objects[1:12, ], W = 3500)
  expect_equal(round(dk$value), 16770)
  expect_true(all(round(dk$elements) %in% c(5, 8)))

  dk <- knapsack_dynamic(x = knapsack_objects[1:8, ], W = 2000)
  expect_equal(round(dk$value), 15428)
  expect_true(all(round(dk$elements) %in% c(3, 8)))

  dk <- knapsack_dynamic(x = knapsack_objects[1:12, ], W = 2000)
  expect_equal(round(dk$value), 15428)
  expect_true(all(round(dk$elements) %in% c(3, 8)))
})
