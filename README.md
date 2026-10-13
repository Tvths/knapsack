# knapsack

<!-- badges: start -->
[![R-CMD-check](https://github.com/Tvths/knapsack/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Tvths/knapsack/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`knapsack` solves the 0/1 knapsack problem. Given item values `v`, weights `w`,
and a capacity `W`, the package provides an exact brute-force solver, an exact
dynamic-programming solver for integer weights, and a fast greedy approximation.

## Install

```r
install.packages("pak")
pak::pak("Tvths/knapsack")
```

## Example

```r
library(knapsack)

items <- data.frame(
  w = c(10, 20, 30),
  v = c(60, 100, 120)
)

brute_force_knapsack(items, W = 50)
knapsack_dynamic(items, W = 50)
greedy_knapsack(items, W = 50)
```

Each solver returns a list with `value`, the total value selected, and
`elements`, the selected row numbers from `items`. The brute-force and dynamic
solvers return an optimum. The greedy solver is an O(n log n) approximation
that returns at least half of the optimum. Dynamic programming requires integer
weights and an integer capacity. Brute force is exponential and supports at
most 30 items; use it for small examples or correctness comparisons. Set
`parallel = TRUE` in `brute_force_knapsack()` to distribute its search across
the detected CPU cores. For small inputs, parallel startup can take longer
than the search itself.

Inputs must be data frames with exactly two finite, positive numeric columns
named `v` and `w`. Values in `w` are item weights and values in `v` are item
values. `W` must be a finite, positive number.

## Tests and package check

From the package project directory, run:

```r
devtools::test()
devtools::check()
```

The lab report vignette includes demonstrations, complexity discussion,
benchmarks for the required input sizes, and the profiling/optimization notes:

```r
browseVignettes("knapsack")
```

## Authors

| Name | LiU-ID | GitHub | Email |
|---|---|---|---|
| Zhengyu Wang | `zhewa470` | [wwwzyccc777](https://github.com/wwwzyccc777) | zhewa470@student.liu.se |
| Viet Tien Trinh | `vietr933` | [Tvths](https://github.com/Tvths) | vietr933@student.liu.se |
