# knapsack

<!-- badges: start -->
[![R-CMD-check](https://github.com/Tvths/knapsack/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Tvths/knapsack/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`knapsack` contains three algorithms for the 0/1 knapsack problem. Each item
has a value (`v`) and a weight (`w`), and the goal is to choose items whose
total weight does not exceed the capacity `W`.

## Install

Install the development version from GitHub with `pak`:

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

Each function returns a list with `value` and `elements`. `value` is the
selected total value rounded to the nearest integer; `elements` contains the
selected row numbers from the input data frame.

## Algorithms and limits

- `brute_force_knapsack()` checks subsets and returns an optimum. Its running
  time grows exponentially with the number of items, so use it for small
  inputs. Set `parallel = TRUE` to split the search across workers; for small
  inputs, parallel startup can take longer than the search.
- `knapsack_dynamic()` solves the problem exactly when item weights and `W`
  are positive integers. Its running time is O(nW), where `n` is the number of
  items.
- `greedy_knapsack()` sorts items by value-to-weight ratio and takes the
  consecutive prefix that fits. This is a heuristic. The current
  implementation does not compare that prefix with the best feasible
  single item, so it does not provide a formal 1/2-optimum guarantee; it can
  also return no items when the first item in the sorted order is too heavy.

All solvers expect a data frame with positive numeric columns named `v` and
`w`, and a positive capacity `W`. Dynamic programming additionally requires
integer weights and an integer capacity. The current input checks do not cover
every malformed or non-finite input, so pass finite, valid data.

## Tests, checks, and report

From the package project directory, run:

```r
devtools::test()
devtools::check()
```

The vignette contains reproducible timings for the lab's required input sizes,
a comparison of the dynamic-programming implementation with a nested-loop
reference, a profiling example, and a benchmark of the parallel brute-force
extension:

```r
browseVignettes("knapsack")
```

## Authors

| Name | LiU-ID | GitHub | Email |
|---|---|---|---|
| Zhengyu Wang | `zhewa470` | [wwwzyccc777](https://github.com/wwwzyccc777) | zhewa470@student.liu.se |
| Viet Tien Trinh | `vietr933` | [Tvths](https://github.com/Tvths) | vietr933@student.liu.se |
