
# rfinance <img src="man/figures/logo.png" align="right" width="120" />

<!-- badges: start -->
<!-- badges: end -->

The goal of rfinance is to provide a user-friendly suit of tools to perform financial analysis, using R.

## Installation

You can install the released version of rfinance from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("rfinance")
```

## Examples

### Historic Prices

Let's say that you want to download all historic prices from a given company. `get_historic_prices()` will let you do just that and get it stored in a `data.frame`. You just need to specify the **symbol**. In the case of this example, *Microsoft*'s symbol is `MSFT`.

``` r
library(rfinance)

df <- get_historic_prices(symbol = 'MSFT')
```

### Financial Statements

In most financial analysis, the financial statements released by firms are arguably the more important variables, and the harder to get.

`rfinance` provies a set of functions to get the different financial statements of any public company.

```r

firm <- 'MSFT'

cash_flow     <- get_cash_flow_statement(symbol = firm)
balance_sheet <- get_balance_sheet_statement(symbol = firm)
income        <- get_income_statement(symbol = firm)

```

### Symbols List

At this point it'd be natural to ask yourself what companies are available for this. `rfinance` includes a function to check just that. `get_symbols_list()` will give you an array of all the symbols available in the API.

```r
symbols_list <- get_symbols_list()
```

