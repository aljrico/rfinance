#' This is a Portfolio class
#' @title portfolio class
#' @docType class
#' @description portfolio class description
#'
portfolio <-
  R6::R6Class(
    public = list(
      cash = 0,
      orders = data.frame(asset = character(0), date = character(0), type = character(0)),
      
      #' @description Spend some cash to buy an asset.
      #' @param asset Symbol specifying the asset to be bought.
      #' @param date Date or date-like string that tells when should this asset be purchased.
      #' @param number_assets How many shares of the asset should be exchanged. Defaults to NULL.
      #' @param money How much money 
      #' 
      buy = function(asset, date, number_assets = NULL, money = NULL) {
        if (!(asset %in% self$assets_info)) self$download_data(asset)

        # Get asset price history
        price <- private$check_price(asset = asset, operation_date = date)
        
        if(!is.null(money)){
          number_assets <- money / price
        }else if(is.null(number_assets)){
          stop('You need to specify either the number of assets or the money to operate with.')
        }
        
        cash_paid <- price * number_assets
        self$cash_out(cash_paid, date = date, report = FALSE)

        # Register Action
        private$register_order(asset, date, type = "purchase", amount = number_assets, price = cash_paid)
      },
      
      #' @description Sells a determined asset and gets cash back.
      #' @param asset Symbol specifying the asset to be sold
      #' @param date Date or date-like string that tells when should this asset be sold.
      #' @param number_assets How many shares of the asset should be exchanged.
      #' 
      sell = function(asset, date, number_assets = NULL, money = NULL) {

        # Get asset price history
        price <- private$check_price(asset = asset, operation_date = date)
        
        if(!is.null(money)){
          number_assets <- money / price
        }else if(is.null(number_assets)){
          stop('You need to specify either the number of assets or the money to operate with.')
        }
        
        cash_received <- price * number_assets
        self$cash_in(cash_received, date = date, report = FALSE)

        # Register Action
        private$register_order(asset, date, type = "sell", amount = number_assets, price = cash_received)
      },
      cash_in = function(amount, date, report = TRUE) {
        if (amount >= 0) self$cash <- self$cash + amount
        if (amount < 0) stop("You can't put negative cash.")

        # Register Operation
        if (report) private$register_order(asset = "cash", date = date, type = "cash_in", amount = amount, price = 1)
      },
      cash_out = function(amount, date, report = TRUE) {
        remaining_cash <- self$cash - amount
        if (remaining_cash < 0) stop("You don't have enough cash to do that.")
        if (amount >= 0) self$cash <- remaining_cash

        # Register Operation
        if (report) private$register_order(asset = "cash", date = date, type = "cash_out", amount = amount, price = 1)
      },
      download_data = function(asset) {
        clean_prices <- function(prices) {
          prices %>%
            rename(date = Date, price = `Adj Close`) %>%
            select(date, price, name) %>%
            mutate(date = as.Date(date)) %>%
            mutate(price = as.numeric(price)) %>%
            arrange(date)
        }
        
        fill_all_days <- function(df) {
          df %>%
            tidyr::complete(date = seq.Date(min(date), max(date), by = "day")) %>%
            arrange(date) %>%
            zoo::na.locf()
        }
        
        private$assets_info[[asset]]$company_profile <- rfinance::get_company_profile(asset)
        private$assets_info[[asset]]$historic_prices <- rfinance::get_prices(asset) %>%
          clean_prices() %>%
          fill_all_days()
      },
      portfolio_evolution = function(d_max) {
        orders <- self$orders
        
        d_max <- lubridate::ymd(as.Date(d_max))
        d_min <- lubridate::ymd(as.Date(min(orders$date)))
        all_dates <- seq.Date(d_min, d_max, by = "day")

        extract_prices <- function(x) {
          x[["historic_prices"]]
        }


        orders_list <- list()

        for (i in seq_along(all_dates)) {
          this_date <- all_dates[[i]]
          this_orders <- orders %>%
            filter(date <= this_date) %>%
            filter(asset != "cash")

          if (nrow(this_orders) < 1) next

          orders_list[[i]] <-
            this_orders %>%
            mutate(amount = ifelse(type == "sell", -amount, amount)) %>%
            group_by(asset) %>%
            summarise(amount = sum(amount, na.rm = TRUE)) %>%
            mutate(date = this_date)
        }

        orders_compact <- orders_list %>% bind_rows()
        existing_assets <- orders_compact$asset %>%
          unique() %>%
          as.character()

        prices <-
          private$assets_info[existing_assets] %>%
          lapply(., extract_prices) %>%
          data.table::rbindlist() %>%
          tibble::as_tibble()

        orders_compact %>%
          arrange(date) %>%
          rename(name = asset) %>%
          mutate(name = as.character(name)) %>%
          left_join(prices) %>%
          zoo::na.locf() %>%
          mutate(asset_value = amount * price) %>%
          select(date, name, asset_value) %>%
          tidyr::pivot_wider(names_from = name, values_from = asset_value) %>%
          replace(., is.na(.), 0)
      }
    ),
    private = list(
      assets_info = list(),
      register_order = function(asset, date, type, amount, price) {
        new_order <- data.frame(asset = asset, date = date, type = type, amount = amount, price = price)
        self$orders <- rbind(self$orders, new_order)
        self$orders$date <- lubridate::ymd(self$orders$date)
      },
      check_price = function(asset, operation_date) {
        private %>%
          .$assets_info %>%
          .[[asset]] %>%
          .[["historic_prices"]] %>%
          filter(date == operation_date) %>%
          .$price
      }
    )
  )
