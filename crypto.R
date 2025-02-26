# Load necessary libraries
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

# Define constants
ratios <- c("BTCUSDT", "LTCUSDT", "ETHUSDT", "BNBUSDT", 
            "ADAUSDT", "BTTUSDT", "DASHUSDT", "XMRUSDT", 
            "NANOUSDT", "DOGEUSDT", "XLMUSDT", "BCHUSDT")
SEQ_LEN <- 48
FUTURE_PERIOD_PREDICT <- 1

# Function to calculate momentum (percentage change over a period)
calculate_momentum <- function(df, period = 30) {
  if (nrow(df) < period) return(NA)  # Ensure we have enough data
  momentum <- (tail(df$close, n = 1) - head(df$close, n = period)) / head(df$close, n = period) * 100
  return(momentum)
}

# Function to apply the momentum strategy
apply_momentum_strategy <- function(ratios) {
  momentum_scores <- data.frame(Symbol = character(), Momentum = numeric(), stringsAsFactors = FALSE)
  market_performance <- data.frame(date = as.POSIXct(character()), cumulative_return_market = numeric(), stringsAsFactors = FALSE)
  
  # Loop through each cryptocurrency to calculate momentum
  for (ratio in ratios) {
    # Construct correct file path
    dataset <- normalizePath(paste0("C:/Users/xodbs/OneDrive/바탕 화면/Crypto/crypto_dataset_&_model/data/Binance_", ratio, "_1h.csv"), winslash = "/")
    
    # Check if file exists before reading
    if (!file.exists(dataset)) {
      warning(paste("File not found:", dataset))
      next
    }
    
    # Read the CSV file safely
    df <- tryCatch({
      read_csv(dataset, col_types = cols()) %>%
        select(date, close, volume) %>%
        mutate(date = as.POSIXct(date))
    }, error = function(e) {
      warning(paste("Error reading file:", dataset))
      return(NULL)
    })
    
    # Skip if data is missing
    if (is.null(df) || nrow(df) < SEQ_LEN) next  
    
    # Calculate momentum
    momentum <- calculate_momentum(df, period = SEQ_LEN)
    if (!is.na(momentum)) {
      momentum_scores <- rbind(momentum_scores, data.frame(Symbol = ratio, Momentum = momentum))
    }
    
    # Calculate buy-and-hold strategy cumulative return
    df$market_return <- (df$close / first(df$close)) - 1
    df$cumulative_return_market <- cumprod(1 + df$market_return)
    
    # Ensure 'date' exists before merging
    if (!"date" %in% colnames(df)) {
      warning(paste("Missing 'date' column in dataset:", ratio))
      next
    }
    
    # Initialize or merge market performance
    if (nrow(market_performance) == 0) {
      market_performance <- df %>% select(date, cumulative_return_market)
    } else {
      market_performance <- left_join(market_performance, df %>% select(date, cumulative_return_market), by = "date")
    }
  }
  
  # Sort momentum scores and select top-performing cryptos
  momentum_scores <- momentum_scores %>% arrange(desc(Momentum))
  top_cryptos <- head(momentum_scores$Symbol, 5)
  
  # Initialize portfolio performance
  portfolio_performance <- data.frame(date = as.POSIXct(character()), cumulative_return_momentum = numeric(), stringsAsFactors = FALSE)
  
  for (ratio in top_cryptos) {
    dataset <- normalizePath(paste0("C:/Users/xodbs/OneDrive/바탕 화면/Crypto/crypto_dataset_&_model/data/Binance_", ratio, "_1h.csv"), winslash = "/")
    
    if (!file.exists(dataset)) {
      warning(paste("File not found:", dataset))
      next
    }
    
    df <- tryCatch({
      read_csv(dataset, col_types = cols()) %>%
        select(date, close) %>%
        mutate(date = as.POSIXct(date))
    }, error = function(e) {
      warning(paste("Error reading file:", dataset))
      return(NULL)
    })
    
    if (is.null(df) || nrow(df) < SEQ_LEN) next  
    
    df$momentum_return <- (df$close / first(df$close)) - 1
    df$cumulative_return_momentum <- cumprod(1 + df$momentum_return)
    
    # Ensure 'date' exists before merging
    if (!"date" %in% colnames(df)) {
      warning(paste("Missing 'date' column in dataset:", ratio))
      next
    }
    
    # Initialize or merge portfolio performance
    if (nrow(portfolio_performance) == 0) {
      portfolio_performance <- df %>% select(date, cumulative_return_momentum)
    } else {
      portfolio_performance <- left_join(portfolio_performance, df %>% select(date, cumulative_return_momentum), by = "date")
    }
  }
  
  # Merge the two performance dataframes (market and momentum strategy)
  combined_performance <- left_join(market_performance, portfolio_performance, by = "date")
  
  # Debugging: Check if date exists before plotting
  print("Market Performance Structure:")
  print(str(market_performance))
  
  print("Portfolio Performance Structure:")
  print(str(portfolio_performance))
  
  print("Combined Performance Structure:")
  print(str(combined_performance))
  
  # Plot the comparison
  ggplot(combined_performance, aes(x = date)) +
    geom_line(aes(y = cumulative_return_market, color = "Market (Buy & Hold)")) +
    geom_line(aes(y = cumulative_return_momentum, color = "Momentum Strategy")) +
    labs(title = "Cumulative Return Comparison: Market vs Momentum Strategy",
         x = "Date", y = "Cumulative Return",
         color = "Strategy") +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# Apply the momentum strategy and plot the results
apply_momentum_strategy(ratios)

