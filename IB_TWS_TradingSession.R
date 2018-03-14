#####################################################################################################################
################################################# A S3 Class used to create trading tool ############################
#####################################################################################################################
################################################# S3 Class reference: ###############################################
################################################# http://www.cyclismo.org/tutorial/R/s3Classes.html #################
#####################################################################################################################

#
# Load libraries
#
libraries <- c("IBrokers","quantmod")
lapply(libraries, function(lib){ library(lib, character.only=TRUE) })

#
# Define default values
#
def_port_holdings_colnames <- c("Date","LocalTicker","SecurityType","Exchange","Currency","Position",
                                "MktPrc","MktVal","AvgCost","UnrealizedPNL","RealizedPNL","TimeStamp")
def_port_info_colnames <- c("Date", "Value", "Currency", "TimeStamp")
def_watchlist_colnames <- c("LocalTicker","Currency","SecurityType","Comments")
def_prelimtradelist_colnames <- c("LocalTicker","Action","Quantity","OrderType",
                                  "LimitPrice","SecurityType","Currency","TradeSwitch")
def_fnltradelist_colnames <- c("LocalTicker","Action","Quantity","OrderType",
                                "AdjustedLimitPrice","LastTradePrice","SecurityType",
                                "Currency","Commission","ValidTicker")

#
# Defind class - TradingSession
#
TradingSession <- function(c_id, 
                           c_type = c("TWS", "IBG"),
                           acct_type = c("Live", "Paper"),
                           l_host = "localhost"){
  
  ct <- match.arg(c_type)
  at <- match.arg(acct_type)
  
  if(ct == "TWS"){
    if(at == "Live"){
      my_conn <- twsConnect(clientId = c_id, host = l_host, port = 7496)
    } else {
      my_conn <- twsConnect(clientId = c_id, host = l_host, port = 7497)
    }
  } else if (ct == "IBG"){
    if(at == "Live"){
      my_conn <- ibgConnect(clientId = c_id, host = l_host, port = 7496)
    } else {
      my_conn <- ibgConnect(clientId = c_id, host = l_host, port = 7497)
    }
  } else {
    res <- "Error connection type!"
  }
  
  InitWatchlist <- function(){
    return(data.frame(LocalTicker = character(0),
                      Currency = character(0),
                      SecurityType = character(0),
                      Comments = character(0),
                      stringsAsFactors = FALSE))
  }
  
  InitPrelimTradelist <- function(){
    return(data.frame(LocalTicker = character(0),
                      Action = character(0),
                      Quantity = numeric(0),
                      OrderType = character(0),
                      LimitPrice = numeric(0),
                      SecurityType = character(0),
                      Currency = character(0),
                      stringsAsFactors = FALSE))
  }
  
  InitFnlTradelist <- function(){
    return(data.frame(LocalTicker = character(0),
                      Action = character(0),
                      Quantity = numeric(0),
                      OrderType = character(0),
                      AdjustedLimitPrice = numeric(0),
                      Commission = character(0),
                      Currency = character(0),
                      
                      stringsAsFactors = FALSE))
  }
  if(isConnected(my_conn)){
    res <- list(ts_client_id = c_id,
                ts_conn = my_conn,
                ts_session_start_time = reqCurrentTime(my_conn),
                ts_server_version = serverVersion(my_conn),
                ts_us_equity_comm = 0.005,                   # US equity trade commission per share
                ts_ca_equity_comm = 0.01,                    # CA equity trade commission per share
                ts_port_holdings = NULL,
                ts_port_info = NULL,
                ts_ca_net_liquidation = 0,
                ts_ca_gross_position_value = 0,
                ts_ca_equity_with_loan_value = 0,
                ts_ca_total_cash_value = 0,
                ts_us_cash_balance = 0,
                ts_ca_cash_balance = 0,
                ts_ca_accrued_divdend = 0,
                ts_exchange_rate = 0,
                ts_watchlist = InitWatchlist(),
                ts_prelimTradelist = InitPrelimTradelist(),
                ts_fnlTradelist = InitFnlTradelist(),
                ts_trade_ids = NULL,
                ts_trade_results = NULL,
                ts_trade_summary = NULL,
                ts_trade_transmit_switch = FALSE,
				        ts_last_trade_message = NULL,
                ts_trade_gooduntildate = as.Date(reqCurrentTime(my_conn)))
    class(res) <- append(class(res), "TradingSession")
    res <- TSRetrievePortHoldings(res)
    res <- TSRetrievePortInfo(res)
    
    #
    # Update portfolio snapshot data
    #
    LocalSaveData(res)
  } else {
    res <- "Error connection to IB TWS!"
  }
  return(res)
}

#
# Retrieve portfolio holdings
#
TSRetrievePortHoldings <- function(ts) UseMethod("TSRetrievePortHoldings")
TSRetrievePortHoldings.default <- function(ts){ return(ts) }
TSRetrievePortHoldings.TradingSession <- function(ts){
  if(isConnected(ts$ts_conn)){
    km.port.prelim <- reqAccountUpdates(ts$ts_conn)
    
    #
    # Retrieve portfolio holdings data
    #
    km.port.holdings.prelim <- km.port.prelim[[2]]
    
    if(length(km.port.holdings.prelim) == 0){
      km.port.holdings <- data.frame(as.Date(integer(0)), character(0), character(0), character(0), character(0), 
                                     numeric(0), numeric(0), numeric(0), numeric(0), numeric(0), numeric(0), as.Date(integer(0)))
    } else {
      km.port.holdings <- NULL
      for(i in 1:length(km.port.holdings.prelim)){
        local <- km.port.holdings.prelim[[i]]$contract$local
        sectype <- km.port.holdings.prelim[[i]]$contract$sectype
        exc <- km.port.holdings.prelim[[i]]$contract$exch
        curr <- km.port.holdings.prelim[[i]]$contract$currency
        pos <- km.port.holdings.prelim[[i]]$portfolioValue$position
        prc <- km.port.holdings.prelim[[i]]$portfolioValue$marketPrice
        val <- km.port.holdings.prelim[[i]]$portfolioValue$marketValue
        avgcost <- km.port.holdings.prelim[[i]]$portfolioValue$averageCost
        unrealizedPNL <- km.port.holdings.prelim[[i]]$portfolioValue$unrealizedPNL
        realizedPNL <- km.port.holdings.prelim[[i]]$portfolioValue$realizedPNL
        dt <- as.Date(ts$ts_session_start_time)
        
        holding <- data.frame(dt, local, sectype, exc, curr, pos, prc, val, avgcost, unrealizedPNL, realizedPNL, ts$ts_session_start_time)
        
        if(i == 1){
          km.port.holdings <- holding 
        } else {
          km.port.holdings <- rbind.data.frame(km.port.holdings, holding)
        }
      }
    }
    
    colnames(km.port.holdings) <- def_port_holdings_colnames
    ts$ts_port_holdings <- km.port.holdings
  } else {
    print("Error connection to IB TWS!")
  }
  
  return(ts)
}

#
# Retrieve portfolio info
#
TSRetrievePortInfo <- function(ts) UseMethod("TSRetrievePortInfo")
TSRetrievePortInfo.default <- function(ts){ return(ts) }
TSRetrievePortInfo.TradingSession <- function(ts){
  if(isConnected(ts$ts_conn)){
    km.port.prelim <- reqAccountUpdates(ts$ts_conn)
    #
    # Retrieve portfolio value overview data
    #
    
    #
    # EquityWithLoanValue (Settled Cash) = Cash recognized at the time of settlement
    #                                       - Purchases at the time of trade
    #                                       - Commissions - taxes - fees
    #
    # TotalCashValue = EquityWithLoanValue + Sales at the time of trade
    #
    km.port.info <- rbind.data.frame(km.port.prelim[[1]]$NetLiquidation,
                                         km.port.prelim[[1]]$GrossPositionValue,
                                         km.port.prelim[[1]]$EquityWithLoanValue,
                                         km.port.prelim[[1]]$TotalCashValue,
                                         km.port.prelim[[1]]$CashBalance,
                                         km.port.prelim[[1]]$ExchangeRate,
                                         km.port.prelim[[1]]$AccruedDividend)
    
    rownames(km.port.info) <- c("NetLiquidation","GrossPositionValue","EquityWithLoanValue","TotalCashValue",
                                    "CashBalance","ExchangeRate","AccruedDividend")
    colnames(km.port.info) <- c("Value","Currency")
    km.port.info$Value <- as.numeric(as.character(km.port.info$Value))
    km.port.info$Currency <- as.character(km.port.info$Currency)
    
    #
    # Check if currency is appropriate
    #
    if(km.port.info["NetLiquidation","Currency"] == "CAD" &
       km.port.info["GrossPositionValue","Currency"] == "CAD" &
       km.port.info["EquityWithLoanValue","Currency"] == "CAD" &
       km.port.info["TotalCashValue","Currency"] == "CAD"){
      #
      # Find available cash for trading
      #
      cash <- km.port.info["CashBalance",]
      cash.us <- cash[cash$Currency == "USD", "Value"]
      cash.ca <- cash[cash$Currency == "CAD", "Value"]
      
      if(length(cash.ca) == 0 & length(cash.us) != 0){
        cal.cash.ca <- km.port.info["TotalCashValue","Value"] - cash.us * km.port.info["ExchangeRate","Value"]
        cash.balance.rev <- data.frame(c(cash.us, cal.cash.ca),
                                       c("USD","CAD"))
      } else if (length(cash.ca) != 0 & length(cash.us) == 0){
        cal.cash.us <- (km.port.info["TotalCashValue","Value"] - cash.ca)/km.port.info["ExchangeRate","Value"]
        cash.balance.rev <- data.frame(c(cal.cash.us, cash.ca),
                                       c("USD","CAD"))
      } else {
        print("Warnings: Multiple currencies exist!")
      }
      rownames(cash.balance.rev) <- c("CashBalanceUS","CashBalanceCA")
      colnames(cash.balance.rev) <- c("Value","Currency")
      km.port.info <- rbind.data.frame(km.port.info, cash.balance.rev)
      
      km.port.info$Date <- rep(as.Date(ts$ts_session_start_time), nrow(km.port.info))
      km.port.info$TimeStamp <- rep(ts$ts_session_start_time, nrow(km.port.info))
      km.port.info <- km.port.info[,def_port_info_colnames]
      
      #
      # Assign individual metric
      #
      ts$ts_ca_net_liquidation <- km.port.info["NetLiquidation","Value"]
      ts$ts_ca_gross_position_value <- km.port.info["GrossPositionValue","Value"]
      ts$ts_ca_equity_with_loan_value <- km.port.info["EquityWithLoanValue","Value"]
      ts$ts_ca_total_cash_value <- km.port.info["TotalCashValue","Value"]
      ts$ts_us_cash_balance <- km.port.info["CashBalanceUS","Value"]
      ts$ts_ca_cash_balance <- km.port.info["CashBalanceCA","Value"]
      ts$ts_ca_accrued_divdend <- km.port.info["AccruedDividend","Value"]
      ts$ts_exchange_rate <- km.port.info["ExchangeRate","Value"]
      
      #
      # Assign port info data frame
      #
      ts$ts_port_info <- km.port.info
    } else {
      print("Warnings: Currency not as expected!")
    }
  } else {
    print("Error connection to IB TWS!")
  }
  
  return(ts)
}

#
# Local function: Save historical data
#
LocalSaveData <- function(ts){
  fn <- "Portfolio Snapshot.RData"
  if(file.exists(fn)){
    load(file = fn)
    curr_port_snapshot <- ts$ts_port_info
    hist_port_snapshot <- rbind.data.frame(hist_port_snapshot, curr_port_snapshot)
    save(hist_port_snapshot, file = fn)
  }
  
  fn1 <- "Portfolio Holdings.RData"
  if(file.exists(fn1)){
    load(file = fn1)
    curr_port_holdings <- ts$ts_port_holdings
    hist_port_holdings <- rbind.data.frame(hist_port_holdings, curr_port_holdings)
    save(hist_port_holdings, file = fn1)
  }
}

#
# Update watchlist
#
TSSetWatchlist <- function(ts, ws) UseMethod("TSSetWatchlist")
TSSetWatchlist.default <- function(ts, ws){ return(ts) }
TSSetWatchlist.TradingSession <- function(ts, ws){
  if(isConnected(ts$ts_conn)){
    if(identical(colnames(ws),def_watchlist_colnames)){
      ts$ts_watchlist <- ws
    } else {
      print("Error generating watchlist!")
    }
  } else {
    print("Error connection to IB TWS!")
  }
  return(ts)
}

#
# Set preliminary trade list
#
TSSetPrelimTradeList <- function(ts, man_trd_df) UseMethod("TSSetPrelimTradeList")
TSSetPrelimTradeList.default <- function(ts, man_trd_df){ return(ts) }
TSSetPrelimTradeList.TradingSession <- function(ts, man_trd_df){
  
  if(isConnected(ts$ts_conn)){
    if(identical(colnames(man_trd_df), def_prelimtradelist_colnames)){
      ts$ts_prelimTradelist <- man_trd_df
    } else{
      print("Error preliminary trade list fields are not valid.")
    }
  } else {
    print("Error connection to IB TWS!")
  }
  return(ts)
}

#
# Generate final trade list
#
TSGenFnlTradeList <- function(ts) UseMethod("TSGenFnlTradeList")
TSGenFnlTradeList.default <- function(ts){ return(ts) }
TSGenFnlTradeList.TradingSession <- function(ts){
  
  if(isConnected(ts$ts_conn)){
    # Field: "LocalTicker","Action","Quantity","OrderType","LimitPrice","SecurityType","Currency"
    man_trd_df <- ts$ts_prelimTradelist
    if(nrow(man_trd_df) != 0){
      man_trd_df$YahooTicker <- paste(man_trd_df$LocalTicker,
                                      ifelse(man_trd_df$Currency=="CAD",".TO",""),
                                      sep="")
      
      man_trd_df$LastTradePrice <- rep(0,nrow(man_trd_df))
      man_trd_df$AdjustedLimitPrice <- man_trd_df$LimitPrice
      man_trd_df$LastTimeStamp <- rep(Sys.Date(),nrow(man_trd_df))
      man_trd_df$Commission <- rep(0,nrow(man_trd_df))
      man_trd_df$ValidTicker <- rep(TRUE,nrow(man_trd_df))

      for(i in 1:nrow(man_trd_df)){
        #
        # Update commission
        #
        if(man_trd_df[i,"Currency"]== "USD"){
          man_trd_df[i,"Commission"] <- max(1, ts$ts_us_equity_comm*man_trd_df[i,"Quantity"], na.rm = TRUE)
        } else if(man_trd_df[i,"Currency"]== "CAD") {
          man_trd_df[i,"Commission"] <- max(1, ts$ts_ca_equity_comm*man_trd_df[i,"Quantity"], na.rm = TRUE)
        } else {
          print("Warnings: Non-north america trade exist!")
        }
        
        #
        # Update Adjusted limit price
        #
        # getQuote no longer works.
        #
        # x <- getQuote(man_trd_df[i,"YahooTicker"])
        # if(!is.na(x[,"Trade Time"])){
        #   man_trd_df[i,"LastTradePrice"] <- as.numeric(x[,"Last"])
        #   man_trd_df[i,"AdjustedLimitPrice"] <- max(as.numeric(x[,"Last"]),
        #                                             man_trd_df[i,"LimitPrice"],
        #                                             na.rm = TRUE)
        #   man_trd_df[i,"LastTimeStamp"] <- x[,"Trade Time"]
        # } else {
        #  man_trd_df[i,"ValidTicker"] <- FALSE
        #  print(paste0("$",man_trd_df[i,"YahooTicker"],
        #              ": Invalid ticker. No real time quote found."))
        # }
      }
      
      ts$ts_fnlTradelist <- man_trd_df[,def_fnltradelist_colnames]
    }
  } else {
    print("Error connection to IB TWS!")
  }
  return(ts)
}

#
# Place a trade 
#
TSExecuteTrade <- function(ts, single_trade) UseMethod("TSExecuteTrade")
TSExecuteTrade.default <- function(ts, single_trade){ return(ts) }
TSExecuteTrade.TradingSession <- function(ts, single_trade){
  if(isConnected(ts$ts_conn)){
    #
    # Request most up-to-date account info
    #
    new_ts <- TSRetrievePortHoldings(ts)
    curr_holdings <- new_ts$ts_port_holdings
    new_ts <- TSRetrievePortInfo(ts)
    us_cash_balance <- new_ts$ts_us_cash_balance
    ca_cash_balance <- new_ts$ts_ca_cash_balance
    
    if(identical(colnames(single_trade),def_fnltradelist_colnames)){
      #
      # Validate and execute a single trade
      #
      bad.sell.order <- 0
      bad.buy.order <- 0
      if(tolower(single_trade[,"SecurityType"]) == "stk"){
        
        #
        # Validate a trade
        #
        if(tolower(single_trade$Action) == "sell"){
          sell_trade <- merge.data.frame(single_trade, curr_holdings, by = c("LocalTicker", "Currency"))
          
          ValidateSellOrder <- function(x){
            if(x[1] > x[2]){
              return(FALSE)
            } else {
              return(TRUE)
            }
          }
          
          if(nrow(single_trade) == nrow(sell_trade)){
            sell_trade$Valid <- apply(sell_trade[,c("Quantity","Position")], 1, ValidateSellOrder)
            bad.sell.order <- sum(sell_trade$Valid == FALSE)
          } else{
            bad.sell.order <- 1
          }
          
        } else if (tolower(single_trade$Action) == "buy"){
          us.order <- single_trade[single_trade$Currency == "USD",c("Quantity","AdjustedLimitPrice","Commission")]
          if(nrow(us.order) != 0){
            us.order.value <- sum(us.order$Quantity*us.order$AdjustedLimitPrice) + sum(us.order$Commission)
          } else {
            us.order.value <- 0
          }
          
          ca.order <- single_trade[single_trade$Currency == "CAD",c("Quantity","AdjustedLimitPrice","Commission")]
          if(nrow(ca.order) != 0){
            ca.order.value <- sum(ca.order$Quantity*ca.order$AdjustedLimitPrice) + sum(ca.order$Commission)
          } else {
            ca.order.value <- 0
          }
          
          bad.buy.order <- 0
          if(us.order.value > us_cash_balance | ca.order.value > ca_cash_balance){
              bad.buy.order <- 1
            }
        } else {
          print("Error invalid trade.")
        }
        
          #
          # Execute the trade
          #
          if(bad.buy.order == 0 & bad.sell.order == 0 & single_trade$ValidTicker == TRUE){
            t <- single_trade$LocalTicker
            ac <- single_trade$Action
            q <- single_trade$Quantity
            o <- single_trade$OrderType
            p <- single_trade$AdjustedLimitPrice
            c <- single_trade$Currency
            gtd <- format(Sys.Date(),"%Y%m%d 23:59:5900")
          
            trd_id <- reqIds(ts$ts_conn)
            if(tolower(o) == "mkt"){
              # Market Order
              trd_res <- placeOrder(ts$ts_conn,
                                    twsEquity(symbol = t,
                                              currency = c),
                                    twsOrder(orderId = trd_id,
                                              action = ac,
                                              totalQuantity = q,
                                              orderType = "MKT",
                                              transmit = ts$ts_trade_transmit_switch,
                                              #goodTillDate = gtd,
                                              outsideRTH = "1"))
            } else if (tolower(o) == "lmt") {
            # Limit order
              trd_res <- placeOrder(ts$ts_conn,
                                    twsEquity(symbol = t,
                                              currency = c),
                                    twsOrder(orderId = trd_id,
                                              action = ac,
                                              totalQuantity = q,
                                              orderType = "LMT",
                                              lmtPrice = p,
                                              transmit = ts$ts_trade_transmit_switch,
                                              #goodTillDate = gtd,
                                              outsideRTH = "1"))
            
            } else {
              print("Invalid order type. Only Mkt and Lmt are currently supported!")
            }
            ts$ts_trade_ids <- c(ts$ts_trade_ids, trd_id)
            ts$ts_trade_results <- c(ts$ts_trade_results, "Successful")
        } else {
          ts$ts_trade_results <- c(ts$ts_trade_results, "Fail")
		      msg <- paste0("$",single_trade$LocalTicker,
                      ifelse(bad.buy.order == 1, ": Bad buy order!", ": Bad sell order!"))
		      ts$ts_last_trade_message <- msg
          print(msg)
        }
      } else {
		    msg <- "Error currently only equity is allowed to be trade!"
		    ts$ts_last_trade_message <- msg
        print(msg)
      }
    } else {
      msg <- "Error Trade list structure is invalid."
	    ts$ts_last_trade_message <- msg
      print(msg)
    }
  } else {
	  msg <- "Error connection to IB TWS!"
	  ts$ts_last_trade_message <- msg
    print(msg)
  }
  return(ts)
}

#
# Place trades 
#
TSExecuteAllTrades <- function(ts) UseMethod("TSExecuteAllTrades")
TSExecuteAllTrades.default <- function(ts){ return(ts) }
TSExecuteAllTrades.TradingSession <- function(ts){
  if(isConnected(ts$ts_conn)){
    if(nrow(ts$ts_fnlTradelist) != 0){
      for(i in 1:nrow(ts$ts_fnlTradelist)){
        ts <- TSExecuteTrade(ts, ts$ts_fnlTradelist[i,])
      }
      ts <- TSGenTradeResults(ts)
    } else {
      print("Error trade list is empty!")
    }
  } else {
    print("Error connection to IB TWS!")
  }
  return(ts)
}

#
# Cancel Order
#
TSCancelTrade <- function(ts, trd_id) UseMethod("TSCancelTrade")
TSCancelTrade.default <- function(ts, trd_id){ return(ts) }
TSCancelTrade.TradingSession <- function(ts, trd_id){
  if(isConnected(ts$ts_conn)){
    cancelOrder(ts$ts_conn, trd_id)
    ts$ts_trade_ids <- ts$ts_trade_ids [! ts$ts_trade_ids %in% trd_id]
  } else {
    print("Error connection to IB TWS!")
  }
  return(ts)
}

#
# Cancel all trades
#
TSCancelAllTrades <- function(ts) UseMethod("TSCancelAllTrades")
TSCancelAllTrades.default <- function(ts){ return(ts) }
TSCancelAllTrades.TradingSession <- function(ts){
  if(isConnected(ts$ts_conn)){
    for(id in ts$ts_trade_ids){
      ts <- TSCancelTrade(ts, id)
    }
  } else {
    print("Error connection to IB TWS!")
  }
  return(ts)
}

#
# Set transmit
#
TSSetTransmit <- function(ts, flg) UseMethod("TSSetTransmit")
TSSetTransmit.default <- function(ts, flg){ return(ts) }
TSSetTransmit.TradingSession <- function(ts, flg){
  if(isConnected(ts$ts_conn)){
    ts$ts_trade_transmit_switch <- flg
  } else {
    print("Error connection to IB TWS!")
  }
  return(ts)
}

#
# Set order good until
#
TSSetGoodUntil <- function(ts, dt) UseMethod("TSSetGoodUntil")
TSSetGoodUntil.default <- function(ts, dt){ return(ts) }
TSSetGoodUntil.TradingSession <- function(ts, dt){
  if(isConnected(ts$ts_conn)){
    if(class(dt) == "date"){
      ts$ts_trade_gooduntildate <- dt
    } else {
      print("Invalid date to be set.")
    }
  } else {
    print("Error connection to IB TWS!")
  }
  return(ts)
}

#
# Show trade results
#
TSGenTradeResults <- function(ts) UseMethod("TSGenTradeResults")
TSGenTradeResults.default <- function(ts){ return(ts) }
TSGenTradeResults.TradingSession <- function(ts){
  if(isConnected(ts$ts_conn)){
    temp_df <- ts$ts_fnlTradelist[,c("LocalTicker","Currency")]
    temp_df$TradeID <- ts$ts_trade_ids
    temp_df$TradeResult <- ts$ts_trade_results
    ts$ts_trade_summary <- temp_df
  } else {
    print("Error connection to IB TWS!")
  }
  return(ts)
}

#
# Show trade results
#
TSTradeResults <- function(ts) UseMethod("TSTradeResults")
TSTradeResults.default <- function(ts){ return(ts) }
TSTradeResults.TradingSession <- function(ts)  return(ts$ts_trade_summary)

#
# Close trade session
#
TSCloseTradingSession <- function(ts) UseMethod("TSCloseTradingSession")
TSCloseTradingSession.default <- function(ts){ return(ts) }
TSCloseTradingSession.TradingSession <- function(ts){
  if(isConnected(ts$ts_conn)){
    twsDisconnect(ts$ts_conn)
  }
  if(isConnected(ts$ts_conn)){
    res.msg <- paste("Trading session ", ts$ts_client_id, " is still open.", sep="")
  } else {
    res.msg <- paste("Trading session ", ts$ts_client_id, " is closed.", sep="")
  }
  print(res.msg)
}
