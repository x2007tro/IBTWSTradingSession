#####################################################################################################################
################################################# A S3 Class used to create trading tool ############################
#####################################################################################################################
################################################# S3 Class reference: ###############################################
################################################# http://www.cyclismo.org/tutorial/R/s3Classes.html #################
#####################################################################################################################

#
# Load libraries
#
libraries <- c("IBrokers","quantmod","R6")
lapply(libraries, function(lib){ library(lib, character.only=TRUE) })

##
# R6 class of trading session
##
IBTradingSession <- R6::R6Class(
  classname = "IBTradingSession",
  private = list(
    # empty
  ),
  public = list(
    ##
    # parameters that should not be changed
    def_port_holdings_colnames = c("Date","LocalTicker","Right","Expiry","Strike","SecurityType","Exchange","Currency","Position",
                                   "MktPrc","MktVal","AvgCost","UnrealizedPNL","RealizedPNL","TimeStamp"),
    def_port_info_colnames = c("Date", "Value", "Currency", "TimeStamp"),
    def_watchlist_colnames = c("LocalTicker","Currency","SecurityType","Comments"),
    def_prelimtradelist_colnames = c("LocalTicker","Right","Expiry","Strike","Exchange","Action","Quantity","OrderType","LimitPrice",
                                     "SecurityType","Currency","TradeSwitch"),
    def_fnltradelist_colnames = c("LocalTicker","Right","Expiry","Strike","Exchange","Action","Quantity","OrderType","AdjustedLimitPrice","LastTradePrice","SecurityType",
                                  "Currency","Commission","ValidTicker"),
    
    ##
    # variables
    ts_client_id = 0,
    ts_conn = NULL,
    ts_session_start_time = NULL,
    ts_server_version = NULL,
    ts_comm = list(
      STK = list(
        USD = 0.005,
        CAD = 0.01
      ),
      OPT = list(
        USD = 0.70,
        CAD = 1.25
      )
    ),
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
    ts_watchlist = NULL,
    ts_prelimTradelist = NULL,
    ts_fnlTradelist = NULL,
    ts_trade_ids = NULL,
    ts_trade_results = NULL,
    ts_trade_summary = NULL,
    ts_trade_transmit_switch = FALSE,
    ts_last_trade_message = NULL,
    ts_trade_gooduntildate = NULL,
    ts_db_obj = NULL,
    ts_app_status = app_sta,
    
    ##
    # initialize function
    initialize = function(c_id, c_type = c("TWS", "IBG"), acct_type = c("Live", "Paper"), l_host = "localhost"){
      
      ##
      # some checks
      ct <- match.arg(c_type)
      at <- match.arg(acct_type)
      
      ##
      # connect to trade session
      if(ct == "TWS"){
        if(at == "Live"){
          my_conn <- twsConnect(clientId = c_id, host = l_host, port = 7496)
        } else {
          my_conn <- twsConnect(clientId = c_id, host = l_host, port = 7497)
        }
      } else if (ct == "IBG"){
        if(at == "Live"){
          my_conn <- ibgConnect(clientId = c_id, host = l_host, port = 4001)
        } else {
          my_conn <- ibgConnect(clientId = c_id, host = l_host, port = 4002)
        }
      } else {
        TSLogError(paste0("Connection type ", ct, " is not supported!"), "Error", "initialize")
      }
      
      ##
      # initialize variables
      self$ts_client_id <- c_id
      self$ts_conn <- my_conn
      self$ts_session_start_time <- reqCurrentTime(my_conn)
      self$ts_server_version <- serverVersion(my_conn)
      self$ts_port_holdings <- NULL
      self$ts_port_info <- NULL
      self$ts_ca_net_liquidation <- 0
      self$ts_ca_gross_position_value <- 0
      self$ts_ca_equity_with_loan_value <- 0
      self$ts_ca_total_cash_value <- 0
      self$ts_us_cash_balance <- 0
      self$ts_ca_cash_balance <- 0
      self$ts_ca_accrued_divdend <- 0
      self$ts_exchange_rate <- 0
      self$ts_watchlist <- data.frame(
        LocalTicker = character(0),
        Currency = character(0),
        SecurityType = character(0),
        Comments = character(0),
        stringsAsFactors = FALSE
      )
      self$ts_prelimTradelist <- data.frame(
        LocalTicker = character(0),
        Action = character(0),
        Quantity = numeric(0),
        OrderType = character(0),
        LimitPrice = numeric(0),
        SecurityType = character(0),
        Currency = character(0),
        stringsAsFactors = FALSE
      )
      self$ts_fnlTradelist <- data.frame(
        LocalTicker = character(0),
        Action = character(0),
        Quantity = numeric(0),
        OrderType = character(0),
        AdjustedLimitPrice = numeric(0),
        Commission = character(0),
        Currency = character(0),
        stringsAsFactors = FALSE
      )
      self$ts_trade_ids <- NULL
      self$ts_trade_results <- NULL
      self$ts_trade_summary <- NULL
      self$ts_trade_transmit_switch <- FALSE
      self$ts_last_trade_message <- NULL
      self$ts_trade_gooduntildate <- as.Date(reqCurrentTime(my_conn))
      self$ts_db_obj <- list(
        srv = "192.168.2.120,3773",
        dbn = "WebappAdmin",
        id = "kmin",
        pwd = "yuheng"
      )
      
    },
    
    ##
    # Connect to SQL server using connection string
    TSConnSqlServer = function(db_obj){
      conn <- DBI::dbConnect(
        drv = odbc::odbc(),
        Driver = "ODBC Driver 17 for SQL Server",
        Server = db_obj$srv,
        Database = db_obj$dbn,
        UID = db_obj$id,
        PWD = db_obj$pwd
      )
      return(conn)
    },
    
    ##
    # Write a table to sql server db
    TSWriteDataToSS = function(db_obj, data, tbl_name, apd = TRUE){
      conn <- self$TSConnSqlServer(db_obj)
      df <- DBI::dbWriteTable(conn, name = tbl_name, value = data,
                              append = apd, overwrite = !apd, row.names = FALSE)
      DBI::dbDisconnect(conn) 				  
      invisible(self)
    },
    
    ##
    # Add error message
    TSLogError = function(app_sta, msg, tp, loc){
      msg <- data.frame(
        ApplicationStatus = app_sta,
        Message = msg,
        Type = tp,
        Location = loc,
        Timestamp = Sys.time(),
        stringsAsFactors = FALSE
      )
      self$TSWriteDataToSS(self$ts_db_obj, msg, "MyBroKe_ErrorLog")
      invisible(self)
    },
    
    ##
    # IsConnected
    TSIsConnected = function(){
      if(isConnected(self$ts_conn)){
        return(TRUE)
      } else {
        return(FALSE)
      }
    },
    
    ##
    # RetrievePortHoldings
    TSUpdatePortHoldings = function(){
      if(isConnected(self$ts_conn)){
        km.port.prelim <- reqAccountUpdates(self$ts_conn)
        
        #
        # Retrieve portfolio holdings data
        #
        km.port.holdings.prelim <- km.port.prelim[[2]]
        
        if(length(km.port.holdings.prelim) == 0){
          km.port.holdings <- data.frame(as.Date(integer(0)), character(0), character(0), character(0), character(0), 
                                         numeric(0), numeric(0), numeric(0), numeric(0), numeric(0), numeric(0), as.Date(integer(0)))
          colnames(km.port.holdings) <- self$def_port_holdings_colnames
        } else {
          res <- lapply(1:length(km.port.holdings.prelim), function(i){
            symbol <- km.port.holdings.prelim[[i]]$contract$symbol
            cont_right <- km.port.holdings.prelim[[i]]$contract$right
            expiry <- km.port.holdings.prelim[[i]]$contract$expiry
            strike <- km.port.holdings.prelim[[i]]$contract$strike
            sectype <- km.port.holdings.prelim[[i]]$contract$sectype
            exc <- km.port.holdings.prelim[[i]]$contract$primary
            curr <- km.port.holdings.prelim[[i]]$contract$currency
            pos <- km.port.holdings.prelim[[i]]$portfolioValue$position
            prc <- km.port.holdings.prelim[[i]]$portfolioValue$marketPrice
            val <- km.port.holdings.prelim[[i]]$portfolioValue$marketValue
            if(sectype == "OPT"){
              prc <- prc * 100
              val <- val * 100
            }
            avgcost <- km.port.holdings.prelim[[i]]$portfolioValue$averageCost
            unrealizedPNL <- km.port.holdings.prelim[[i]]$portfolioValue$unrealizedPNL
            realizedPNL <- km.port.holdings.prelim[[i]]$portfolioValue$realizedPNL
            dt <- as.Date(self$ts_session_start_time)
            
            holding <- data.frame(dt, symbol, cont_right, expiry, strike, sectype, exc, curr, pos, prc, val, avgcost, unrealizedPNL, realizedPNL, self$ts_session_start_time,
                                  stringsAsFactors = FALSE)
            colnames(holding) <- self$def_port_holdings_colnames
            return(holding)
          })
          km.port.holdings <- dplyr::bind_rows(res)
        }
        
        self$ts_port_holdings <- km.port.holdings
        
        ##
        # add code to enter port holdings data into database
        
      } else {
        self$TSLogError(self$ts_app_status, paste0("Connection to IB is not active."), "Error", "TSRetrievePortHoldings")
      }
      
      invisible(self)
    },
    
    ##
    # get portfolio holdings
    TSGetPortHoldings = function(){
      return(self$ts_port_holdings)
    },
    
    ##
    # RetrievePortfolioInfo
    TSUpdatePortInfo = function(){
      if(isConnected(self$ts_conn)){
        km.port.prelim <- reqAccountUpdates(self$ts_conn)
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
            self$TSLogError(self$ts_app_status, paste0("Multiple currencies exist!"), "Warning", "TSRetrievePortInfo")
          }
          rownames(cash.balance.rev) <- c("CashBalanceUS","CashBalanceCA")
          colnames(cash.balance.rev) <- c("Value","Currency")
          km.port.info <- rbind.data.frame(km.port.info, cash.balance.rev)
          
          km.port.info$Date <- rep(as.Date(self$ts_session_start_time), nrow(km.port.info))
          km.port.info$TimeStamp <- rep(self$ts_session_start_time, nrow(km.port.info))
          km.port.info <- km.port.info[,self$def_port_info_colnames]
          
          #
          # Assign individual metric
          #
          self$ts_ca_net_liquidation <- km.port.info["NetLiquidation","Value"]
          self$ts_ca_gross_position_value <- km.port.info["GrossPositionValue","Value"]
          self$ts_ca_equity_with_loan_value <- km.port.info["EquityWithLoanValue","Value"]
          self$ts_ca_total_cash_value <- km.port.info["TotalCashValue","Value"]
          self$ts_us_cash_balance <- km.port.info["CashBalanceUS","Value"]
          self$ts_ca_cash_balance <- km.port.info["CashBalanceCA","Value"]
          self$ts_ca_accrued_divdend <- km.port.info["AccruedDividend","Value"]
          self$ts_exchange_rate <- km.port.info["ExchangeRate","Value"]
          
          #
          # Assign port info data frame
          #
          self$ts_port_info <- km.port.info
          
          ##
          # Add code to add port info to database
          
        } else {
          self$TSLogError(self$ts_app_status, paste0("Currency not as expected!"), "Warning", "TSRetrievePortInfo")
        }
      } else {
        self$TSLogError(self$ts_app_status, paste0("Connection to IB is not active."), "Error", "TSRetrievePortInfo")
      }
      
      invisible(self)
    },
    
    ##
    # get portfolio info
    TSGetPortInfo = function(){
      return(self$ts_port_info)
    },
    
    ##
    # get contract info
    TSProcessContract = function(contract){
      #
      # currently, only support equity and option
      #
      tmp <- lapply(1:length(contract), function(i, opt1){
        item <- opt1[[i]]
        res <- data.frame(
          CompanyName = item$longName,
          Industry = item$industry,
          Category = item$category,
          SubCategory = item$subcategory,
          TimeZone = item$timeZoneId,
          ReqExchange = item$contract$exch,
          PriExchange = item$contract$primary,
          IBSymbol = item$contract$symbol,
          LocalSymbol = item$contract$local,
          SecurityType = item$contract$sectype,
          ContractRight = item$contract$right,
          Expiry = item$contract$expiry,
          Strike = item$contract$strike,
          Currency = item$contract$currency,
          Multiplier = item$contract$multiplier,
          stringsAsFactors = FALSE
        )
        return(res)
      }, contract)
      return(tmp %>% dplyr::bind_rows())
    },
    
    ##
    # generate contract and request contract details
    TSGetContractDetails = function(sym, cur = "", sec_typ = c("equity", "option")){
      st <- sec_typ
      
      if(isConnected(self$ts_conn)){
        cont <- NULL
        if(st == "equity"){
          cont <- twsEquity(
            local = "",
            symbol = sym,
            exch = "SMART",
            currency = cur
          )
        } else if (st == "option"){
          cont <- twsOPT(
            local = "",
            symbol = sym,
            right = "",    # both call and put
            expiry = "",
            strike = "",
            exch = "SMART",
            primary = "",
            currency = cur
          )
        } else {
          self$TSLogError(self$ts_app_status, paste0("The security type ", ct, " you specified is not supported!"), "Error", "TSGetContractDetails")
        }
        
        if(!is.null(cont)){
          cont_info <- IBrokers::reqContractDetails(self$ts_conn, cont)
          if(length(cont_info) != 0){
            res <- self$TSProcessContract(cont_info)
          } else {
            res <- data.frame(Message = "Contract details cannot be retrieved due to invalid contract parameters!", stringsAsFactors = FALSE)
          }
        } else {
          res <- data.frame(Message = "Contract details cannot be retrieved due to invalid security type!", stringsAsFactors = FALSE)
        }
      } else {
        self$TSLogError(self$ts_app_status, paste0("Connection to IB is not active."), "Error", "TSGetContractDetails")
        res <- data.frame(Message = "Contract details cannot be retrieved due to inactive IB connection!", stringsAsFactors = FALSE)
      }
      
      return(res)
    },
    
    ##
    # Set watchlist
    TSSetWatchlist = function(ws){
      if(isConnected(self$ts_conn)){
        if(identical(colnames(ws), self$def_watchlist_colnames)){
          self$ts_watchlist <- ws
        } else {
          self$TSLogError(self$ts_app_status, paste0("User entered watchlist has unexpected structure!"), "Error", "TSSetWatchlist")
        }
      } else {
        self$TSLogError(self$ts_app_status, paste0("Connection to IB is not active."), "Error", "TSSetWatchlist")
      }
      invisible(self)
    },
    
    ##
    # Set prelim tradelist
    TSSetPrelimTradeList = function(man_trd_df){
      
      if(isConnected(self$ts_conn)){
        if(identical(colnames(man_trd_df), self$def_prelimtradelist_colnames)){
          self$ts_prelimTradelist <- man_trd_df
        } else{
          self$TSLogError(self$ts_app_status, paste0("User entered tradelist has unexpected structure!"), "Error", "TSSetPrelimTradeList")
        }
      } else {
        self$TSLogError(self$ts_app_status, paste0("Connection to IB is not active."), "Error", "TSSetPrelimTradeList")
      }
      invisible(self)
    },
    
    ##
    # Generate final trade list
    TSGenFnlTradeList = function(){
      
      if(isConnected(self$ts_conn)){
        # Field: "LocalTicker","Action","Quantity","OrderType","LimitPrice","SecurityType","Currency"
        man_trd_df <- self$ts_prelimTradelist
        if(nrow(man_trd_df) != 0){
          man_trd_df$YahooTicker <- paste0(man_trd_df$LocalTicker,ifelse(man_trd_df$Currency=="CAD",".TO",""))
          
          man_trd_df$LastTradePrice <- rep(0,nrow(man_trd_df))
          man_trd_df$AdjustedLimitPrice <- man_trd_df$LimitPrice
          man_trd_df$LastTimeStamp <- rep(Sys.Date(),nrow(man_trd_df))
          man_trd_df$Commission <- rep(0,nrow(man_trd_df))
          man_trd_df$ValidTicker <- rep(TRUE,nrow(man_trd_df))
          
          for(i in 1:nrow(man_trd_df)){
            #
            # Update commission
            #
            if(man_trd_df[i,"Currency"] == "USD" | man_trd_df[i,"Currency"] == "CAD"){
              base_comm <- self$ts_comm[[man_trd_df[i,"SecurityType"]]][[man_trd_df[i,"Currency"]]]
              man_trd_df[i,"Commission"] <- max(1, base_comm*man_trd_df[i,"Quantity"], na.rm = TRUE)
            } else {
              self$TSLogError(self$ts_app_status, paste0("Non-north america trade exist!"), "Warning", "TSGenFnlTradeList")
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
          
          self$ts_fnlTradelist <- man_trd_df[,self$def_fnltradelist_colnames]
        }
      } else {
        self$TSLogError(self$ts_app_status, paste0("Connection to IB is not active."), "Error", "TSGenFnlTradeList")
      }
      invisible(self)
    },
    
    ##
    # Execute single trade
    TSExecuteTrade = function(single_trade){
      if(isConnected(self$ts_conn)){
        #
        # Request most up-to-date account info
        #
        self$TSUpdatePortHoldings()
        curr_holdings <- self$TSGetPortHoldings()
        
        self$TSUpdatePortInfo()
        us_cash_balance <- self$ts_us_cash_balance
        ca_cash_balance <- self$ts_ca_cash_balance
        exch_rate <- self$ts_exchange_rate
        
        if(identical(colnames(single_trade), self$def_fnltradelist_colnames) | tolower(single_trade[,"SecurityType"]) == "forex"){
          #
          # Validate and execute a single trade
          #
          bad.sell.order <- 0
          bad.buy.order <- 0
          if(tolower(single_trade[,"SecurityType"]) == "stk" | tolower(single_trade[,"SecurityType"]) == "opt"){
            
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
              if(us.order.value > max(us_cash_balance,0) | ca.order.value > max(ca_cash_balance,0)){
                bad.buy.order <- 1
              }
            } else {
              self$TSLogError(self$ts_app_status, paste0("Trade type ", single_trade$Action, " is not valid!"), "Error", "TSExecuteTrade")
            }
            
            #
            # Execute the trade
            #
            if(bad.buy.order == 0 & bad.sell.order == 0 & single_trade$ValidTicker == TRUE){
              t <- single_trade$LocalTicker
              ri <- single_trade$Right
              ex <- single_trade$Expiry
              stk <- single_trade$Strike
              pe <- single_trade$Exchange
              ac <- single_trade$Action
              q <- single_trade$Quantity
              o <- single_trade$OrderType
              sect <- single_trade$SecurityType
              p <- single_trade$AdjustedLimitPrice
              c <- single_trade$Currency

              gtd <- format(Sys.Date(),"%Y%m%d 23:59:5900")
              
              ##
              # make contract for stock and option
              if(tolower(sect) == "stk"){
                cont <- IBrokers::twsEquity(
                  symbol = t,
                  exch = "SMART",
                  primary = pe,
                  currency = c
                )
              } else {
                cont <- IBrokers::twsOPT(
                  local = "",
                  symbol = t,
                  right = ri,    # both call and put
                  expiry = ex,
                  strike = stk,
                  exch = "SMART",
                  primary = "",
                  currency = c
                )
              }
              
              if(tolower(o) == "mkt"){
                # Market Order
                trd_id <- reqIds(self$ts_conn)
                trd_res <- placeOrder(self$ts_conn,
                                      cont,
                                      twsOrder(orderId = trd_id,
                                               action = ac,
                                               totalQuantity = q,
                                               orderType = "MKT",
                                               transmit = self$ts_trade_transmit_switch,
                                               #goodTillDate = gtd,
                                               outsideRTH = "1"))
                Sys.sleep(1)
              } else if (tolower(o) == "lmt") {
                # Limit order
                trd_id <- reqIds(self$ts_conn)
                trd_res <- placeOrder(self$ts_conn,
                                      cont,
                                      twsOrder(orderId = trd_id,
                                               action = ac,
                                               totalQuantity = q,
                                               orderType = "LMT",
                                               lmtPrice = p,
                                               transmit = self$ts_trade_transmit_switch,
                                               #goodTillDate = gtd,
                                               outsideRTH = "1"))
                Sys.sleep(1)
              } else {
                trd_id <- -1
                self$TSLogError(self$ts_app_status, paste0("Trade type ", o, " is not valid for trade #", trd_id,"!"), "Error", "TSExecuteTrade")
              }
              self$ts_trade_ids <- c(self$ts_trade_ids, trd_id)
              self$ts_trade_results <- c(self$ts_trade_results, "Successful")
            } else {  
              # order validation failed
              self$ts_trade_ids <- c(self$ts_trade_ids, -1)
              self$ts_trade_results <- c(self$ts_trade_results, "Fail")
              msg <- paste0("$",single_trade$LocalTicker,
                            ifelse(bad.buy.order == 1, ": Bad buy order!", ": Bad sell order!"))
              self$TSLogError(self$ts_app_status, msg, "Error", "TSExecuteTrade")
              self$ts_last_trade_message <- msg
              print(msg)
            }
          } else if(tolower(single_trade[,"SecurityType"]) == "forex"){
            #
            # Validate a trade
            #
            tgt_curr <- single_trade[,"TargetCurrency"]
            tgt_value <- single_trade[,"TargetValue"]
            
            if(tgt_curr == "CAD"){
              side <- "SELL"
              trade_value <- round(tgt_value/exch_rate, 0)
              us_cash_order <- trade_value
              if(us_cash_balance < us_cash_order){
                bad.buy.order = 1
              }
            } else if (tgt_curr == "USD"){
              side <- "BUY"
              trade_value <- tgt_value
              ca_order_value <- exch_rate * trade_value
              if(ca_cash_balance < ca_order_value){
                bad.buy.order = 1
              }
            } else {
              self$TSLogError(self$ts_app_status, paste0("Error only USD and CAD are allowed to traded."), "Error", "TSExecuteTrade")
            }
            #
            # Execute the trade
            #
            if(bad.buy.order == 0){
              gtd <- format(Sys.Date(),"%Y%m%d 23:59:5900")
              
              trd_id <- reqIds(self$ts_conn)
              trd_res <- placeOrder(self$ts_conn,
                                    twsCurrency(symbol = "USD",
                                                currency = "CAD"),
                                    twsOrder(orderId = trd_id,
                                             action = side,
                                             totalQuantity = trade_value,
                                             orderType = "MKT",
                                             transmit = self$ts_trade_transmit_switch,
                                             #goodTillDate = gtd,
                                             outsideRTH = "1"))
              Sys.sleep(1)
              self$ts_trade_ids <- c(self$ts_trade_ids, trd_id)
              self$ts_trade_results <- c(self$ts_trade_results, "Successful")
            } else {
              self$ts_trade_ids <- c(self$ts_trade_ids, -1)
              self$ts_trade_results <- c(self$ts_trade_results, "Fail")
              msg <- paste0("forex", ifelse(bad.buy.order == 1, ": Bad buy order!", ": Bad sell order!"))
              self$TSLogError(self$ts_app_status, msg, "Error", "TSExecuteTrade")
              self$ts_last_trade_message <- msg
              print(msg)
            }
          } else {
            self$ts_trade_ids <- c(self$ts_trade_ids, -1)
            self$ts_trade_results <- c(self$ts_trade_results, "Fail")
            msg <- "Error currently only equity and currenct are allowed to be traded!"
            self$TSLogError(self$ts_app_status, msg, "Error", "TSExecuteTrade")
            self$ts_last_trade_message <- msg
            print(msg)
          }
          
        } else {
          self$ts_trade_ids <- c(self$ts_trade_ids, -1)
          self$ts_trade_results <- c(self$ts_trade_results, "Fail")
          msg <- "Error Trade list structure is invalid."
          self$TSLogError(self$ts_app_status, msg, "Error", "TSExecuteTrade")
          self$ts_last_trade_message <- msg
          print(msg)
        }
      } else {
        self$ts_trade_ids <- c(self$ts_trade_ids, -1)
        self$ts_trade_results <- c(self$ts_trade_results, "Fail")
        msg <- "Connection to IB is not active."
        self$TSLogError(self$ts_app_status, msg, "Error", "TSExecuteTrade")
        self$ts_last_trade_message <- msg
        print(msg)
      }
      invisible(self)
    },
    
    ##
    # Execute all trade
    TSExecuteAllTrades = function(){
      if(isConnected(self$ts_conn)){
        if(nrow(self$ts_fnlTradelist) != 0){
          for(i in 1:nrow(self$ts_fnlTradelist)){
            self$TSExecuteTrade(self$ts_fnlTradelist[i,])
          }
          self$TSGenTradeResults()
        } else {
          self$TSLogError(self$ts_app_status, paste0("Error trade list is empty!"), "Error", "TSExecuteAllTrades")
        }
      } else {
        self$TSLogError(self$ts_app_status, paste0("Connection to IB is not active."), "Error", "TSExecuteAllTrades")
      }
      invisible(self)
    },
    
    ##
    # Cancel single trade
    TSCancelTrade = function(trd_id){
      if(isConnected(self$ts_conn)){
        cancelOrder(self$ts_conn, trd_id)
        self$ts_trade_ids <- self$ts_trade_ids[! self$ts_trade_ids %in% trd_id]
      } else {
        self$TSLogError(self$ts_app_status, paste0("Connection to IB is not active."), "Error", "TSCancelTrade")
      }
      invisible(self)
    },
    
    ## 
    # Cancel all trades
    TSCancelAllTrades = function(){
      if(isConnected(self$ts_conn)){
        all_trades <- self$ts_trade_ids
        all_valid_trades <- all_trades[all_trades != -1]
        for(id in all_valid_trades){
          self$TSCancelTrade(id)
        }
      } else {
        self$TSLogError(self$ts_app_status, paste0("Connection to IB is not active."), "Error", "TSCancelAllTrades")
      }
      invisible(self)
    },
    
    ##
    # Set transmit
    TSSetTransmit = function(flg){
      if(isConnected(self$ts_conn)){
        self$ts_trade_transmit_switch <- flg
      } else {
        self$TSLogError(self$ts_app_status, paste0("Connection to IB is not active."), "Error", "TSSetTransmit")
      }
      invisible(self)
    },
    
    ##
    # set gooduntil time
    TSSetGoodUntil = function(dt){
      if(isConnected(self$ts_conn)){
        if(class(dt) == "date"){
          self$ts_trade_gooduntildate <- dt
        } else {
          self$TSLogError(self$ts_app_status, paste0("Invalid date ", dt, " to be set."), "Error", "TSSetGoodUntil")
        }
      } else {
        self$TSLogError(self$ts_app_status, paste0("Connection to IB is not active."), "Error", "TSSetGoodUntil")
      }
      invisible(self)
    },
    
    ##
    # generate trade result
    TSGenTradeResults = function(){
      if(isConnected(self$ts_conn)){
        temp_df <- self$ts_fnlTradelist[,c("LocalTicker","Currency")]
        exp_trd_cnt <- nrow(temp_df)
        act_trd_cnt <- length(self$ts_trade_ids)
        temp_df$TradeID <- self$ts_trade_ids[(act_trd_cnt-exp_trd_cnt+1):act_trd_cnt]
        temp_df$TradeResult <- self$ts_trade_results[(act_trd_cnt-exp_trd_cnt+1):act_trd_cnt]
        self$ts_trade_summary <- temp_df
      } else {
        self$TSLogError(self$ts_app_status, paste0("Connection to IB is not active."), "Error", "TSGenTradeResults")
      }
      invisible(self)
    },
    
    ##
    # get trade result
    TSGetTradeResults = function(){
      return(self$ts_trade_summary)
    },
    
    ##
    # close trade session
    TSCloseTradingSession = function(){
      if(isConnected(self$ts_conn)){
        twsDisconnect(self$ts_conn)
      }
      if(isConnected(self$ts_conn)){
        res.msg <- paste("Trading session ", self$ts_client_id, " is still open.", sep="")
      } else {
        res.msg <- paste("Trading session ", self$ts_client_id, " is closed.", sep="")
      }
      self$TSLogError(self$ts_app_status, res.msg, "Info", "TSCloseTradingSession")
    }
    
    ##
    # future functions starts here
  )
)

