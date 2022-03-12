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
options(scipen = 999)

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
    def_port_holdings_remain = c("symbol","right","expiry","strike","sectype","primary","currency","position",
                                 "averageCost","marketPrice","marketValue","unrealizedPNL","realizedPNL"),
    def_port_holdings_colnames = c("Market Date","Symbol","Right","Expiry","Strike","Security Type","Exchange","Currency","Position",
                                   "Cost","Market Price","Market Value","Unrealized Profit","Realized Profit"),
    def_realized_profits_remain = c("Market Date","Market Datetime","Symbol","Right","Expiry","Strike","Security Type",
                                    "Exchange","Currency","Position","Cost","Realized Profit", "Trade Mode", "Application Status"),
    def_port_info_colnames = c("Date", "Metric", "Value", "Currency"),
    def_watchlist_colnames = c("Symbol","Currency","Security Type","Comments"),
    def_prelimtradelist_colnames = c("Symbol","Right","Expiry","Strike","Exchange","Action","Quantity","OrderType","LimitPrice",
                                     "Security Type","Currency","TradeSwitch"),
    def_fnltradelist_colnames = c("Symbol","Right","Expiry","Strike","Exchange","Action","Quantity","OrderType","AdjustedLimitPrice","LastTradePrice","Security Type",
                                  "Currency","Commission","ValidTicker"),
    
    ##
    # variables
    ts_client_id = 0,
    ts_platform = NULL,
    ts_account_type = NULL,
    ts_trade_mode = NULL,
    ts_app_status = NULL, 
    ts_conn = NULL,
    ts_session_start_time = NULL,
    ts_server_version = NULL,
    ts_comm = list(
      STK = list(
        USD = 0.005,
        CAD = 0.01,
        AUD = 0,
        EUR = 0,
        GBP = 0,
        NZD = 0
      ),
      OPT = list(
        USD = 0.70,
        CAD = 1.25,
        AUD = 0,
        EUR = 0,
        GBP = 0,
        NZD = 0
      )
    ),
    ts_port_holdings_nonforex = NULL,
    ts_port_holdings_forex = NULL,
    ts_port_info = NULL,
    ts_cash_balance = NULL,
    ts_acc_recon = NULL,
    ts_tradable_currency = tradable_curr,   # from global
    ts_watchlist = NULL,
    ts_prelimTradelist = NULL,
    ts_fnlTradelist = NULL,
    ts_trade_ids = NULL,
    ts_trade_results = NULL,
    ts_trade_summary = NULL,
    ts_trade_transmit_switch = FALSE,
    ts_last_trade_message = NULL,
    ts_trade_gooduntildate = NULL,
    ts_db_obj = db_obj,   # from global
    
    ##
    # initialize function
    initialize = function(
      c_id, 
      c_type = c("TWS", "IBG"), 
      acct_type = c("Live", "Paper"), 
      l_host = "localhost"){
      
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
      self$ts_platform <- ct
      self$ts_account_type <- at
      self$ts_trade_mode <- at
      if(at == "Live") self$ts_app_status <- "Production" else self$ts_app_status <- "Test"
      self$ts_conn <- my_conn
      self$ts_session_start_time <- reqCurrentTime(my_conn)
      self$ts_server_version <- serverVersion(my_conn)
      self$ts_port_holdings_nonforex <- NULL
      self$ts_port_holdings_forex <- NULL
      self$ts_port_info <- NULL
      self$ts_cash_balance <- NULL
      self$ts_acc_recon <- NULL
      self$ts_watchlist <- data.frame(
        Symbol = character(0),
        Currency = character(0),
        `Security Type` = character(0),
        Comments = character(0),
        stringsAsFactors = FALSE
      )
      self$ts_prelimTradelist <- data.frame(
        Symbol = character(0),
        Action = character(0),
        Quantity = numeric(0),
        OrderType = character(0),
        LimitPrice = numeric(0),
        `Security Type` = character(0),
        Currency = character(0),
        stringsAsFactors = FALSE
      )
      self$ts_fnlTradelist <- data.frame(
        Symbol = character(0),
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
    # Connect to MariaDB using connection string
    ##
    TSConnMySql = function(db_obj){
      conn <- DBI::dbConnect(
        drv = RMariaDB::MariaDB(),
        user = db_obj$id,
        password = db_obj$pwd,
        dbname = db_obj$dbn,
        host = db_obj$srv,
        port = db_obj$prt
      )
      return(conn)
    },
    
    ##
    # Write a table to sql server db
    TSWriteDataToSS = function(db_obj, data, tbl_name, apd = TRUE){
      conn <- self$TSConnMySql(db_obj)
      df <- DBI::dbWriteTable(conn, name = tbl_name, value = data,
                              append = apd, overwrite = !apd, row.names = FALSE)
      DBI::dbDisconnect(conn) 				  
      invisible(self)
    },
    
    ##
    # Get query results from sql server db
    TSGetQueryResFromSS = function(db_obj, qry_str){
      conn <- self$TSConnMySql(db_obj)
      qry_conn <- DBI::dbSendQuery(conn, qry_str)
      res <- DBI::dbFetch(qry_conn)
      DBI::dbClearResult(qry_conn)
      DBI::dbDisconnect(conn)
      return(res)
    },
    
    ##
    # Read a table from sql server db
    TSReadDataFromSS = function(db_obj, tbl_name){
      conn <- self$TSConnMySql(db_obj)
      df <- DBI::dbReadTable(conn, tbl_name)
      DBI::dbDisconnect(conn) 				  
      return(df)
    },
    
    ##
    # Add error message
    TSLogError = function(app_sta, tm, msg, tp, loc){
      msg <- data.frame(
        ApplicationStatus = app_sta,
        TradeMode = tm,
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
    # Process account balance
    TSProcessAccBal = function(acc_bal){
      ##
      # convert info from list to data.frame
      res <- lapply(1:length(acc_bal), function(i, acc_bal1){
        tmp <- acc_bal1[[i]]
        
        tmp_df <- data.frame(
          Metric = names(acc_bal1)[i],
          Value = as.numeric(tmp['value']),
          Currency = tmp['currency'],
          stringsAsFactors = FALSE
        ) %>% dplyr::filter(!is.na(Value))
        return(tmp_df)
      }, acc_bal)
      res1 <- dplyr::bind_rows(res)
      return(res1)
    },
    
    ##
    # Process holding
    TSProcessHolding = function(holding){
      ##
      # convert info from list to data.frame
      res <- lapply(1:length(holding), function(i, holding1){
        tmp <- holding1[[i]]
        contract <- tmp$contract
        pv <- tmp$portfolioValue
        
        tmp2 <- cbind.data.frame(
          data.frame(t(unlist(contract)), stringsAsFactors = FALSE),
          data.frame(t(unlist(pv)), stringsAsFactors = FALSE)
        ) %>% dplyr::mutate(
          multiplier = ifelse(is.na(as.numeric(multiplier)), 1, as.numeric(multiplier)),
          right = ifelse(right == "0", "", right),
          expiry = ifelse(expiry == "0", "", expiry),
          strike = ifelse(strike == "0", "", strike)
        ) %>% dplyr::mutate(
          position = as.numeric(position),
          marketPrice = as.numeric(marketPrice)*multiplier,
          marketValue = as.numeric(marketValue),
          averageCost = as.numeric(averageCost),
          unrealizedPNL = as.numeric(unrealizedPNL),
          realizedPNL = as.numeric(realizedPNL)
        )
        
        return(tmp2)
      }, holding)
      res1 <- dplyr::bind_rows(res)
      return(res1)
    },
    
    ##
    # Update account details 
    TSUpdateAccountDetail = function(){
      tmp <- reqAccountUpdates(self$ts_conn)
      curr_mkt_datetime <- IBrokers::reqCurrentTime(self$ts_conn)
      curr_mkt_date <- as.Date(curr_mkt_datetime)
      
      # 1. process account balance
      acc_bal <- self$TSProcessAccBal(tmp[[1]])
      port_info <- acc_bal %>% 
        dplyr::mutate(`Market Date` = curr_mkt_date) %>% 
        dplyr::select(dplyr::one_of(c("Market Date", self$def_port_info_colnames)))
      self$ts_port_info <- port_info
      
      # 2.1 process holding
      holding <- self$TSProcessHolding(tmp[[2]])
      holding_cln <- holding %>% 
        dplyr::select(dplyr::one_of(self$def_port_holdings_remain)) %>% 
        dplyr::mutate(`Market Date` = curr_mkt_date) %>% 
        dplyr::select(dplyr::one_of(c("Market Date", self$def_port_holdings_remain)))
      colnames(holding_cln) <- self$def_port_holdings_colnames
      
      # 2.2 assign nonforex holding
      port_holdings_nonforex <- holding_cln %>% 
        dplyr::filter(Exchange != "IDEALPRO")
      self$ts_port_holdings_nonforex <- port_holdings_nonforex
      
      # add realized profit to database
      realized_profit <- port_holdings_nonforex %>% 
        dplyr::filter(`Realized Profit` != 0) %>% 
        dplyr::mutate(
          `Market Datetime` = format(curr_mkt_datetime, "%Y-%m-%d %H:%M:%S"),
          `Trade Mode` = self$ts_trade_mode,
          `Application Status` = self$ts_app_status) %>% 
        dplyr::select(dplyr::one_of(self$def_realized_profits_remain))
      self$TSWriteDataToSS(self$ts_db_obj, realized_profit, "MyBroKe_RealizedProfitHistory", apd = TRUE)
      
      # 2.3 assign forex holding
      port_holdings_forex <- holding_cln %>% 
        dplyr::filter(Exchange == "IDEALPRO")
      self$ts_port_holdings_forex <- port_holdings_forex
      
      # 3 calculate cash balance
      # non_cad_usd_cash <- sum(port_holdings_forex[port_holdings_forex$Symbol != "USD","Market Value"])
      non_cad_usd_cash <- sum(port_holdings_forex[,"Market Value"])
      cash_balance <- self$TSReadDataFromSS(self$ts_db_obj, "MyBroKe_CashBalanceMap") %>% 
        dplyr::left_join(port_holdings_forex, by = c("Currency" = "Symbol")) %>% 
        dplyr::mutate(
          `Market Date` = curr_mkt_date,
          Position = ifelse(is.na(Position), 0, Position),
          `Market Price` = ifelse(is.na(`Market Price`), 1, `Market Price`)) %>% 
        dplyr::mutate(
          Balance = ifelse(
            Currency == "USD", 
            port_info[port_info$Metric == "CashBalance" & port_info$Currency == "USD","Value"], 
            ifelse(
              Currency == "CAD", 
              port_info[port_info$Metric == "TotalCashValue" & port_info$Currency == "CAD","Value"] - non_cad_usd_cash,
              Position
            )),
          `Exchange Rate` = `Market Price`
        ) %>% 
        dplyr::mutate(
          `CAD Balance` = Balance * `Exchange Rate`
        ) %>% 
        dplyr::select(dplyr::one_of(c("Market Date","Name","Currency","Balance","Exchange Rate","CAD Balance")))
      self$ts_cash_balance <- cash_balance
      
      #
      # 4 account reconciliation
      #
      # holding group by currency and security_type (remove future)
      # + non-CAD, converted to CAD
      # + CAD (TotalCashValue - non-CAD, converted to CAD)
      # + non-CAD AccruedCash, converted to CAD
      # + CAD AccruedCash
      # + NetLiquidationUncertainty
      # = CalcNetLiquidation
      # - ActualNetLiquidation 
      # = Unreconciled amount
      #
      curr_mkt_datetime_str <- format(curr_mkt_datetime, "%Y-%m-%d %H:%M:%S")
      
      # security portion
      acc_recon1 <- port_holdings_nonforex %>% 
        dplyr::filter(`Security Type` != "FUT") %>%
        dplyr::group_by(Currency, `Security Type`) %>% 
        dplyr::summarise(Balance = sum(`Market Value`)) %>% 
        dplyr::mutate(`Market Date` = curr_mkt_date, `Market Datetime` = curr_mkt_datetime_str) %>% 
        dplyr::select(dplyr::one_of(c("Market Date","Market Datetime","Security Type","Currency","Balance")))
      
      # cash portion
      acc_recon2 <- cash_balance %>% 
        dplyr::mutate(`Market Datetime` = curr_mkt_datetime_str, `Security Type` = "Cash") %>% 
        dplyr::select(dplyr::one_of(c("Market Date","Market Datetime","Security Type","Currency","Balance")))
      
      # portfort info portion
      acc_recon3 <- data.frame(
        `Market Datetime` = rep(curr_mkt_datetime_str, 4),
        `Security Type` = c("AccruedCash","AccruedCash","NetLiquidationUncertainty","NetLiquidation"),
        Currency = c("USD", "CAD","CAD", "CAD"),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      acc_recon3 <- acc_recon3 %>% 
        dplyr::left_join(port_info[,], by = c("Currency", "Security Type" = "Metric")) %>% 
        dplyr::mutate(
          `Market Date` = curr_mkt_date,
          `Market Datetime` = curr_mkt_datetime_str,
          Balance = ifelse(is.na(Value), 0, Value)
        ) %>% 
        dplyr::select(dplyr::one_of(c("Market Date", "Market Datetime", "Security Type", "Currency", "Balance")))
      
      # combine all three portions
      tmp <- dplyr::bind_rows(list(
        acc_recon1,
        acc_recon2,
        acc_recon3
      ))
      
      # final tuning
      acc_recon <- tmp %>% 
        dplyr::left_join(cash_balance[,c("Currency","Exchange Rate")], by = c("Currency")) %>%
        dplyr::mutate(
          `CAD Balance` = Balance * `Exchange Rate`,
          `Trade mode` = self$ts_trade_mode,
          `Application Status` = self$ts_app_status
        )
      self$ts_acc_recon <- acc_recon
      self$TSWriteDataToSS(self$ts_db_obj, acc_recon, "MyBroKe_DailyAccountReconciliation", apd = TRUE)
      
      invisible(self)
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
          `Security Type` = item$contract$sectype,
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
    TSGetContractDetails = function(sym, cur = "", sec_typ = c("equity", "option", "forex", "future")){
      st <- sec_typ
      
      if(isConnected(self$ts_conn)){
        cont <- NULL
        if(st == "forex"){
          cont <- twsCurrency(
            local = "",
            symbol = sym,
            exch = "",
            currency = "CAD"
          )
        } else if (st == "equity"){
          cont <- twsSTK(
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
        } else if (st == "future"){
          cont <- twsFUT(
            local = "",
            symbol = sym,
            right = "",    
            expiry = "",
            exch = "ONE",
            primary = "",
            currency = cur
          )
        } else {
          self$TSLogError(self$ts_app_status, self$ts_trade_mode,  paste0("The security type ", ct, " you specified is not supported!"), "Error", "TSGetContractDetails")
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
        self$TSLogError(self$ts_app_status, self$ts_trade_mode,  paste0("Connection to IB is not active."), "Error", "TSGetContractDetails")
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
          self$TSLogError(self$ts_app_status, self$ts_trade_mode,  paste0("User entered watchlist has unexpected structure!"), "Error", "TSSetWatchlist")
        }
      } else {
        self$TSLogError(self$ts_app_status, self$ts_trade_mode,  paste0("Connection to IB is not active."), "Error", "TSSetWatchlist")
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
          self$TSLogError(self$ts_app_status, self$ts_trade_mode,  paste0("User entered tradelist has unexpected structure!"), "Error", "TSSetPrelimTradeList")
        }
      } else {
        self$TSLogError(self$ts_app_status, self$ts_trade_mode,  paste0("Connection to IB is not active."), "Error", "TSSetPrelimTradeList")
      }
      invisible(self)
    },
    
    ##
    # Generate final trade list
    TSGenFnlTradeList = function(){
      
      if(isConnected(self$ts_conn)){
        # Field: "Symbol","Action","Quantity","OrderType","LimitPrice","Security Type","Currency"
        man_trd_df <- self$ts_prelimTradelist
        if(nrow(man_trd_df) != 0){
          man_trd_df$YahooTicker <- paste0(man_trd_df$Symbol,ifelse(man_trd_df$Currency=="CAD",".TO",""))
          
          man_trd_df$LastTradePrice <- rep(0,nrow(man_trd_df))
          man_trd_df$AdjustedLimitPrice <- man_trd_df$LimitPrice
          man_trd_df$LastTimeStamp <- rep(Sys.Date(),nrow(man_trd_df))
          man_trd_df$Commission <- rep(0,nrow(man_trd_df))
          man_trd_df$ValidTicker <- rep(TRUE,nrow(man_trd_df))
          
          for(i in 1:nrow(man_trd_df)){
            #
            # Update commission
            #
            if(man_trd_df[i,"Currency"] %in% self$ts_tradable_currency){
              base_comm <- self$ts_comm[[man_trd_df[i,"Security Type"]]][[man_trd_df[i,"Currency"]]]
              man_trd_df[i,"Commission"] <- max(1, base_comm*man_trd_df[i,"Quantity"], na.rm = TRUE)
            } else {
              self$TSLogError(self$ts_app_status, self$ts_trade_mode,  paste0("Invalid trade currency - ", man_trd_df[i,"Currency"]), "Warning", "TSGenFnlTradeList")
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
        self$TSLogError(self$ts_app_status, self$ts_trade_mode,  paste0("Connection to IB is not active."), "Error", "TSGenFnlTradeList")
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
        self$TSUpdateAccountDetail()
        
        if(identical(colnames(single_trade), self$def_fnltradelist_colnames)){
          
          #
          # Validate and execute a single trade
          #
          bad.sell.order <- 0
          bad.buy.order <- 0
          if(tolower(single_trade[,"Security Type"]) == "stk" | 
             tolower(single_trade[,"Security Type"]) == "opt" | 
             tolower(single_trade[,"Security Type"]) == "fut" ){
            
            #
            # Retrieve holding and cash balance
            #
            curr_holdings <- self$ts_port_holdings_nonforex %>% dplyr::select(-Exchange)
            tmp <- self$ts_cash_balance %>% dplyr::filter(Currency == single_trade$Currency)
            cash_balance <- tmp$Balance
            exch_rate <- tmp$`Exchange Rate`
            
            #
            # Validate a trade
            #
            if(tolower(single_trade$Action) == "sell"){
              sell_trade <- dplyr::inner_join(
                single_trade,
                curr_holdings,
                by = c("Symbol", "Right", "Expiry", "Strike", "Currency", "Security Type")
              )
              
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
              order <- single_trade %>%
                dplyr::mutate(trade_amount = Quantity*AdjustedLimitPrice)
              
              order.total.value <- sum(order$trade_amount) + sum(order$Commission)
              
              if(order.total.value > max(cash_balance,0)){
                bad.buy.order <- 1
              } else {
                bad.buy.order <- 0
              }
            } else {
              self$TSLogError(self$ts_app_status, self$ts_trade_mode,  paste0("Trade type ", single_trade$Action, " is not valid!"), "Error", "TSExecuteTrade")
            }
            
            #
            # Execute the trade
            #
            if(bad.buy.order == 0 & bad.sell.order == 0 & single_trade$ValidTicker == TRUE){
              t <- single_trade$Symbol
              ri <- single_trade$Right
              ex <- single_trade$Expiry
              stk <- single_trade$Strike
              pe <- single_trade$Exchange
              ac <- single_trade$Action
              q <- single_trade$Quantity
              o <- single_trade$OrderType
              sect <- single_trade$`Security Type`
              p <- single_trade$AdjustedLimitPrice
              c <- single_trade$Currency

              gtd <- format(Sys.Date(),"%Y%m%d 23:59:5900")
              
              ##
              # make contract for stock and option
              if(tolower(sect) == "stk"){
                cont <- IBrokers::twsSTK(
                  symbol = t,
                  exch = "SMART",
                  primary = pe,
                  currency = c
                )
              } else if (tolower(sect) == "opt") {
                cont <- IBrokers::twsOPT(
                  local = "",    # necessary for option 
                  symbol = t,
                  right = ri,    # both call and put
                  expiry = ex,
                  strike = stk,
                  exch = "SMART",
                  primary = "",
                  currency = c
                )
              } else if (tolower(sect) == "fut") {
                cont <- IBrokers::twsFUT(
                  symbol = t,
                  expiry = ex,
                  exch = "ONE",
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
                self$TSLogError(self$ts_app_status, self$ts_trade_mode,  paste0("Trade type ", o, " is not valid for trade #", trd_id,"!"), "Error", "TSExecuteTrade")
              }
              self$ts_trade_ids <- c(self$ts_trade_ids, trd_id)
              self$ts_trade_results <- c(self$ts_trade_results, "Successful")
            } else {  
              # order validation failed
              self$ts_trade_ids <- c(self$ts_trade_ids, -1)
              self$ts_trade_results <- c(self$ts_trade_results, "Fail")
              msg <- paste0("$",single_trade$Symbol,
                            ifelse(bad.buy.order == 1, ": Bad buy order!", ": Bad sell order!"))
              self$TSLogError(self$ts_app_status, self$ts_trade_mode,  msg, "Error", "TSExecuteTrade")
              self$ts_last_trade_message <- msg
              print(msg)
            }
          } else if(tolower(single_trade[,"Security Type"]) == "forex"){
            #
            # Validate a trade
            #
            tgt_curr <- single_trade[,"Symbol"]
            tgt_value <- single_trade[,"Quantity"]
            exch <- "IDEALPRO"
            quoted_curr <- single_trade[,"Currency"]
            
            if(tgt_curr %in% self$ts_tradable_currency & quoted_curr %in% self$ts_tradable_currency){
              if(tgt_curr == "CAD"){
                tmp <- self$ts_cash_balance %>% dplyr::filter(Currency == quoted_curr)
                cash_balance <- tmp$Balance
                exch_rate <- tmp$`Exchange Rate`
                
                side <- "SELL"
                trade_value <- round(tgt_value/exch_rate, 0)
                order_value <- 1 * trade_value
                if(cash_balance < trade_value){
                  bad.buy.order = 1
                }
                forex_cont <- twsCurrency(
                  symbol = quoted_curr,
                  exch = exch,
                  currency = tgt_curr
                )
              } else {
                tmp <- self$ts_cash_balance %>% dplyr::filter(Currency == tgt_curr)
                exch_rate <- tmp$`Exchange Rate`
                
                tmp2 <- self$ts_cash_balance %>% dplyr::filter(Currency == 'CAD')
                cash_balance <- tmp2$Balance
                
                side <- "BUY"
                trade_value <- tgt_value
                order_value <- exch_rate * trade_value
                if(cash_balance < order_value){
                  bad.buy.order = 1
                }
                forex_cont <- twsCurrency(
                  symbol = tgt_curr,
                  exch = exch,
                  currency = quoted_curr
                )
              } 
            } else {
              self$TSLogError(self$ts_app_status, self$ts_trade_mode,  paste0("The requested currency ", tgt_curr, " or ", quoted_curr, " is not allowed to traded."), "Error", "TSExecuteTrade")
            }
            #
            # Execute the trade
            #
            if(bad.buy.order == 0){
              gtd <- format(Sys.Date(),"%Y%m%d 23:59:5900")
              
              trd_id <- reqIds(self$ts_conn)
              trd_res <- placeOrder(self$ts_conn,
                                    forex_cont,
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
              self$TSLogError(self$ts_app_status, self$ts_trade_mode,  msg, "Error", "TSExecuteTrade")
              self$ts_last_trade_message <- msg
              print(msg)
            }
          } else {
            self$ts_trade_ids <- c(self$ts_trade_ids, -1)
            self$ts_trade_results <- c(self$ts_trade_results, "Fail")
            msg <- "Error currently only equity and currenct are allowed to be traded!"
            self$TSLogError(self$ts_app_status, self$ts_trade_mode,  msg, "Error", "TSExecuteTrade")
            self$ts_last_trade_message <- msg
            print(msg)
          }
          
        } else {
          self$ts_trade_ids <- c(self$ts_trade_ids, -1)
          self$ts_trade_results <- c(self$ts_trade_results, "Fail")
          msg <- "Error Trade list structure is invalid."
          self$TSLogError(self$ts_app_status, self$ts_trade_mode,  msg, "Error", "TSExecuteTrade")
          self$ts_last_trade_message <- msg
          print(msg)
        }
      } else {
        self$ts_trade_ids <- c(self$ts_trade_ids, -1)
        self$ts_trade_results <- c(self$ts_trade_results, "Fail")
        msg <- "Connection to IB is not active."
        self$TSLogError(self$ts_app_status, self$ts_trade_mode,  msg, "Error", "TSExecuteTrade")
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
          self$TSLogError(self$ts_app_status, self$ts_trade_mode,  paste0("Error trade list is empty!"), "Error", "TSExecuteAllTrades")
        }
      } else {
        self$TSLogError(self$ts_app_status, self$ts_trade_mode,  paste0("Connection to IB is not active."), "Error", "TSExecuteAllTrades")
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
        self$TSLogError(self$ts_app_status, self$ts_trade_mode,  paste0("Connection to IB is not active."), "Error", "TSCancelTrade")
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
        self$TSLogError(self$ts_app_status, self$ts_trade_mode,  paste0("Connection to IB is not active."), "Error", "TSCancelAllTrades")
      }
      invisible(self)
    },
    
    ##
    # Set transmit
    TSSetTransmit = function(flg){
      if(isConnected(self$ts_conn)){
        self$ts_trade_transmit_switch <- flg
      } else {
        self$TSLogError(self$ts_app_status, self$ts_trade_mode,  paste0("Connection to IB is not active."), "Error", "TSSetTransmit")
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
          self$TSLogError(self$ts_app_status, self$ts_trade_mode,  paste0("Invalid date ", dt, " to be set."), "Error", "TSSetGoodUntil")
        }
      } else {
        self$TSLogError(self$ts_app_status, self$ts_trade_mode,  paste0("Connection to IB is not active."), "Error", "TSSetGoodUntil")
      }
      invisible(self)
    },
    
    ##
    # generate trade result
    TSGenTradeResults = function(){
      if(isConnected(self$ts_conn)){
        temp_df <- self$ts_fnlTradelist[,c("Symbol","Currency")]
        exp_trd_cnt <- nrow(temp_df)
        act_trd_cnt <- length(self$ts_trade_ids)
        temp_df$TradeID <- self$ts_trade_ids[(act_trd_cnt-exp_trd_cnt+1):act_trd_cnt]
        temp_df$TradeResult <- self$ts_trade_results[(act_trd_cnt-exp_trd_cnt+1):act_trd_cnt]
        self$ts_trade_summary <- temp_df
      } else {
        self$TSLogError(self$ts_app_status, self$ts_trade_mode,  paste0("Connection to IB is not active."), "Error", "TSGenTradeResults")
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
      self$TSLogError(self$ts_app_status, self$ts_trade_mode,  res.msg, "Info", "TSCloseTradingSession")
    }
    
    ##
    # future functions starts here
  )
)

