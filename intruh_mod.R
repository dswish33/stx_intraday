library(quantmod)
library(dplyr)
library(stringr)
library("leaps")
library(boot)
#library(zoo)



key = c("G8Y1XWFZLF17O3E8")

get_ohlc <- function(symbol){

  daf <- getSymbols(Symbols = symbol, src = "av", api.key = key, auto.assign = FALSE, periodicity = "intraday", interval = "30min", output.size = "full")
  colnames(daf) <- c("open", "high", "low", "close", "volume")
  ohlc_df <- as.data.frame(daf)
  
  return(ohlc_df)
}


get_mod <- function(symbol){
  
  a <- get_ohlc(symbol)
  a <- cbind(timeseries = rownames(a), a)
  rownames(a) <- 1:nrow(a)
  
  #y_t
  end_of_day_opens = a[grepl("15:30:00", a$timeseries, fixed = TRUE), "open"]
  end_of_day_closes = a[grepl("15:30:00", a$timeseries, fixed = TRUE), "close"]
  y_t = (end_of_day_closes - end_of_day_opens)/end_of_day_opens
  #don't need first day
  y_t = y_t[-(1)]
  
  #rod
  rest_of_day_closes = a[grepl("15:00:00", a$timeseries, fixed = TRUE), "close"]
  rest_of_day_opens = a[grepl("9:30:00", a$timeseries, fixed = TRUE) & !grepl("19:30:00", a$timeseries, fixed = TRUE), "open"]
  rest_of_day_returns = (rest_of_day_closes - rest_of_day_opens)/rest_of_day_opens
  #don't need first day
  rest_of_day_returns = rest_of_day_returns[-(1)]
  
  #post_mark
  post_mark_A = a[grepl("16:00:00", a$timeseries, fixed = TRUE), "open"]
  post_mark_B = a[grepl("19:30:00", a$timeseries, fixed = TRUE), "close"]
  post_mark_returns = (post_mark_B - post_mark_A)/post_mark_A
  #don't need last day
  post_mark_returns = post_mark_returns[-(length(post_mark_returns))]
  
  #pre_mark
  pre_mark_A = a[grepl("19:30:00", a$timeseries, fixed = TRUE), "close"]
  pre_mark_A = pre_mark_A[-(length(pre_mark_A))] #gets rid of last item
  pre_mark_B = a[grepl("9:00:00", a$timeseries, fixed = TRUE) & !grepl("19:00:00", a$timeseries, fixed = TRUE), "close"]
  pre_mark_B = pre_mark_B[-(1)] #gets rid of first item, this aligns the two arrays
  pre_mark_returns = (pre_mark_B - pre_mark_A)/pre_mark_A
  
  #open_volu
  volume_at_open = a[grepl("9:30:00", a$timeseries, fixed = TRUE) & !grepl("19:30:00", a$timeseries, fixed = TRUE), "volume"]
  #don't need first day
  volume_at_open = volume_at_open[-(1)]
  
  pre_mark_cands_volu = a[str_detect(a$timeseries, "4:00:00|4:30:00|5:00:00|5:30:00|6:00:00|6:30:00|7:00:00|7:30:00|8:00:00|8:30:00|9:00:00") & !str_detect(a$timeseries, "14:00:00|14:30:00|15:00:00|15:30:00|16:00:00|16:30:00|17:00:00|17:30:00|18:00:00|18:30:00|19:00:00"), "volume"]
  day_cands_volu = a[str_detect(a$timeseries, "10:00:00|10:30:00|11:00:00|11:30:00|12:00:00|12:30:00|13:00:00|13:30:00|14:00:00|14:30:00|15:00:00"), "volume"]
  cands_to_sum = 11
  
  #pre_mark_volu --> from 4:00 open to 9:00 close
  #don't need first day
  pre_mark_volu <- sapply(seq(1, length(pre_mark_cands_volu), by = cands_to_sum), function(i) sum(pre_mark_cands_volu[i:min(i + cands_to_sum - 1, length(pre_mark_cands_volu))]))
  pre_mark_volu = pre_mark_volu[-(1)]
  
  #volu_for_day --> from 10:00 open to 15:00 close
  #don't need first day
  day_volu <- sapply(seq(1, length(day_cands_volu), by = cands_to_sum), function(i) sum(day_cands_volu[i:min(i + cands_to_sum - 1, length(day_cands_volu))]))
  day_volu = day_volu[-(1)]
  
  
  #scaled preds
  #prev day post market returns
  pomr_s = c(scale(post_mark_returns))
  
  #pre market returns
  prmr_s = c(scale(pre_mark_returns))
  
  #return for rest of day
  rodr_s = c(scale(rest_of_day_returns))
  
  #pre market volume
  pmv_s = c(scale(pre_mark_volu))
  
  #volume at open
  vao_s = c(scale(volume_at_open))
  
  #volume for day
  dv_s = c(scale(day_volu))
  
  
  design_matrix <- data.frame(Y = y_t, POMR = pomr_s, PRMR = prmr_s, RODR = rodr_s, PMV = pmv_s, VAO = vao_s, DV = dv_s)
  
  best_subset <- regsubsets(y_t ~ pomr_s + prmr_s + rodr_s + pmv_s + vao_s + dv_s, data = design_matrix)
  
  return(best_subset)
  
}

qqq <- get_mod("QQQ")
qqq_sum <- summary(qqq)
#plot(qqq_sum$adjr2 , xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")

nvda <- get_mod("NVDA")
nvda_sum <- summary(nvda)

tsla <- get_mod("TSLA")
tsla_sum <- summary(tsla)

coin <- get_mod("COIN")
coin_sum <- summary(coin)

slv <- get_mod("SLV")
slv_sum <- summary(slv)

aapl <- get_mod("AAPL")
aapl_sum <- summary(aapl)

msft <- get_mod("MSFT")
msft_sum <- summary(msft)

avgo <- get_mod("AVGO")
avgo_sum <- summary(avgo)
