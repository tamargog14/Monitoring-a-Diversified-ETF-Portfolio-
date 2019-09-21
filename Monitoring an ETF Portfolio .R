library(quantmod, warn.conflicts = FALSE, quietly = TRUE)
library(PerformanceAnalytics, warn.conflicts = FALSE, quietly = TRUE)
library(knitr, warn.conflicts = FALSE, quietly = TRUE)

getData<-function(tickers,datasrc){
  for (i in 1:length(tickers)){
    cat(tickers[i],i,"\n")
    getSymbols(tickers[i],src=datasrc,
               auto.assign=getOption("getSymbols.auto.assign",TRUE),
               env=parent.frame())
  }
}


makeIndex<-function(x,inv,ret){
  # Takes an xts object x and returns an index starting at 100 and evolving as the log returns of x.
  # The inv flag tells whether or not to invert the series before calculating returns.
  # The ret flag tells whether or not we have been passed a series of returns already.
  init.val<-100
  dts<-index(x,0)
  if (inv==TRUE) data<-1/x else data<-x
  if (ret==TRUE){ # we have a series of returns...
    ret.series<-x
  } else {
    ret.series<-periodReturn(data,period="daily",subset=NULL,type="log")
    dts<-index(ret.series,0)
  }
  n<-length(ret.series)
  new.series<-ret.series
  new.series[1]<-init.val
  
  for (i in 2:n){
    new.series[i]<-(1+ret.series[i-1])*new.series[i-1]
  }
  names(new.series)<-c("index")
  return(new.series)
} # My custom index funtion for converting indices to 100 based at inception.


calcWeights<-function(prices,numshares,initial){
  ret<-NULL
  for (i in 1:length(numshares)){
    sh<-numshares[i]
    ret<-cbind(ret,sh*prices[,i]/initial)
  }
  return(ret)
}


getOHLC<-function(assets,OHLC){
  # Takes a list of assets and returns either the Open, High, Low, or Close depending
  # on the passed value of HLOC. Return value is of type xts/zoo.
  ret<-NULL
  for (i in 1:length(assets)){
    if (OHLC=="O" || OHLC=="Open"){
      ret<-cbind(ret,assets[[i]][,1])
    } else {
      if (OHLC=="H" || OHLC=="High"){
        ret<-cbind(ret,assets[[i]][,2])
      } else {
        if (OHLC=="L" || OHLC=="Low"){
          ret<-cbind(ret,assets[[i]][,3])
        } else {
          if (OHLC=="C" || OHLC=="Close"){
            ret<-cbind(ret,assets[[i]][,4])
          }
        }
      }
    }
  }
  return(ret)
}


# Set up the data source and the tickers for all the ETFs...
data.source = c("yahoo")
tickers.etf = c("VGSIX", "VUSTX", "VGTSX", "VFISX", "VTSMX", "VFITX", "VEIEX", "VIPSX")
tickers.human = c("Real Est", "USTSY Long", "For Eq", "USTSY Short", "All Eq", "USTSY Int.", "EM Eq", "Infl Sec.")
tickers.bench = c("SPY", "^GSPC")
tsy.2y <- c("DGS2")

# Get the data from yahoo and FRED...
suppressWarnings(getData(tickers.etf, data.source))
suppressWarnings(getData(tickers.bench, data.source))
suppressWarnings(getData(tsy.2y, datasrc = "FRED"))

etfs <- list(VGSIX, VUSTX, VGTSX, VFISX, VTSMX, VFITX, VEIEX, VIPSX)
first.date <- c("2012-10-17/")

# Get all the closing prices into a vector starting from the inception date...
etfs_close <- getOHLC(etfs, "C")
etfs_close <- etfs_close[first.date]

# Specify the initial number of shares that were purchased and the initial investment...
shares <- c(2347.447, 977.96, 867.798, 1167.414, 2086.846, 1080.06, 1409.844, 2615.382)
init.invest <- 250000

mtm_last <- sum(last(etfs_close) * shares)
mtm_last

port_value <- as.xts(apply(etfs_close, 1, FUN = function(x) sum(x * shares)))
plot.xts(port_value, las = 1)

bench.60 <- periodReturn(GSPC[first.date][, 4], period = "daily", subset = NULL, type = "log")
bench.40 <- periodReturn(VFITX[first.date][, 4], period = "daily", subset = NULL, type = "log")

bench_ret <- 0.6 * bench.60 + 0.4 * bench.40  # Return stream for the 60/40 benchmark portfolio...
port_ret <- periodReturn(port_value, period = "daily", subset = NULL, type = "log")  # Ret. stream for our portfolio...
port_index <- makeIndex(port_ret, inv = FALSE, ret = TRUE)  # Our portfolio indexed to 100 at inception...
bench_index <- makeIndex(bench_ret, inv = FALSE, ret = TRUE)  # Benchmark portfolio indexed to 100 at our inception date...

# Setting up some variables to mach the PerformanceAnalytics lexicon...
Ra <- port_ret
Rb <- bench_ret
dts <- index(Ra, 0)
Rb <- xts(Rb, dts)
Rf <- as.numeric(last(DGS2)/100/252)

chart.RelativePerformance(Ra, as.vector(Rb), main = "Relative Performace vs. Benchmark", xaxis = TRUE)

#how much portfolio outperforms benchmark
act.premium <- ActivePremium(Ra, Rb, scale = 252)
act.premium

# The initial weights of the portfolio:
weights_init <- (first(etfs_close) * shares)/init.invest
weights_init

# The weights of the portfolio now:
weights_last <- (last(etfs_close) * shares)/init.invest
weights_last

# Change in weights since inception:
weights_chg <- as.vector(weights_last) - as.vector(weights_init)
weights_chg

# Display the weights of each asset over time:
port_weights <- calcWeights(etfs_close, shares, init.invest)
par(mfrow = c(2, 2))
for (i in 1:length(tickers.etf)) {
  plot.xts(port_weights[, i], las = 1, type = "l", main = paste(tickers.human[i]))
}


#price evolution over time 
par(mfrow = c(2, 2))
for (i in 1:length(tickers.etf)) {
  plot.xts(makeIndex(etfs_close[, i], inv = FALSE, ret = FALSE), las = 1, 
           type = "l", main = paste(tickers.human[i]))
}



#generate returns
etfs.ret <- NULL  # A data frame that holds all of the etf return streams...
for (i in 1:length(tickers.etf)) {
  temp <- as.xts(periodReturn(etfs_close[, i], period = "daily", type = "log"))
  etfs.ret <- cbind(etfs.ret, temp)
}
names(etfs.ret) <- tickers.human
head(etfs.ret)


#relative vs benchmark 
par(mfrow = c(1, 1))
chart.RelativePerformance(etfs.ret, as.vector(Rb), main = "Relative Performace vs. Benchmark", 
                          colorset = c(1:8), xaxis = TRUE, ylog = FALSE, legend.loc = "bottomleft", 
                          cex.legend = 0.8)


#returns for each asset class with 20 day StdDev and most recent 1 day VaR (95%) observation indicated by a solid line:
par(mfrow = c(1, 1))
charts.BarVaR(etfs.ret[, 1:8], width = 20, gap = 0, methods = "StdDev", p = 0.95, 
              clean = "none", all = TRUE, show.clean = FALSE, show.horizontal = TRUE, 
              show.symmetric = FALSE, show.endvalue = TRUE, show.greenredbars = TRUE, 
              ylim = NA, colorset = 1:12, lty = 1, ypad = 0, legend.cex = 0.8)


#CAPM Parameters
table.CAPM(Ra, Rb, scale = 252, Rf = Rf, digits = 4)

#VaR estimates
chart.VaRSensitivity(Ra, methods = c("HistoricalVaR", "ModifiedVaR", "GaussianVaR"))

#drawdowns
table.Drawdowns(Ra, top = 5, digits = 4)








