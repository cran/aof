## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library("aof")
library("bcpa")

## ----cc010--------------------------------------------------------------------
# mu1 and mu2: behavioural values at stage 1 and stage 2. 
# Both values mu1 and mu2 are equal (e.g. mu1=mu2=50) if no behavioural change is 
# simulated, or different (e.g. mu1=25 and mu2=50) if behavioural change is 
# simulated.
# rho1 and rho2 : interval frequency (default value 0.5 for both stages) 
# n.obs: no. observations randomly selected in the time series, from 5 to 45
# sigma1 and sigma2: variance around the behavioural value, from 0.1 to 3
# t.full: time series from 0 to 50
# n.obs: no. observations randomly selected in the time series, from 5 to 45
# t.break: the time of the simulated behavioural change (default value of 25)

getTimeBudget <- function(
  mu1 = 50,
  mu2 = 50,
  rho1 = 0.5,
  rho2 = 0.5,
  n.obs = 5, 
  sigma1 = 3,
  sigma2 = 3,
  t.full = 0:50,
  t.break = 25
){
  SimTS <- function(n, mu, rho, sigma){
    X.standard <- arima.sim(n, model = list(ar = rho))
    X.standard/sd(X.standard)*sigma + mu
  }
  x.full <- c(SimTS(t.break, mu1, rho1, sigma1),
    SimTS(max(t.full) - t.break + 1, mu2, rho2, sigma2))
  keep <- sort(sample(1:length(x.full), n.obs))
  TimeBudget <- data.frame(
    name = "A",
    Age = t.full[keep],
    x = x.full[keep])
  return(TimeBudget)
}
getAofPlot <- function(
  TimeBudget = TimeBudget, 
  AOF = AOF, 
  t.break = 25, 
  poly = FALSE,
  ylabX = "x"
){
  oldpar <- par(no.readonly =TRUE)
  par(mar = c(4, 4, 1, 1), mfrow = c(1, 1))
  plot(
    x = TimeBudget$Age,
    y = TimeBudget$x, 
    las = 1,
    xlab = "Age (day)", ylab = ylabX,
    pch = 21,
    col = "gray20", bg = "gray80")
  goodbreak1 = max(TimeBudget$Age[TimeBudget$Age < t.break])
  goodbreak2 = min(TimeBudget$Age[TimeBudget$Age >= t.break])
  if(poly == TRUE){
    polygon(
      c(
        goodbreak1 - 0.5, 
        goodbreak2 + 0.5, 
        goodbreak2 + 0.5, 
        goodbreak1 - 0.5),
      c(0, 0, 100, 100), 
      col = "gray50", border = NA)
  }
  abline(v = AOF$AOF, col = "black", lwd = 2, lty = 3)
  lines(
    x = c(min(TimeBudget$Age), AOF$AOF), 
    y = c(AOF$behav.stage1, AOF$behav.stage1), 
    col = "black", lwd = 2)
  lines(
    x = c(AOF$AOF, max(TimeBudget$Age)),
    y = c(AOF$behav.stage2, AOF$behav.stage2),
    col = "black", lwd = 2)
  par(oldpar)
  return(c(goodbreak1, goodbreak2))
}

## ----cc011--------------------------------------------------------------------
TimeBudget <- getTimeBudget(
  n.obs = 5, 
  sigma1 = 0.1,
  sigma2 = 0.1)
print(TimeBudget)
AOF <- aof(
  name = TimeBudget[,1], 
  Age = TimeBudget[,2], 
  x = TimeBudget[,3])
print(AOF)

## ----cc011b, fig.width=7, fig.height=5, warning=FALSE-------------------------
getAofPlot(TimeBudget, AOF)

## ----cc012--------------------------------------------------------------------
TimeBudget <- getTimeBudget(
  n.obs = 5, 
  sigma1 = 3,
  sigma2 = 3)
print(TimeBudget)
AOF <- aof(
  name = TimeBudget[,1], 
  Age = TimeBudget[,2], 
  x = TimeBudget[,3])
print(AOF)

## ----cc012b, fig.width=7, fig.height=5, warning=FALSE-------------------------
getAofPlot(TimeBudget, AOF)

## ----cc013--------------------------------------------------------------------
TimeBudget <- getTimeBudget(
  n.obs = 45, 
  sigma1 = 0.1,
  sigma2 = 0.1)
print(TimeBudget)
AOF <- aof(
  name = TimeBudget[,1], 
  Age = TimeBudget[,2], 
  x = TimeBudget[,3])
print(AOF)

## ----cc013b, fig.width=7, fig.height=5, warning=FALSE-------------------------
getAofPlot(TimeBudget, AOF)

## ----cc014--------------------------------------------------------------------
TimeBudget <- getTimeBudget(
  n.obs = 45, 
  sigma1 = 3,
  sigma2 = 3)
print(TimeBudget)
AOF <- aof(
  name = TimeBudget[,1], 
  Age = TimeBudget[,2], 
  x = TimeBudget[,3])
print(AOF)

## ----cc014b, fig.width=7, fig.height=5, warning=FALSE-------------------------
getAofPlot(TimeBudget, AOF)

## ----cc021--------------------------------------------------------------------
TimeBudget <- getTimeBudget(
  mu1 = 25,
  n.obs = 5, 
  sigma1 = 0.1,
  sigma2 = 0.1)
print(TimeBudget)
AOF <- aof(
  name = TimeBudget[,1], 
  Age = TimeBudget[,2], 
  x = TimeBudget[,3])
print(AOF)

## ----cc021b, fig.width=7, fig.height=5, warning=FALSE-------------------------
getAofPlot(TimeBudget, AOF)

## ----cc022--------------------------------------------------------------------
TimeBudget <- getTimeBudget(
  mu1 = 25,
  n.obs = 5, 
  sigma1 = 3,
  sigma2 = 3)
print(TimeBudget)
AOF <- aof(
  name = TimeBudget[,1], 
  Age = TimeBudget[,2], 
  x = TimeBudget[,3])
print(AOF)

## ----cc022b, fig.width=7, fig.height=5, warning=FALSE-------------------------
getAofPlot(TimeBudget, AOF)

## ----cc023--------------------------------------------------------------------
TimeBudget <- getTimeBudget(
  mu1 = 25,
  n.obs = 45, 
  sigma1 = 0.1,
  sigma2 = 0.1)
print(TimeBudget)
AOF <- aof(
  name = TimeBudget[,1], 
  Age = TimeBudget[,2], 
  x = TimeBudget[,3])
print(AOF)

## ----cc023b, fig.width=7, fig.height=5, warning=FALSE-------------------------
getAofPlot(TimeBudget, AOF)

## ----cc024--------------------------------------------------------------------
TimeBudget <- getTimeBudget(
  mu1 = 25,
  n.obs = 45, 
  sigma1 = 3,
  sigma2 = 3)
print(TimeBudget)
AOF <- aof(
  name = TimeBudget[,1], 
  Age = TimeBudget[,2], 
  x = TimeBudget[,3])
print(AOF)

## ----cc024b, fig.width=7, fig.height=5, warning=FALSE-------------------------
getAofPlot(TimeBudget, AOF)

## ----cc030, warning=FALSE-----------------------------------------------------
TimeBudget <- dataExample
head(TimeBudget, n = 25)

## ----cc031, warning=FALSE-----------------------------------------------------
# working on Number of trips
varX <- "Number"
AOF_number <- aof(
  name = TimeBudget[,1], 
  Age = TimeBudget[,2], 
  x = TimeBudget[varX])
print(AOF_number)

## ----cc031b, warning=FALSE----------------------------------------------------
# working on Duration of trips
varX <- "Duration"
AOF_duration <- aof(
  name = TimeBudget[,1], 
  Age = TimeBudget[,2], 
  x = TimeBudget[varX])
print(AOF_duration)

## ----cc031c, warning=FALSE----------------------------------------------------
# working on Time of trips
varX <- "Time"
AOF_time <- aof(
  name = TimeBudget[,1], 
  Age = TimeBudget[,2], 
  x = TimeBudget[varX])
print(AOF_time)

## ----cc032, warning=FALSE-----------------------------------------------------
goodbreak <- lapply(seq_along(unique(TimeBudget[,1])), function(i){
  print(as.character(unique(TimeBudget[,1])[i]))
  oldpar <- par(no.readonly =TRUE)
  par(mfrow = c(1, 3))
  varXList <- list("Number", "Duration", "Time")
  varAOFList <- list(AOF_number, AOF_duration, AOF_time)
  varYlabList <- list("Trip number (per day)", "Trip duration (seconds)", "Trip time (seconds)")
  varXRes <- sapply(1:3, function(j){
    TimeBx <- TimeBudget[
      TimeBudget[,1] == unique(TimeBudget[,1])[i], 
      c("name", "Age", varXList[[j]])]
    names(TimeBx)[3] <- "x"
    getAofPlot(
      TimeBudget = TimeBx, 
      AOF = varAOFList[[j]][i,], 
      ylabX = varYlabList[[j]])
  })
  par(oldpar)
  return(varXRes)
})

