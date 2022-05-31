#read packages
library(xts)
library(magrittr)
library(tseries)
library(forecast)
library(TSstudio)

#processing data
dataset <-
  read.csv("newly_confirmed_cases_daily.csv", header = T) %>% read.zoo() %>%
  as.xts() %>% ts(frequency = 7)

#plot data
plot(dataset, main = "Number of infected people")

#correlation
acf1 <- acf(dataset,
            type = "correlation",
            plot = TRUE,
            demean = TRUE)
acf1

acf2 <- acf(dataset,
            type = "covariance",
            plot = TRUE,
            demean = TRUE)
acf2

acf3 <- acf(dataset,
            type = "partial",
            plot = TRUE,
            demean = TRUE)
acf3


#Unit root
#H0=Unit root true
pp1 <- pp.test(dataset,
               alternative = c("stationary"),
               lshort = F)
pp1

pp2 <-  pp.test(dataset,
                alternative = c("explosive"),
                lshort = F)
pp2


#H0=Unit root true
adf1 <- adf.test(dataset, alternative = "stationary")
adf1

adf2 <- adf.test(dataset, alternative = "explosive")
adf2

#H0=Unit root Flase
kpss1 <- kpss.test(dataset, null = "Level", lshort = "F")
kpss1

kpss2 <- kpss.test(dataset, null = "T", lshort = "F")
kpss2

#seasonal
#ggseasonplot(dataset)
stl_ans <- stl(dataset[, 1], s.window = "per", robust = TRUE)
plot(stl_ans)

#arima
system.time(
  ans_arima <- auto.arima(
    y = dataset[, 1],
    d = NA,
    D = NA,
    max.p = 6,
    max.q = 6,
    max.P = 30,
    max.Q = 30,
    max.order = 30,
    max.d = 10,
    max.D = 10,
    stationary = FALSE,
    seasonal = TRUE,
    ic = c("aic"),
    stepwise = T,
    nmodels = 100000000,
    trace = T,
    approximation = F,
    method = NULL,
    truncate = NULL,
    xreg = NULL,
    test = c("adf"),
    test.args = list(),
    allowdrift = TRUE,
    allowmean = TRUE,
    lambda = NULL,
    biasadj = FALSE,
    parallel = F,
    num.cores = NULL
  )
)

plot(ans_arima)
ans_arima
summary(ans_arima)


#Residual autocorrelation
box <- Box.test(ans_arima$residuals, type = "Ljung-Box")
box

#Residual normality
jar <- jarque.bera.test(ans_arima$residuals)
jar

tsdiag(ans_arima)
