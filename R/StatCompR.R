#' @title A illustration dataset
#' @name data
#' @description A dataset used to illustrate the wine production in california from 1980-2020.
NULL

#' @title use SGD to estimate the x0 where f(x0) get the min value.
#' @description estimating regression parameter by SGD.
#' @param x a point in the axis x
#' @param alpha each-iter moving step.
#' @param N Total number of iterations.
#' @param coefficient coefficient of function.
#' @param powers power of every variable in the function.
#' @return the  x0 point .
#' @examples
#' \dontrun{
#'x<-stats::rnorm(1,0,100);N<-2000
#'alpha<-0.005;coefficient<-c(1,3);powers<-c(2,4)
#'x_last<-SGD_min(x,alpha,N,coefficient,powers)
#'paste("The x corresponding to the function value is",x_last)
#'}
#' @importFrom graphics curve
#' @importFrom  stats rnorm
#' @export
SGD_min <- function(x,alpha,N,coefficient,powers) {

  get_G <- function(x,coefficient,powers) {
    gradient<-0
    for (i in 1:length(coefficient)) {
      gradient<-gradient+coefficient[i]*powers[i]*x^(powers[i]-1)
    }
    print(gradient)
    return (gradient)
  }

  for (i in 1:N) {
    if (! is.na(x)){
    x<-x-alpha*get_G(x,coefficient,powers)
    }
    else{
      x<-stats::rnorm(1,0,100)
    }
  }
  print("The minimum function value is")
  y<-0
  
  for (i in 1:length(coefficient)) {
    y<-y+coefficient[i]*x^powers[i]
  }
  print(y)
  #plot the curve
  exp_par <- function(x) {
    y<-0
    len_par<-length(coefficient)
    for (i in 1:len_par) {
      y<-y+coefficient[i]*x^powers[i]
    }
    return (y)
  }
  graphics::curve(exp_par,-20,20)

  return(x)
  
}

#' @title Use timeSeries' data  to predict wine production in the future using R.
#' @description An arime model .
#' @param path the file path of data stored.
#' @param start the start of the timeseries,e.g 1999.
#' @param end the end of the timeseries,e.g 2022.
#' @param h Specify how long to forecast. 
#' @return Forecast results in the next few times.
#' @examples
#' \dontrun{
#' path<-"D:\\Code\\R_HW\\Californa_Wine_Production.xlsx"
#' start <- 1980
#' end <- 2020
#' h<-36
#' Arima_forecast(start,end,h,path)
#' }
#' @importFrom Rcpp evalCpp
#' @importFrom graphics par
#' @importFrom stats acf pacf plot.ts runif ts
#' @importFrom utils str
#' @importFrom  readxl read_excel
#' @importFrom  forecast auto.arima forecast
#' @export
Arima_forecast <- function(start,end,h,path=0) {
  library(StatComp22032)
  #Implementing time series forecasting using Auto ARIMA method
  # Dataset contains Californa_Wine_Production from 1980 to 2020 
  # library(readxl)
  if (path) {
    data<-readxl::read_excel(path)
  }
  else{
    data(data)
  }
  
  data<-data['Production']
  utils::str(data)
  #We do not divide into test and train data.Hence not computing Error  metrics
  #This code just shows the implementation of Auto arima in R
  #Converting data to time series 
  # library(stats)
  data_ts=stats::ts(data, frequency = 1, start = start,end = end)
  data_ts
  
  #Plotting time series
  graphics::par(mfrow=c(1,1))
  plot(data_ts)
  
  #ACF and PACF of data
  graphics::par(mfrow=c(1,3))
  stats::plot.ts(data_ts)
  stats::acf(data_ts, lag.max=10)
  stats::pacf(data_ts, lag.max=10)
  
  # Build time series model using auto.arima
  # library(forecast)
  
  model_autoarima= forecast::auto.arima(data_ts,ic='aic')
  model_autoarima
  
  #Forecasting using auto arima model
  data_forecasts=forecast::forecast(model_autoarima,h=h)
  par(mfrow=c(1,1))
  plot(data_forecasts)
  data_forecasts
}
