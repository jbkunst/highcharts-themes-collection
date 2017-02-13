list_get_demos <- function(){
  
  data(mpg, package = "ggplot2")
  
  p1 <- highcharts_demo()
  
  
  p2 <- hchart(mpg, "scatter", hcaes(x = displ, y = hwy, group = class))
  
  library(forecast)
  p3 <- hchart(
    forecast(auto.arima(AirPassengers), level = 95, h = 12*3),
    showInLegend = FALSE) %>% 
    hc_tooltip(valueDecimals = 2)
  
  
  p4 <- hchart(stl(log(AirPassengers), "per"))
  
  
  mpgman2 <- count(mpg, class, year)
  p5 <- hchart(mpgman2, "column", hcaes(x = class, y = n, group = year))
  
  
  library(quantmod)
  p6 <- hchart(getSymbols("YHOO", auto.assign = FALSE))
  
  
  p <- list(p1, p2, p5, p6)
  
  p
  
}






