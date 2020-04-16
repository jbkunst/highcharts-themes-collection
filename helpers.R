list_get_demos <- function(){
  
  # data(mpg, package = "ggplot2")
  
  p1 <- highcharts_demo() %>% 
    hc_tooltip(table = TRUE, sort = TRUE)
  
  # p2 <- hchart(mtcars, "scatter", hcaes(x = mpg, y = disp, group = cyl))
  
  library(forecast)
  p3 <- hchart(
    forecast(auto.arima(AirPassengers), level = 95, h = 12*3)) %>% 
    hc_tooltip(valueDecimals = 2)
  
  # p4 <- hchart(stl(log(AirPassengers), "per"))
  
  mpgman2 <- count(mpg, class, year)
  p5 <- hchart(mpgman2, "column", hcaes(x = class, y = n, group = year))
  
  
  library(quantmod)
  # p6 <- hchart(getSymbols("APPL", auto.assign = FALSE))
  x <- getSymbols("AAPL", auto.assign = FALSE, from = lubridate::ymd(20181001))
  y <- getSymbols("AMZN", auto.assign = FALSE, from = lubridate::ymd(20181001))
  
  p6 <- highchart(type = "stock") %>% 
    hc_add_series(x) %>% 
    hc_add_series(y, type = "ohlc")
  
  p7 <- highchart() %>% 
    hc_add_series(density(rnorm(100000)),
                  type = "area", name = "Normal Distribution") %>% 
    hc_add_series(density(rgamma(100000, 5, 0.8)),
                  type = "area", name = "Gamma(5. 0.8) Distribution") %>%
    hc_add_series(density(rgamma(100000, 3, 0.8)),
                  type = "area", name = "Gamma(3. 0.8) Distribution") %>% 
    hc_plotOptions(series = list(fillOpacity = 0.5)) %>% 
    hc_xAxis(min = -5, max = 12) %>% 
    hc_yAxis(showLastLabel = FALSE, showFirstLabel = FALSE, endOnTick = FALSE, startOnTick = FALSE) %>% 
    hc_xAxis(showLastLabel = FALSE, showFirstLabel = FALSE, endOnTick = FALSE, startOnTick = FALSE) %>% 
    hc_tooltip(valueDecimals = 3)
  
  data(GNI2014, package = "treemap")
  GNI2014 <- select(GNI2014, -population, -continent)
  color_stops_index <- color_stops(colors = viridis::inferno(10, begin = 0.1))
  p8 <- hcmap(
    "custom/world-robinson-lowres",
    data = GNI2014,
    name = "GNI",
    value = "GNI",
    joinBy = c("iso-a3", "iso3"),
    nullColor = "#932667"
  ) %>%
    hc_colorAxis(stops = color_stops_index, type = "logarithmic") %>%
    hc_legend(enabled = FALSE) %>%
    hc_mapNavigation(enabled = FALSE)  
  
  p <- list(p1, p3, p5, p7, p6, p8)
  
  p 
  
  
}






