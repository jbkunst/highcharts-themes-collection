#' ---
#' title: "highcharts themes collection"
#' author: "Joshua Kunst"
#' date: "`r format(Sys.time(), ' %Y/%m')`"
#' output:
#'   html_document:
#'     theme: journal
#' ---
#' <link rel="stylesheet" href="styles.css" media="screen">
  

#+echo=FALSE, warning=FALSE
library("highcharter")
library("jsonlite")
library("htmltools")

rm(list = ls())

data("citytemp")
dtemp <- citytemp

hc <-  highchart(height = 400) %>% 
  hc_title(text = "Monthly Average Temperature") %>% 
  hc_subtitle(text = "Source: WorldClimate.com") %>% 
  hc_yAxis(title = list(text = "Temperature")) %>% 
  hc_xAxis(categories = dtemp$month) %>% 
  hc_add_series(name = "Tokyo", data = dtemp$tokyo) %>% 
  hc_add_series(name = "London", data = dtemp$london) %>% 
  hc_add_series(name = "Berlin", data = dtemp$berlin) 

thms <- ls("package:highcharter", pattern = "hc_theme_")
thms <- setdiff(thms, "hc_theme_merge")
thms <- c("538", "db", "economist", "google", "chalk", "null")

fonts <- NULL


charts <- lapply(thms, function(thmname){
  
  thm <- get(paste0("hc_theme_", thmname))()
  
  attr(thm, "class") <- NULL
  
  writeLines(toJSON(as.list(thm), pretty = TRUE, auto_unbox = TRUE),
             paste0(thmname, ".js"))
  
  chart <- hc %>%
    hc_subtitle(text = paste("Theme", thmname)) %>% 
    hc_add_theme(get(paste0("hc_theme_", thmname))())
  
  tags$div(class = "col-md-6", chart)
  
})

htmltools::tagList(charts)
