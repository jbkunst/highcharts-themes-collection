#' ---
#' title: "Highcharts Themes Collection"
#' author: "Joshua Kunst"
#' date: "`r format(Sys.time(), ' %Y/%m')`"
#' output:
#'   rmdformats::html_clean
#' ---
#' <link rel="stylesheet" href="styles.css" media="screen">
  

#+echo=FALSE, warning=FALSE
library("highcharter")
library("jsonlite")
library("htmltools")
library("printr")

rm(list = ls())

data("citytemp")
dtemp <- citytemp

hc <-  highchart() %>% 
  hc_title(text = "Monthly Average Temperature") %>% 
  hc_subtitle(text = "Source: WorldClimate.com") %>% 
  hc_yAxis(title = list(text = "Temperature")) %>% 
  hc_xAxis(categories = dtemp$month) %>% 
  hc_add_series(name = "Tokyo", data = dtemp$tokyo) %>% 
  hc_add_series(name = "London", data = dtemp$london) %>% 
  hc_add_series(name = "Berlin", data = dtemp$berlin) 

thms <- ls("package:highcharter", pattern = "hc_theme_")
thms <- c("538", "db", "economist", "ft", "google",
          "smpl", "flat", "flatdark")

fonts <- NULL

charts <- lapply(thms, function(thmname){
  # thmname <- "ft"
  thm <- get(paste0("hc_theme_", thmname))()
  
  attr(thm, "class") <- NULL
  
  writeLines(toJSON(as.list(thm), pretty = TRUE, auto_unbox = TRUE),
             paste0("themes/", thmname,".js"))
  
  chart <- hc %>%
    hc_subtitle(text = paste("Theme", thmname)) %>% 
    hc_add_theme(get(paste0("hc_theme_", thmname))())
  
  tags$div(
    tags$h1(sprintf("Theme %s", thmname)),
    chart
    )
})

htmltools::tagList(charts)
