#' ---
#' title: "Highcharts Themes Collection"
#' author: "Joshua Kunst"
#' date: "`r format(Sys.time(), ' %Y/%m')`"
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: true
#' ---
#' 
#' <link rel="stylesheet" href="styles.css" media="screen">
#' 
#' You can download them from [here](https://github.com/jbkunst/highcharts-themes-collection/tree/gh-pages/themes)
#'
#' 
#+ echo=FALSE, warning=FALSE, include=FALSE
rm(list = ls())
library(highcharter)
library(tidyverse)
library(jsonlite)
library(htmltools)
source("helpers.R")

p <- list_get_demos()

thms <- c("smpl", "538", "db", "economist", "ft",
          "elementary", "flat", "flatdark",
          "google", "tufte", "null")

charts <- lapply(thms, function(thmname){
  # thmname <- sample(thms, 1)
  # thmname <- "tufte"
  thm <- get(paste0("hc_theme_", thmname))()
  attr(thm, "class") <- NULL
  
  writeLines(toJSON(as.list(thm), pretty = TRUE, auto_unbox = TRUE),
             paste0("themes/", thmname,".js"))
  
  thm <- get(paste0("hc_theme_", thmname))()
  
  p <- map(p, hc_add_theme, thm)
  p <- map(p, hc_size, height = 350, width = "100%")
  
  if(thmname == "tufte") {
    p[[1]] <- p[[1]] %>% hc_add_theme(hc_theme_tufte2())
    p[[3]] <- p[[3]] %>% hc_add_theme(hc_theme_tufte2())
    
    thm <- hc_theme_tufte2()
    attr(thm, "class") <- NULL
    writeLines(toJSON(as.list(thm), pretty = TRUE, auto_unbox = TRUE),
               paste0("themes/", "tufte2",".js"))
  }
  
  tags$div(
    tags$h1(thmname),
    p(),
    hw_grid(p, ncol = 2),
    p()
    )
})

#+ echo=FALSE, warning=FALSE
tagList(charts)
