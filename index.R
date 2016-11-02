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
library(stringr)
library(jsonlite)
library(htmltools)
source("helpers.R")

p <- list_get_demos()

themes <- help.search("theme", package = "highcharter") %>%
  .$matches %>%
  tbl_df() %>%
  janitor::clean_names() %>% 
  select(name, title) %>% 
  distinct() %>% 
  filter(str_detect(name, "hc_theme_*")) %>% 
  filter(!name %in% c("hc_theme_merge", "hc_theme",
                      "hc_theme_darkunica", "hc_theme_sparkline",
                     "hc_theme_gridlight", "hc_theme_sandsignika",
                     "hc_theme_chalk", "hc_theme_handdrawn")) %>% 
  mutate(title = str_replace(title, " theme for highcharts", "")) %>% 
  arrange(name)

thms <- unique(c("hc_theme_smpl", "hc_theme_db", themes$name))

charts <- map(thms, function(thmname){
  
  # thmname <- sample(thms, size = 1)
  message(thmname)
  thmname_short <- str_replace(thmname, "hc_theme_", "")
  
  thm <- get(thmname)()
  attr(thm, "class") <- NULL
  writeLines(toJSON(as.list(thm), pretty = TRUE, auto_unbox = TRUE),
             paste0("themes/", thmname_short,".js"))
  
  thm <- get(thmname)()
  
  p <- map(p, hc_add_theme, thm)
  p <- map(p, hc_size, height = 400, width = "100%")
  
  if(thmname == "hc_theme_tufte") {
    p[[1]] <- p[[1]] %>% hc_add_theme(hc_theme_tufte2())
    p[[3]] <- p[[3]] %>% hc_add_theme(hc_theme_tufte2())
    
    thm <- hc_theme_tufte2()
    attr(thm, "class") <- NULL
    writeLines(toJSON(as.list(thm), pretty = TRUE, auto_unbox = TRUE),
               paste0("themes/", "tufte2",".js"))
  }
  
  link <- sprintf("https://raw.githubusercontent.com/jbkunst/highcharts-themes-collection/gh-pages/themes/%s.js",
                  thmname_short)
  
  tags$div(
    tags$h1(themes$title[themes$name == thmname]),
    p(tags$a(href = link, download = thmname_short, "download")),
    hw_grid(p, ncol = 2),
    p()
    )
  
})

#+ echo=FALSE, warning=FALSE
tagList(charts)
