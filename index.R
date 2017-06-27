#' ---
#' title: false
#' author: false # "Joshua Kunst"
#' date: false # "`r format(Sys.time(), ' %Y/%m')`"
#' output:
#'   html_document:
#'     theme: paper
#'     css: styles.css
#'     toc: false
#'     toc_float: true
#' ---
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

get_charts_w_theme <- function(thmname) { # thmname <- sample(thms, size = 1)
    
  thmname_short <- str_replace(thmname, "hc_theme_", "")
  
  thm <- get(thmname)()
  attr(thm, "class") <- NULL
  writeLines(toJSON(as.list(thm), pretty = TRUE, auto_unbox = TRUE),
             paste0("themes/", thmname_short,".js"))
  
  thm <- get(thmname)()
  
  p <- map(p, hc_add_theme, thm)
  p <- map(p, hc_size, height = 325, width = "100%")
  
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
  
  
  p %>% 
    map(tags$div, class = "col-md-6") %>%
    tags$div(., br(), tags$small(tags$a(href = link, download = thmname_short, "download")), class = "fluid-row")
    
}

#+ echo=FALSE, warning=FALSE
thms %>% 
  map(function(t){ # t <- sample(thms, 1)
    
    t %>% 
      str_replace_all("hc_theme_", "") %>% 
      tags$a(`data-target` = paste0("#", t), `data-toggle` = "tab") %>% 
      tags$li(class = if(t == thms[1]) "active" else NULL)
    # "<li><a data-target="#profile" data-toggle="tab">Profile</a></li>"
    
  }) %>% 
  tags$ul(class = "nav nav-pills", id = "myTab", .)

tags$br()

thms %>% 
  map(function(t){ # t <- sample(thms, 1)
    
    content <- get_charts_w_theme(t)
    
    tags$div(content, id = t, class = "tab-pane", class = if(t == thms[1]) "active" else NULL)
    
  }) %>% 
  tags$div(class = "tab-content")
    
