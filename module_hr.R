library(shiny)
library(DT)
library(tidyverse)
library(bslib)
library(rio)
library(janitor)
library(echarts4r)

cat('---module_hr.R---\n')

db<-import('nature_rsv_month_period_173052.rds')

db.wide<-db %>%
  filter(month!='Total') %>%
  mutate(across(c(month,period),~as.numeric(.x))) %>%
  mutate(time=month+period,month=month.abb[month]) %>%
  pivot_wider(id_cols = month,names_from = time,values_from = est) %>%
  mutate(month=ordered(month,levels=month.abb)) %>%
  arrange(month) %>%
  setNames(c('BirthMonth',rep(month.abb,3))) %>%
  column_to_rownames(var = 'BirthMonth') %>%
  round(1) %>%
  clean_names()

colors<-colorRampPalette(c('#FFFFFF','#15607A'))

db.wide.norm<- db.wide %>%
  clean_names() %>%
  mutate(across(everything(),~{
    (.x - min(db.wide,na.rm=T))/(max(db.wide,na.rm=T)-min(db.wide,na.rm=T))
  })) %>%
  mutate(across(everything(),~replace_na(.x,0)))


brks <- quantile(db.wide, probs = seq(.05, .95, .05), na.rm = TRUE)
clrs<-tail(colors(25),20)

hrUI <- function(id) {
  ns <- NS(id)
  card(height = 650,
       full_screen = TRUE,
       card_header("Table 1. Monthly Hospitalization Rate of RSV-ALRI stratified by month of birth among children under 2 years old"),
       div(style="overflow-y:scroll",dataTableOutput(ns('hr_nature'))),
       card_footer("BM: Birth Month")
  )
}

hrServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$hr_nature<-renderDataTable(
        datatable(db.wide,width = '800px',height = '600px',colnames = c('BM',rep(month.abb,3)[1:35]),
                  selection = "none",
                  options = list(searching=FALSE,paging=FALSE,ordering=FALSE,select=F,
                                 dom = 't',scrollX=TRUE,autoWidth = TRUE,
                                 columnDefs = list(list(width = '5px', targets = 1:34))
                  )) %>% formatStyle(names(db.wide), 
                                     backgroundColor = styleInterval(brks, clrs),
                                     color = "#ffffff",
                                     fontFamily = "Arial",
                                     fontSize = "13px",
                                     lineHeight = "30%",
                                     verticalAlign = "middle")
      )
    }
  )
}
