pacman::p_load(tidyverse,lubridate,rio,
               echarts4r,patchwork,ggsci,furrr,bslib,shiny,
               janitor,ggrepel,ggforce,plotly,ggthemes,ggsci,tictoc,DT)

scenario_df_all<-import('scenario_df_all.rds')
nat<-import('nature_rsv_month_period_173052.rds')

source('result-viz.R')
source('strategy.R',local = T)

ui <- page_navbar(
  title='Result',
  sidebar = sidebar(
    h5('Scanario Left'),
    verbatimTextOutput('s1'),
    h5('Scanario Right'),
    verbatimTextOutput('s2')
  ),
  nav_panel(title='Table',DT::dataTableOutput('table')),
  nav_panel(title='Indicator',
            navs_tab_card(full_screen=T,
                          nav_panel('CasesN',echarts4rOutput('uni_casesn', width='100%')),
                          nav_panel('CasesA',echarts4rOutput('uni_casesa', width='100%')),
                          nav_panel('Doses',echarts4rOutput('uni_doses', width='100%')),
                          nav_panel('NNT',echarts4rOutput('uni_nnt', width='100%')))),
  nav_panel(
    title='Comparison',
    navs_tab_card(full_screen = T,
      nav_panel('TOP-NNT',layout_column_wrap(
        width=1/2,
        echarts4rOutput('top_nnt_e1',width = '100%'),
        echarts4rOutput('top_nnt_e2',width = '100%')
      )),
      nav_panel('Top-NNT',layout_column_wrap(
        width=1/2,
        plotOutput('top_nnt_1'),
        plotOutput('top_nnt_2')
      )),
      nav_panel('TOP-AVT',layout_column_wrap(
        width=1/2,
        echarts4rOutput('top_avt_e1',width = '100%'),
        echarts4rOutput('top_avt_e2',width = '100%')
      )),
      nav_panel('Top-AVT',layout_column_wrap(
        width=1/2,
        plotOutput('top_avt_1'),
        plotOutput('top_avt_2')
      )),
      nav_panel('NNT-zoom',layout_column_wrap(
        width=1/2,
        plotOutput('nnt_1'),
        plotOutput('nnt_2')
      )),
      nav_panel('NNT-plotly',layout_column_wrap(
        width=1/2,
        plotlyOutput('nnt_p1'),
        plotlyOutput('nnt_p2')
      )),
      nav_panel('NNT-facet',layout_column_wrap(
        width=1/2,
        plotOutput('nnt_f1'),
        plotOutput('nnt_f2')
      ))
    )
  ),
  nav_panel(title='Candidate Strategy',
            layout_sidebar(
              sidebar=sidebar(
                selectInput('s','Strategy',choices=sList),
                sliderInput('m','Protection Month',value=5,min=1,max=20)
              ),
              echarts4rOutput('plot_int',height = '600px')
            )
  )
)

server <- function(input, output, session) {
  output$table<-renderDataTable(
    scenario_df_all %>% select(-c(res_df,sList)),
    options=list(columnDefs = list(list(targets = 1:3, searchable = T)))
  )
  s<-reactive({
    if(length(input$table_rows_selected>1)){
      return(input$table_rows_selected[1:2])
    } else {
      return(c(1,2))
    }
    
  })
  res_df<-reactive({
    s() %>%
      map(~scenario_df_all[[.x,'res_df']][[1]])
  })
  output$s1<-renderText({
    paste0(
      'Row ',s()[1],': \n',
      print_parmas(scenario_df_all[s()[1],])
    )
  })
  output$s2<-renderText({
    paste0(
      'Row ',s()[2],': \n',
    print_parmas(scenario_df_all[s()[2],])
    )
  })
  sum_res<-reactive({
    res_df() %>%
      map(~summarise_res(.x))
  })
  # top nnt
  output$top_nnt_e1<-renderEcharts4r(
    sum_res()[[1]] %>%
      top_echart('NNT') %>%
      e_grid(left='80px')
  )
  output$top_nnt_e2<-renderEcharts4r(
    sum_res()[[2]] %>%
      top_echart('NNT') %>%
      e_grid(left='80px')
  )
  output$top_nnt_1<-renderPlot(
    sum_res()[[1]] %>%
      top_ggplot('NNT')
  )
  output$top_nnt_2<-renderPlot(
    sum_res()[[2]] %>%
      top_ggplot('NNT')
  )
  # top avt
  output$top_avt_e1<-renderEcharts4r(
    sum_res()[[1]] %>%
      top_echart('avt') %>%
      e_grid(left='80px')
  )
  output$top_avt_e2<-renderEcharts4r(
    sum_res()[[2]] %>%
      top_echart('avt') %>%
      e_grid(left='80px')
  )
  output$top_avt_1<-renderPlot(
    sum_res()[[1]] %>%
      top_ggplot('avt')
  )
  output$top_avt_2<-renderPlot(
    sum_res()[[2]] %>%
      top_ggplot('avt')
  )
  # nnt facet
  output$nnt_f1<-renderPlot(
    plot_facet(sum_res()[[1]])
  )
  output$nnt_f2<-renderPlot(
    plot_facet(sum_res()[[2]])
  )
  # nnt zoom
  output$nnt_1<-renderPlot(
    plot_nnt(sum_res()[[1]])$p_zoom
  )
  output$nnt_2<-renderPlot(
    plot_nnt(sum_res()[[2]])$p_zoom
  )
  # nnt plotly
  output$nnt_p1<-renderPlotly(
    plot_nnt(sum_res()[[1]])$p %>%
      ggplotly()
  )
  output$nnt_p2<-renderPlotly(
    plot_nnt(sum_res()[[2]])$p %>%
      ggplotly()
  )
  # uni scenario
  output$uni_casesn<-renderEcharts4r(
    plot_indicator(sum_res()[[1]],CasesN)
  )
  output$uni_casesa<-renderEcharts4r(
    plot_indicator(sum_res()[[1]],CasesA)
  )
  output$uni_doses<-renderEcharts4r(
    plot_indicator(sum_res()[[1]],Doses)
  )
  output$uni_nnt<-renderEcharts4r(
    plot_indicator(sum_res()[[1]],NNT)
  )
  # strategy
  output$plot_int<-renderEcharts4r(
    plot_int(input$s,input$m)
  )
}

shinyApp(ui, server)
