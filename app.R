library(shiny)
library(tidyverse)
library(glue)
library(DT)
library(rio)
library(janitor)
library(plotly)
library(echarts4r)
source('map-leafleat.R')
source('strategy.R')
source('module_hr.R')
source('result-viz.R')

scenario_df_all_init<-import('scenario_df_all.rds')

theme<-bs_theme(fg = "rgb(20, 86, 131)", font_scale = NULL,
                bootswatch = "litera", bg = "rgb(255, 255, 255)")



ui <- page_navbar(id="pagenav",theme = theme,fillable = T,selected = 'default',window_title = 'Modeling the Optimal RSV Immunization strategy',
                  header=tagList(includeCSS("www/custom.css")),
                  title = strong(style="color:white;",HTML("Modeling the Optimal Monoclonal Antibody Administration <br/> Strategy for Respiratory Syncytial Virus(RSV) Prevention")),
                  nav_panel(title='Methods',
                            navset_pill_list(widths = c(2,10),fluid = F,
                                             nav_panel(title = "Study Site",
                                                       card(height = '600px',
                                                                       full_screen = TRUE,
                                                                       card_header('Study Site'),
                                                                       leafletOutput('map'),
                                                                       card_footer("SCH: Soochow University affilicated Childrens' Hospital"))
                                             ),
                                             nav_panel(title = 'Hospitalization Rate',hrUI('hr')),
                                             nav_panel(title='Model Structure',
                                                       card(height = '600px',
                                                            full_screen = TRUE,
                                                            card_header('Figure 1.Directed acyclic graph and model structure for strategy evaluatio'),
                                                            imageOutput('dag',width='80%',height = '600px'),
                                                            card_footer("RSV-ALRI: respiratory syncytial virus associated acute lower respiratory infection; mAb: monoclonal antibody"))
                                                       )
                            )
                  ),
                  nav_panel(title='Candidate Strategies',
                            layout_sidebar(
                              sidebar=sidebar(width = '25%',
                                              selectInput('s','Strategy',choices=sList),
                                              sliderInput('m','Protection Month',value=5,min=1,max=20),
                                              wellPanel(uiOutput('strategy_description'),style="font-size:18px")
                              ),
                              echarts4rOutput('plot_int',height = '600px')
                            )
                  ),
                  nav_panel(title='Simulate Intervention',value = 'default',
                                 layout_sidebar(
                                   sidebar = sidebar(
                                     numericInput('population','Population',value=12000000),
                                     sliderInput('br','Birth rate:',value=7,min=5,max=10,post='‰'),
                                     sliderInput('efficacy','Efficacy',value=c(60,60),min=10,max=100,post='%'),
                                     sliderInput('protection_month','Protection Duration (month)',value=5,min=1,max=20),
                                     sliderInput('coverage','Coverage Rate',value=20,min=10,max=100,post='%'),
                                     numericInput('seed','Random seed',value=1234,min=1),
                                     sliderInput('repetition','Repetition times',value=100,min=10,step=10,max=100),
                                     accordion(
                                       accordion_panel('Strategies',
                                         checkboxGroupInput('s_type','Quick Select',choices = c('S1','S2','S3','S4'),selected =c('S1','S2','S3','S4'),inline = T),
                                         checkboxGroupInput('s_selected','Strategies',choices=sList,inline = T,selected = sList)
                                       )
                                     ),
                                     actionButton('run','Run Model',class="btn-primary")
                                   ),
                                   navset_tab(nav_panel(title = "Result",full_screen = TRUE,
                                                        div(style="overflow-y:scroll",DT::dataTableOutput('tb_res_df'))),
                                              nav_panel(title = "Average",
                                                        div(style="overflow-y:scroll",DT::dataTableOutput('tb_sum_res'))),
                                              nav_panel(title = "Cases Raw",echarts4rOutput('et_casesn',height = '500px')),
                                              nav_panel(title = "Cases Averted",echarts4rOutput('et_casesa',height = '500px')),
                                              nav_panel(title = "Doses",echarts4rOutput('et_doses',height = '500px')),
                                              nav_panel(title = "NNT",echarts4rOutput('et_nnt',height = '500px')),
                                              nav_panel(title = "TOP-NNT",echarts4rOutput('et_top_nnt',height = '500px')),
                                              nav_panel(title = "TOP-NNT(ggplot)",plotOutput('pt_top_nnt',height = '500px')),
                                              nav_panel(title = "TOP-AVT",echarts4rOutput('et_top_avt',height = '500px')),
                                              nav_panel(title = "TOP-AVT(ggplot)",plotOutput('pt_top_avt',height = '500px')),
                                              nav_panel(title = "NNT-AVT(facet)",plotOutput('pt_facet',height = '500px')),
                                              nav_panel(title = "NNT-AVT(zoom)",plotOutput('pt_zoom',height = '500px')),
                                              nav_panel(title = "NNT-AVT(plotly)",plotlyOutput('pt_plotly',height = '500px')),
                                              nav_panel(title = "Superior Strategies",plotOutput('pt_superior',height = '500px'))
                                   )
                                 )),
                  nav_panel(title='Sensitive Analysis',
                            layout_sidebar(
                              sidebar = sidebar(
                                h5('Scanario Left'),
                                verbatimTextOutput('s1'),
                                h5('Scanario Right'),
                                verbatimTextOutput('s2')
                              ),
                              navset_tab(
                                nav_panel(title='Scenario',
                                          div(style="overflow-y:scroll",DT::dataTableOutput('scenario_table'))),
                                nav_panel('TOP-NNT',layout_column_wrap(
                                  width=1/2,height = '500px',
                                  echarts4rOutput('top_nnt_e1',width = 'auto'),
                                  echarts4rOutput('top_nnt_e2',width = 'auto')
                                )),
                                nav_panel('Top-NNT(ggplot)',layout_column_wrap(
                                  width=1/2,height = '500px',
                                  plotOutput('top_nnt_1'),
                                  plotOutput('top_nnt_2')
                                )),
                                nav_panel('TOP-AVT',layout_column_wrap(
                                  width=1/2,height = '500px',
                                  echarts4rOutput('top_avt_e1',width = '100%'),
                                  echarts4rOutput('top_avt_e2',width = '100%')
                                )),
                                nav_panel('Top-AVT(ggplot)',layout_column_wrap(
                                  width=1/2,height = '500px',
                                  plotOutput('top_avt_1'),
                                  plotOutput('top_avt_2')
                                )),
                                nav_panel('NNT-AVT(zoom)',layout_column_wrap(
                                  width=1/2,height = '500px',
                                  plotOutput('nnt_1'),
                                  plotOutput('nnt_2')
                                )),
                                nav_panel('NNT-AVT(plotly)',layout_column_wrap(
                                  width=1/2,height = '500px',
                                  plotlyOutput('nnt_p1'),
                                  plotlyOutput('nnt_p2')
                                )),
                                nav_panel('NNT-AVT(facet)',layout_column_wrap(
                                  width=1/2,height = '500px',
                                  plotOutput('nnt_f1'),
                                  plotOutput('nnt_f2')
                                )),
                                nav_panel('Superior Strategy',layout_column_wrap(
                                  width=1/2,height = '500px',
                                  plotOutput('superior_1'),
                                  plotOutput('superior_2')
                                ))
                              )
                            )),
                  nav_spacer(),
                  nav_panel(tags$a(shiny::icon("github"), "Github", href = "https://github.com/shalom-lab/rsv-shiny", target = "_blank"))
)
server <- function(input, output,session) {
  output$dag<-renderImage(
    list(src='www/fig-dag.png')
  )
  hrServer('hr')
  output$map<-renderLeaflet(base)
  params<-reactive({
    list(seed=round(input$seed),
         population=input$population,
         br=input$br/1000,
         efficacy_min=input$efficacy[1]/100,
         efficacy_max=input$efficacy[2]/100,
         coverage_rate=input$coverage/100,
         protection_month=input$protection_month)
  })
  scenario_df<-reactive({
    sList=input$s_selected
    tribble(
      ~seed,~repetition,~population,~br,~efficacy_min,~efficacy_max,~coverage_rate,~protection_month,
      input$seed,input$repetition,input$population,input$br/1000,
      input$efficacy[1]/100,input$efficacy[2]/100,input$coverage/100,input$protection_month
    ) %>%
      mutate(sList=map(seed,~sList)) %>%
      mutate(res_df=NA)
  })
  observeEvent(input$s_type,ignoreNULL = F,{
    if(is.null(input$s_type)){
      newList<-''
    } else {
      pattern<-paste0(input$s_type,collapse = '|')
      newList<-grep(pattern,sList,value=T)
    }
    updateCheckboxGroupInput(inputId = 's_selected',selected=newList)
  })
  scenario_df_all<-reactiveVal(
    scenario_df_all_init
  )
  res_df<-reactiveVal({
    scenario_df_all_init[[1,'res_df']][[1]]
  })
  sum_res<-reactive(
    summarise_res2(res_df())
  )
  # indicator
  output$et_casesn<-renderEcharts4r(
    plot_indicator(sum_res(),'CasesN')
  )
  output$et_casesa<-renderEcharts4r(
    plot_indicator(sum_res(),'CasesA')
  )
  output$et_doses<-renderEcharts4r(
    plot_indicator(sum_res(),'Doses')
  )
  output$et_nnt<-renderEcharts4r(
    plot_indicator(sum_res(),'NNT')
  )
  # TOP 20
  output$et_top_nnt<-renderEcharts4r(
    sum_res() %>% top_echart('NNT')
  )
  output$et_top_avt<-renderEcharts4r(
    sum_res() %>% top_echart('AVT')
  )
  output$pt_top_nnt<-renderPlot(
    sum_res() %>% top_ggplot('NNT')
  )
  output$pt_top_avt<-renderPlot(
    sum_res() %>% top_ggplot('AVT')
  )
  # NNT-AVT
  output$pt_facet<-renderPlot(
    sum_res() %>% plot_facet()
  )
  output$pt_zoom<-renderPlot(
    plot_nnt(sum_res())$p_zoom
  )
  output$pt_plotly<-renderPlotly(
    plot_nnt(sum_res())$p %>% ggplotly()
  )
  # Superior
  output$pt_superior<-renderPlot(
    plot_superior(sum_res())
  )
  output$tb_res_df<-DT::renderDataTable(
    datatable(res_df() %>% dplyr::mutate(across(where(is.numeric),~round(.x))),width = '800px',height = '600px',
              extensions = 'Buttons',
              rownames = F,
              options = list(dom = 'Blfrtip',
                             autoWidth = TRUE,
                             buttons = c('copy', 'csv', 'excel'),
                             lengthMenu = list(c(10,25,50,-1),
                                               c(10,25,50,"All"))))
  )
  output$tb_sum_res<-DT::renderDataTable(
    datatable(sum_res() %>% 
                select(strategy,CasesN,Doses,CasesA_mean,CasesA_sd,NNT_mean,NNT_sd,ratio_casesa,ratio_nnt) %>%
                mutate(across(2:7,~round(.x))) %>%
                mutate(Ratio_of_CasesA_relative_to_S1=round(ratio_casesa,3),
                       Ratio_of_NNT_relative_to_S1=round(ratio_nnt,3)) %>%
                select(-ratio_casesa,-ratio_nnt),
              extensions = 'Buttons',
              rownames = F,
              options = list(dom = 'Blfrtip',
                             autoWidth = TRUE,
                             buttons = c('copy', 'csv', 'excel'),
                             lengthMenu = list(c(10,25,50,-1),
                                               c(10,25,50,"All"))))
  )
  observeEvent(input$run,{
    if(length(input$s_selected)==0){
      showNotification("Please select at least one candidate strategy",type="warning")
    }
    req(length(input$s_selected)>0)
    anti_scenario<-anti_join(scenario_df() %>% select(-c(res_df,sList)),
                            scenario_df_all() %>% select(-c(res_df,sList)))
    if(nrow(anti_scenario)==0){
      showNotification("The same scenario has been run before, and the result is returned directly.",type="message")
      old_res<-inner_join(scenario_df_all(),scenario_df() %>%
                            select(-c(res_df,sList)))[[1,'res_df']][[1]] %>%
        filter(strategy %in% input$s_selected)
      res_df(old_res)
    } else {
      withProgress(message = 'Calculation in progress',
                   detail = 'This may take a while...', value = 0, {
                     incProgress(1/4)
                     new_scenario<-scenario_df()
                     print(scenario_df()$sList)
                     incProgress(1/4)
                     new_scenario_res<-model_for(new_scenario)
                     res_df(new_scenario_res)
                     new_scenario[[1,'res_df']]<-list(new_scenario_res)
                     incProgress(1/4)
                     if(length(new_scenario$sList)==length(sList)){
                       scenario_df_all(bind_rows(scenario_df_all(),new_scenario))
                     }
                     incProgress(1/4)
                   })
    }
  })
  output$plot_int<-renderEcharts4r(
    plot_int(input$s,input$m)
  )
  output$strategy_description<-renderUI({
    df<-data.frame(s=input$s) %>%
      separate(s,into = c('cat','n1','n2','n3','n4')) %>%
      mutate(across(2:5,~as.numeric(.x)))

    if(df$cat[1]=='S1'){
      t<-'Year-round immunization at birth strategy(S1), administering monoclonal antibody at birth regardless of their birth month'
    } else if (df$cat[1]=='S2'){
      t<-glue_data(df,'Seasonal immunization at birth strategy(S2.{n1}.{n2}), administering monoclonal antibody at birth only to children born between {month.name[n1]} and {rep(month.name,2)[n1+n2-1]} at birth')[1]
    } else if(df$cat[1]=='S3') {
      t<-glue_data(df,'Immunization at season strategy(S3.{n1}.{n2}), administering monoclonal antibody to children age less or equal than {n2} months old in {month.name[n1]} every year')[1]
    } else {
      t<-glue_data(df,'Seasonal with annual catch-up strategy(S4.{n1}.{n2}.{n3}.{n4}),combination of S2.{n1}.{n2} and S3.{n3}.{n4}, administering monoclonal antibody at birth to children born between {month.name[n1]} and {rep(month.name,2)[n1+n2-1]}, and administering monoclonal antibody to children age less or equal than {n4} months old in {month.name[n3]} for children born outside the time window from {month.name[n1]} to {rep(month.name,2)[n1+n2-1]}.')
    }
    p(t)
  })
  output$scenario_table<-DT::renderDataTable(
    scenario_df_all() %>% select(-c(res_df,sList)),
    options=list(columnDefs = list(list(targets = 1:3, searchable = T)),
                 dom = 'Blfrtip',
                 autoWidth = TRUE,
                 buttons = c('copy', 'csv', 'excel'),
                 lengthMenu = list(c(10,25,50,-1),
                                   c(10,25,50,"All"))),
    selection = list(mode = 'multiple', selected = c(1,2), target="row")
  )
  s_vs<-reactive({
    if(length(input$scenario_table_rows_selected>1)){
      return(input$scenario_table_rows_selected[1:2])
    } else {
      return(c(1,2))
    }
  })
  res_df_vs <- reactive({
    s_vs() %>%
      map( ~ scenario_df_all()[[.x, 'res_df']][[1]])
  })
  sum_res_vs<-reactive({
    res_df_vs() %>%
      map(~summarise_res2(.x))
  })
  output$s1<-renderText({
    paste0(
      'Row ',s_vs()[1],'\n',
      print_parmas(scenario_df_all()[s_vs()[1],])
    )
  })
  output$s2<-renderText({
    paste0(
      'Row ',s_vs()[2],'\n',
      print_parmas(scenario_df_all()[s_vs()[2],])
    )
  })
  # top nnt
  v<-reactive({
    validate(
      need(length(input$scenario_table_rows_selected)>=2, 'Please select two rows in Scenario Panel at first.')
    )
  })
  output$top_nnt_e1<-renderEcharts4r({
    v()
    sum_res_vs()[[1]] %>%
      top_echart('NNT') %>%
      e_grid(left='80px')
  }
  )
  output$top_nnt_e2<-renderEcharts4r({
    v()
    sum_res_vs()[[2]] %>%
      top_echart('NNT') %>%
      e_grid(left='80px')
  }
  )
  output$top_nnt_1<-renderPlot({
    v()
    sum_res_vs()[[1]] %>%
      top_ggplot('NNT')
  }
  )
  output$top_nnt_2<-renderPlot({
    v()
    sum_res_vs()[[2]] %>%
      top_ggplot('NNT')
  }
  )
  # top avt
  output$top_avt_e1<-renderEcharts4r({
    v()
    sum_res_vs()[[1]] %>%
      top_echart('avt') %>%
      e_grid(left='80px')
  }
  )
  output$top_avt_e2<-renderEcharts4r({
    v()
    sum_res_vs()[[2]] %>%
      top_echart('avt') %>%
      e_grid(left='80px')
  }
  )
  output$top_avt_1<-renderPlot({
    v()
    sum_res_vs()[[1]] %>%
      top_ggplot('avt')+
      theme(text=element_text(size=18))
  }
  )
  output$top_avt_2<-renderPlot({
    v()
    sum_res_vs()[[2]] %>%
      top_ggplot('avt')+
      theme(text=element_text(size=18))
  }
  )
  # NNT-AVT facet
  output$nnt_f1<-renderPlot({
    v()
    plot_facet(sum_res_vs()[[1]])
  }
  )
  output$nnt_f2<-renderPlot({
    v()
    plot_facet(sum_res_vs()[[2]])
  }
  )
  # nnt zoom
  output$nnt_1<-renderPlot({
    v()
    plot_nnt(sum_res_vs()[[1]])$p_zoom
  }
  )
  output$nnt_2<-renderPlot({
    v()
    plot_nnt(sum_res_vs()[[2]])$p_zoom
  }
  )
  # nnt plotly
  output$nnt_p1<-renderPlotly({
    v()
    plot_nnt(sum_res_vs()[[1]])$p %>%
      ggplotly()
  }
  )
  output$nnt_p2<-renderPlotly({
    v()
    plot_nnt(sum_res_vs()[[2]])$p %>%
      ggplotly()
  }
  )
  # Dominate
  output$superior_1<-renderPlot({
    v()
    plot_superior(sum_res_vs()[[1]])
  }
  )
  output$superior_2<-renderPlot({
    v()
    plot_superior(sum_res_vs()[[2]])
  }
  )
}

shinyApp(ui, server)

