library(tidyverse)
library(lubridate)
library(rio)
library(echarts4r)
library(patchwork)
library(furrr)
library(janitor)
library(ggrepel)
library(ggforce)
library(plotly)
library(ggthemes)
library(ggsci)
library(rlang)
cat('---result-viz.R---\n')
#scenario_df_all<-import('rsv-shiny/scenario_df_all.rds')

#res_df<-scenario_df_all[[1,'res_df']][[1]]

# Result Summrise ----
# summarise
summarise_res <- function(res_df){
  res_mean<-res_df %>%
    summarise(across(c(CasesN, CasesA, Doses, NNT),  ~ round(mean(.x, na.rm =
                                                                    T))), .by = strategy) %>% 
    separate(
      strategy,
      remove = F,
      into = c('s', 'n1', 'n2','n3','n4'),
      sep = '\\.'
    ) %>%
    mutate(n1 = as.numeric(n1), n2 = as.numeric(n2)) %>%
    arrange(s, n1, n2) %>%
    mutate(shape = ifelse(s == 'S1', 23, 16),
           size = ifelse(s == 'S1',3,1))
  
  res_s1<-res_mean %>% filter(strategy=='S1')
  NNT_S1=res_s1$NNT
  CasesA_S1=res_s1$CasesA
  
  res_mean<-res_mean %>%
    mutate(ratio_casesa=CasesA/CasesA_S1,
           ratio_nnt=NNT/NNT_S1) %>%
    rowwise() %>%
    mutate(dominated=all(ratio_casesa<1,ratio_nnt>1),
           dominate=all(ratio_casesa>1,ratio_nnt<1)) %>%
    ungroup() %>%
    mutate(color=case_when(s == 'S1'~'S1',
                           dominated==TRUE~'Dominated by S1',
                           dominate==TRUE~'Dominate S1',
                           T~'4'),
           label=case_when(dominate==TRUE~strategy,
                           T~'-'))
  res_mean
}
summarise_res2 <- function(res_df){
  res_mean<-res_df %>%
    mutate() %>%
    summarise(across(c(CasesN, CasesA, Doses, NNT),
                     list(mean=~ mean(.x, na.rm =T),
                          sd=~ sd(.x, na.rm =T),
                          median=~median(.x,na.rm=T),
                          q1=~quantile(.x,0.25,na.rm=T),
                          q3=~quantile(.x,0.75,na.rm=T))), .by = strategy) %>%
    mutate(across(where(is.numeric),~round(.x))) %>%
    separate(
      strategy,
      remove = F,
      into = c('s', 'n1', 'n2','n3','n4'),
      sep = '\\.'
    ) %>%
    mutate(n1 = as.numeric(n1), n2 = as.numeric(n2)) %>%
    arrange(s, n1, n2) %>%
    mutate(shape = ifelse(s == 'S1', 23, 16),
           size = ifelse(s == 'S1',3,1))
  
  res_s1<-res_mean %>% filter(strategy=='S1')
  NNT_S1=res_s1$NNT_mean
  CasesA_S1=res_s1$CasesA_mean
  
  res_mean<-res_mean %>%
    mutate(ratio_casesa=CasesA_mean/CasesA_S1,
           ratio_nnt=NNT_mean/NNT_S1) %>%
    rowwise() %>%
    mutate(dominated=all(ratio_casesa<1,ratio_nnt>1),
           dominate=all(ratio_casesa>1,ratio_nnt<1)) %>%
    ungroup() %>%
    mutate(color=case_when(s == 'S1'~'S1',
                           dominated==TRUE~'Dominated by S1',
                           dominate==TRUE~'Dominate S1',
                           T~'4'),
           label=case_when(dominate==TRUE~strategy,
                           T~'-'))
  res_mean %>%
    mutate(CasesN=CasesN_mean,CasesN_lower=CasesN_mean-CasesN_sd,CasesN_upper=CasesN_mean+CasesN_sd,
           CasesA=CasesA_mean,CasesA_lower=CasesA_mean-CasesA_sd,CasesA_upper=CasesA_mean+CasesA_sd,
           Doses=Doses_mean,Doses_lower=Doses_mean-Doses_sd,Doses_upper=Doses_mean+Doses_sd,
           NNT=NNT_mean,NNT_lower=NNT_mean-NNT_sd,NNT_upper=NNT_mean+NNT_sd)
}

# Candidate strategies list ------------
s2<-paste0('S2.',rep(6:12,each=8),'.',rep(4:11,time=7))
s3<-paste0('S3.',rep(6:12,each=2),'.',rep(c(6,12),time=7))
s4<-paste0('S4.',rep(str_sub(s2,4),each=length(s3)),'.',rep(str_sub(s3,4),time=length(s2)))
sList<-c('S1',s2,s3,s4)

# Sum_res ----
# sum_res<-summarise_res(res_df)
#sum_res<-summarise_res2(res_df)

# Strategy-indicator-echarts -----

# res_df %>%
#   group_by(strategy) %>%
#   e_charts(strategy) %>%
#   e_boxplot(NNT) %>%
#   e_datazoom(type='inside') %>%
#   e_datazoom(type='slider') %>%
#   e_grid(bottom='100px')%>%
#   e_x_axis(axisLabel=list(interval=0,rotate=90))

plot_indicator<-function(sum_res,indicator){
  sum_res %>%
    mutate(indicator=!!data_sym(indicator),
           indicator_lower=!!data_sym(paste0(indicator,'_lower')),
           indicator_upper=!!data_sym(paste0(indicator,'_upper'))) %>%
    mutate(tt = paste0(
      strategy,"<br />",
      "Mean:", !!data_sym(indicator), "<br />",
      "Standard deviation(sd): ", !!data_sym(paste0(indicator,'_sd')), "<br />",
      "Mean-sd:", !!data_sym(paste0(indicator,'_lower')), "<br />",
      "Mean+sd: ",!!data_sym(paste0(indicator,'_upper'))
    )) %>%
    group_by(s) %>%
    e_charts(strategy) %>%
    e_bar(indicator,bind = tt) %>%
    e_error_bar(lower = indicator_lower,upper=indicator_upper) %>%
    e_datazoom(type='inside') %>%
    e_datazoom(type='slider') %>%
    e_grid(bottom='150px')%>%
    e_x_axis(type='category', scale=TRUE,axisLabel=list(rotate=0)) %>%
    e_y_axis(name=indicator) %>%
    e_tooltip(formatter = htmlwidgets::JS("
    function(params) {
      return params.name;
    }
  "))
}
#plot_indicator(sum_res,'NNT')

plotRes<-function(res.long,var='NNT',sShow=sList,title=''){
  res.long %>%
    filter(variable==var,strategy %in% sShow) %>%
    mutate(strategy=factor(strategy,levels=unique(strategy))) %>%
    ggplot(aes(strategy,value,fill=factor(paste0(s,n1))))+
    geom_jitter(position = position_jitter(seed = 1, width = 0.2))+
    geom_boxplot()+
    theme_bw()+
    labs(y=var,title = title)+
    scale_fill_discrete()+
    theme(legend.position = 'NULL',
          axis.text.x = element_text(angle = 45))
}

# (p1<-plotRes(res.long,'CasesN',s4))
# (p2<-plotRes(res.long,'Doses',sList))
# (p3<-plotRes(res.long,'CasesA',sList))
# (p4<-plotRes(res.long,'NNT',sList))

myplotly<-function(obj){
  max<-max(obj$data$value)+100
  ggplotly(obj) %>% layout(yaxis = list(range = c(0, max)))
}


# nnt-avt scatter ----
options(ggrepel.max.overlaps = Inf)
plot_nnt<-function(sum_res){
  res_s1<-sum_res %>% filter(strategy=='S1')
  NNT_S1=res_s1$NNT
  CasesA_S1=res_s1$CasesA
  NNT_min=min(sum_res$NNT,na.rm=T)
  CasesA_max=max(sum_res$CasesA,na.rm=T)
  p <-
    ggplot(sum_res, aes(NNT, CasesA, label = label, color = s,fill=s)) +
    geom_point(aes(shape = I(shape),size=size)) +
    geom_text_repel(size=3)+
    #geom_label_repel(size=3,color = "white")+
    #geom_text(aes(y = CasesA + 1),size = 3,position = position_jitter(width = 1, height = 1)) +
    theme_bw() +
    labs(x = 'NNT', y = 'Cases averted')+
    geom_hline(yintercept = CasesA_S1,linetype='dashed')+
    geom_vline(xintercept = NNT_S1,linetype='dashed')+
    guides(size = FALSE)
  p_zoom<-p + facet_zoom(xlim=c(NNT_min,NNT_S1),
                         ylim=c(CasesA_S1,CasesA_max),horizontal = F)
  list(p=p,
       p_zoom=p_zoom)
}

#plot_nnt(sum_res)$p
#plot_nnt(sum_res)$p_zoom

# NNT-AVT facet ----

plot_facet<-function(sum_res){
  res_s1<-sum_res %>% filter(strategy=='S1')
  NNT_S1=res_s1$NNT
  CasesA_S1=res_s1$CasesA
  NNT_min=min(sum_res$NNT,na.rm=T)
  CasesA_max=max(sum_res$CasesA,na.rm=T)
  ggplot(sum_res, aes(NNT, CasesA, label = label, color = s,fill=s)) +
    geom_jitter(aes(shape = I(shape),size=size),alpha=0.8) +
    #geom_text_repel(size=3)+
    #geom_text(aes(y = CasesA + 1),size = 3,position = position_jitter(width = 1, height = 1)) +
    theme_bw() +
    labs(x = 'NNT', y = 'Averted Hospitalizations',fill='Strategy',color='Strategy')+
    geom_hline(yintercept = CasesA_S1,linetype='dashed')+
    geom_vline(xintercept = NNT_S1,linetype='dashed')+
    guides(size = FALSE)+
    facet_wrap(vars(s),nrow = 2)+
    theme(text=element_text(size=18))
}
#plot_facet(sum_res)

# Top res
top_res<-function(sum_res,indicator,n=20){
  sum_res_s1<-sum_res %>% filter(strategy=='S1')
  NNT_S1=sum_res_s1$NNT
  CasesA_S1=sum_res_s1$CasesA
  
  min_nnt<-sum_res %>%
    filter(dominate==T | strategy=='S1') %>% 
    transmute(strategy,CasesN,CasesA,Doses,NNT,ratio_nnt,ratio_casesa) %>%
    slice_min(ratio_nnt,n=n) %>%
    mutate(label_nnt=sprintf('%s%%',round((ratio_nnt-1)*100)),
           label_casesa=sprintf('+%s%%',round((ratio_casesa-1)*100))) %>%
    bind_rows(sum_res_s1,.)
  
  max_avt<-sum_res %>%
    filter(dominate==T | strategy=='S1') %>% 
    transmute(strategy,CasesN,CasesA,Doses,NNT,ratio_nnt,ratio_casesa) %>%
    slice_max(ratio_casesa,n=n) %>%
    mutate(label_nnt=sprintf('%s%%',round((ratio_nnt-1)*100)),
           label_casesa=sprintf('+%s%%',round((ratio_casesa-1)*100))) %>%
    bind_rows(sum_res_s1,.)
  
  if(indicator=='NNT'){
    return(min_nnt)
  } else {
    return(max_avt)
  }
}
#top_res(sum_res,'NNT',10)$strategy

# Top NNT or avt ----
top_echart<-function(sum_res,indicator){
  sum_res_s1<-sum_res %>% filter(strategy=='S1')
  NNT_S1=sum_res_s1$NNT
  CasesA_S1=sum_res_s1$CasesA
  if(indicator=='NNT'){
    df<-sum_res %>%
      filter(dominate==T | strategy=='S1') %>% 
      arrange(dominate,ratio_nnt,desc(ratio_casesa))
  } else {
    df<-sum_res %>%
      filter(dominate==T | strategy=='S1') %>% 
      arrange(dominate,desc(ratio_casesa),desc(ratio_nnt)) 
  }
  df %>%
    arrange(desc(row_number())) %>%
    e_chart(strategy,name='Strategy') %>%
    e_bar(NNT,name='NNT',bind=ratio_nnt,color='rgb(31,119,180)') %>%
    e_bar(CasesA,name='Averted Hospitalizations',bind=ratio_casesa,color='rgb(255,127,14)') %>%
    e_datazoom(type='inside',orient='vertical',startValue=940,endValue=960) %>%
    e_datazoom(type='slider',orient='vertical') %>%
    e_tooltip(trigger = 'axis',
              formatter = htmlwidgets::JS("
              function(params){
                console.log(params)
                var s=params[0].value[1]
                var nnt=params[0].value[0]
                var ratio_nnt=params[0].data.name
                var label_nnt=Math.round((ratio_nnt-1)*100)+'%'
                
                var casesa=params[1].value[0]
                var ratio_casesa=params[1].data.name
                var label_casesa=Math.round((ratio_casesa-1)*100)+'%'
                
                return(s+'<br> NNT: '+nnt+'('+label_nnt+')'+
                '<br> Averted Hospitalizations: '+ casesa+'(+'+label_casesa+')')
              }
            "),
            axisPointer = list(
              type = "shadow",
              axis='y'
            )) %>%
    e_flip_coords() %>%
    e_mark_line(data=list(xAxis=NNT_S1,
                          label=list(color='rgb(31,119,180)'),
                          lineStyle=list(type='dotted',color='rgb(31,119,180)')),
                symbol=c('none','none')) %>%
    e_mark_line(data=list(xAxis=CasesA_S1,
                          label=list(color='rgb(255,127,14)'),
                          lineStyle=list(type='dotted',color='rgb(255,127,14)')),
                symbol=c('none','none'))
}
#top_echart(sum_res,'NNT')
#top_echart(sum_res,'avt')

top_ggplot<-function(sum_res,indicator){
  sum_res_s1<-sum_res %>% filter(strategy=='S1') %>%
    select(strategy,CasesN,starts_with('CasesA'),Doses,starts_with('NNT'),ratio_nnt,ratio_casesa)
  NNT_S1=sum_res_s1$NNT
  CasesA_S1=sum_res_s1$CasesA
  
  min_nnt<-sum_res %>%
    filter(dominate==T) %>% 
    select(strategy,CasesN,starts_with('CasesA'),Doses,starts_with('NNT'),ratio_nnt,ratio_casesa) %>%
    slice_min(ratio_nnt,n=20,with_ties = F) %>% 
    mutate(label_nnt=sprintf('%s%%',round((ratio_nnt-1)*100)),
           label_casesa=sprintf('+%s%%',round((ratio_casesa-1)*100))) %>%
    bind_rows(sum_res_s1,.)
  
  max_avt<-sum_res %>%
    filter(dominate==T) %>% 
    select(strategy,CasesN,starts_with('CasesA'),Doses,starts_with('NNT'),ratio_nnt,ratio_casesa) %>%
    slice_max(ratio_casesa,n=20,with_ties = F) %>%
    mutate(label_nnt=sprintf('%s%%',round((ratio_nnt-1)*100)),
           label_casesa=sprintf('+%s%%',round((ratio_casesa-1)*100))) %>%
    bind_rows(sum_res_s1,.)
  plot_top<-function(df,name='NNT'){
    subtitle=ifelse(name=='NNT','Top 20 strategies with lowest NNT',
                    'Top 20 strategies with highest averted hospitalizations')
    df %>%
      pivot_longer(cols = c(NNT,CasesA)) %>% 
      mutate(label=ifelse(name=='NNT',label_nnt,label_casesa),
             lower=ifelse(name=='NNT',NNT_lower,CasesA_lower),
             upper=ifelse(name=='NNT',NNT_upper,CasesA_upper)) %>% 
      ggplot(aes(factor(strategy,levels=rev(df$strategy)),value,fill=name,label=label))+
      geom_col(position = 'dodge')+
      geom_errorbar(aes(ymin=lower,ymax=upper),
                    width=0.5,
                    position=position_dodge(.9))+
      geom_text(aes(y=upper+6),position=position_dodge(1),size=4,color='black')+
      theme_bw()+
      geom_hline(yintercept = c(NNT_S1,CasesA_S1),linetype='dotted')+
      scale_y_continuous(expand = expansion(add = c(0,5)))+
      coord_flip()+
      labs(x='Strategy',y='Averted RSV-ALRI hospitalizations or NNT',fill='',subtitle=subtitle)+
      theme(legend.position = 'top',
            legend.text = element_text(size=20),
            axis.title = element_text(size=20),
            axis.text = element_text(size=19),
            title = element_text(size=24))+
      scale_fill_d3(labels = c("Averted hospitalizations", "NNT"))
  }
  if(indicator=='NNT'){
    p<-plot_top(min_nnt)
  } else {
    p<-plot_top(max_avt,'avt')
  }
  p
}

#top_nnt<-top_ggplot(sum_res,'NNT')
#top_avt<-top_ggplot(sum_res,'avt')

# top_nnt+top_avt+
#   plot_annotation(tag_levels = 'A')+
#   plot_layout(tag_level = 'new',guides = "collect") & 
#   theme(legend.position = 'bottom')

print_parmas<-function(df){
  df<-df %>%
    select(-c(res_df,sList)) %>%
    slice_head(n=1)
  names<-names(df)
  value<-paste0(df)
  paste0(names,': ',value,collapse = '\n')
}

# Superior-S1 ----
plot_superior<-function(sum_res){
  sum_res_s1<-sum_res %>% filter(strategy=='S1')
  NNT_S1=sum_res_s1$NNT
  CasesA_S1=sum_res_s1$CasesA
  Doses_S1=sum_res_s1$Doses
  
  # sum_res_s1_by_n1_s3 ----
  df<-expand.grid(strategy='S1',
                  n1=6:11,
                  s3=factor(paste(rep(6:12,time=2),rep(c(6,12),each=7),sep='.'),
                            levels=paste(rep(6:12,time=2),rep(c(6,12),each=7),sep='.')))
  
  sum_res_s1_by_n1_s3<-full_join(
    df,sum_res_s1,by='strategy'
  ) %>%
    mutate(n1=n1.x)
  
  plot_fig5<-sum_res %>% filter(dominate==T) %>%
    mutate(s2=paste(n1,n2,sep='.'),
           s3=paste(n3,n4,sep = '.')) %>% 
    mutate(across(c(n1,n2,n3,n4),~as.numeric(.x))) %>% 
    mutate(s3=factor(s3,levels=paste(rep(6:12,time=2),rep(c(6,12),each=7),sep='.'))) %>% 
    ggplot(aes(NNT,CasesA))+
    geom_point(aes(color=factor(n2)),size=1,shape=16)+
    geom_errorbar(aes(color=factor(n2),xmin = NNT_lower, xmax = NNT_upper), width = 0.2)+
    geom_errorbar(aes(color=factor(n2),ymin = CasesA_lower, ymax = CasesA_upper), width = 0.2)+
    #geom_text()+
    facet_grid(n1~s3)+
    theme_bw()+
    geom_hline(yintercept = c(CasesA_S1),linetype='dotted',color='grey50')+
    geom_vline(xintercept=NNT_S1,linetype='dotted',color='grey50')+
    scale_shape_manual(values = c(1:8))+
    scale_color_lancet()+
    scale_y_continuous(sec.axis = sec_axis(~ . , name = "Starting month of immunization at birth(s)", breaks = NULL, labels = NULL),
                       breaks = c(CasesA_S1,90,100,110),
                       limits = c(82,110),
                       labels = function(breaks){
                         p<-round((breaks/CasesA_S1-1)*100)
                         sprintf('(+%d%%)%3d',p,breaks) %>%
                           str_replace('\\+0','0')
                       }) +
    scale_x_continuous(sec.axis = sec_axis(~ . , name = "Month and age threshold of immunization at season(m.a)", breaks = NULL, labels = NULL),
                       breaks = c(140,170,NNT_S1),
                       limits = c(128,205),
                       labels = function(breaks){
                         p<-round((breaks/NNT_S1-1)*100)
                         sprintf('%3d\n(%d%%) ',breaks,p)
                       }) +
    labs(x='The Doses of mAb needed to prevent one case of RSV-ALRI hospitalization (NNT)',
         y='Total averted RSV-ALRI Hospitalizations',color='Duration(d)',shape='Duration(d)')+
    theme(axis.text =  element_text(size=10),
          axis.title = element_text(size=17),
          strip.text = element_text(size=15),
          axis.text.x = element_text(size=10))+
    geom_errorbar(data=sum_res_s1_by_n1_s3,aes(ymin=CasesA_lower,ymax=CasesA_upper),width=.2)+
    geom_errorbar(data=sum_res_s1_by_n1_s3,aes(xmin=NNT_lower,xmax=NNT_upper),width=.2) 
  plot_fig5
}
