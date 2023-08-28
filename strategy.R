pacman::p_load(tidyverse,lubridate,rio,
               echarts4r,patchwork,ggsci,furrr,
               janitor,ggrepel,ggforce,plotly,ggthemes,ggsci,tictoc,lubridate)

cat('---strategy.R---\n')

plan(multisession,workers=3)
# Import the incidence density of RSV-ALRI hospitalization ---------------------

if(file.exists('rda/nature_rsv_month_period_173052.rds')){
  nat<-import('rda/nature_rsv_month_period_173052.rds')
} else {
  nat<-import('nature_rsv_month_period_173052.rds')
}


nat.est<-nat %>% 
  filter(month!='Total') %>%
  select(month,period,est) %>%
  pivot_wider(id_cols = month,names_from = period,values_from = est)

nat.lower<-nat %>% 
  filter(month!='Total') %>%
  select(month,period,lower) %>%
  pivot_wider(id_cols = month,names_from = period,values_from = lower)

nat.upper<-nat %>% 
  filter(month!='Total') %>%
  select(month,period,upper) %>%
  pivot_wider(id_cols = month,names_from = period,values_from = upper)

m.est<-nat.est %>% select(-1) %>% as.matrix()
m.lower<-nat.lower %>% select(-1) %>% as.matrix()
m.upper<-nat.upper %>% select(-1) %>% as.matrix()

# Assign the incidence density of the second month to the first month
m.est[,1]<-m.est[,2]
m.lower[,1]<-m.lower[,2]
m.upper[,1]<-m.upper[,2]

# Functions --------------------------------------------------------------

# population
getPop<-function(population=12000000,br=7/1000){
  matrix(rep(population*br/12,12*24),nrow = 12)
}
#sum(getPop(12000000,7/1000)*1/12*m.est/1000)
#sum(getPop(12000000,7/1000)*1/12*getRateN(1234)/1000)
# efficacy
getEfficacy<-function(seed=1234,min=0.6,max=0.6){
  set.seed(seed)
  efficacy<-runif(1,min,max)
  mat<-matrix(rep(efficacy,12*24),nrow = 12)
  #cat('Efficacy:',min,'~',max,'\n')
  mat
}
getEfficacy(1234,0.6,0.6)


mat2df<-function(mat){
  as.data.frame(t(mat))
}
df2mat<-function(df){
  mat<-t(df) 
  rownames(mat)<-NULL 
  mat
}


# efficacy lag, protection duration
df2lag<-function(df,lag=5){
  df %>%
    mutate(across(everything(),~{
      paste(.x,collapse = '')
    })) %>%
    mutate(across(everything(),~{
      substr(.x,row_number()-lag+1,row_number()) %>% str_detect('1') %>% as.integer()
    }))
}

# Intervention, candidate strategies
getInt<-function(strategy='S1'){
  m<-matrix(rep(0,12*24),nrow = 12)
  if(strategy=='S1'){
    m[,1]=1
  } else if (str_detect(strategy,'S2')){
    start = str_split_1(strategy,'\\.')[2] %>% as.numeric()
    length = str_split_1(strategy,'\\.')[3] %>% as.numeric()
    seq=seq(start,length.out=length)
    index=case_when(seq>12~seq-12,T~seq)
    m[index,1]=1
    #cat('Strategy:',strategy,'\n')
  } else if (str_detect(strategy,'S3')){
    month = str_split_1(strategy,'\\.')[2] %>% as.numeric()
    age = str_split_1(strategy,'\\.')[3] %>% as.numeric()
    for(i in 1:12){
      for(j in 1:24) {
        mod<-(i+j-1) %% 12
        mod<-ifelse(mod==0,12,mod)
        if(mod==month & j<=age){m[i,j]=1}
      }
    }
    #cat('Strategy:',strategy,'\n')
  } else if (str_detect(strategy,'S4')){
    start = str_split_1(strategy,'\\.')[2] %>% as.numeric()
    length = str_split_1(strategy,'\\.')[3] %>% as.numeric()
    month = str_split_1(strategy,'\\.')[4] %>% as.numeric()
    age = str_split_1(strategy,'\\.')[5] %>% as.numeric()
    seq=seq(start,length.out=length)
    index=case_when(seq>12~seq-12,T~seq)
    m[index,1]=1
    for(i in 1:12){
      for(j in 1:24) {
        mod<-(i+j-1) %% 12
        mod<-ifelse(mod==0,12,mod)
        if((!(i %in% index)) & mod==month & j<=age){m[i,j]=1}
      }
    }
    #cat('Strategy:',strategy,'\n')
  }
  m
}

int2Protect<-function(Int,lag=5){
  Int %>% mat2df() %>% df2lag(lag) %>% df2mat()
}

getInt('S1')
getInt('S2.6.12')
getInt('S3.1.5')
getInt('S3.1.24')
getInt('S4.6.6.8.24')

#getInt() %>% mat2df() %>% df2lag(2) %>% df2mat()

# incidence density, hospitalization rate under the Natural condition 
getRateN<-function(seed=1234){
  set.seed(seed)
  df<-data.frame(est=as.vector(m.est),
                 lower=as.vector(m.lower),
                 upper=as.vector(m.upper)) %>%
    rowwise() %>%
    #mutate(rate=rpois(1,runif(1,lower,upper)))
    mutate(rate=rpois(1,est))
  matrix(df$rate,ncol = 24)
}

# cases under nature 
getCasesN<-function(pop,rate){
  pop*(1/12)*rate/1000
}

# cases averted by strategies
getCasesA<-function(pop,rate,coverage_rate){
  pop*(1/12)*rate*coverage_rate/1000
}

# rate averted
getRateA<-function(rateN,VE){
  rateN*VE
}

# coverage rate
getCov<-function(coverage_rate=0.2){
  matrix(rep(coverage_rate,12*24),nrow = 12)
}



# Candidate strategies list ------------
s2<-paste0('S2.',rep(6:12,each=8),'.',rep(4:11,time=7))
s3<-paste0('S3.',rep(6:12,each=2),'.',rep(c(6,12),time=7))
s4<-paste0('S4.',rep(str_sub(s2,4),each=length(s3)),'.',rep(str_sub(s3,4),time=length(s2)))
sList<-c('S1',s2,s3,s4)

# strategy visualization
matrix_aug<-function(matrix){
  mat<-matrix(nrow = 12,ncol=35)
  for(i in 1:12){
    mat[i,seq(i,length.out=24)]=matrix[i,1:24]
  }
  mat
}

plot_int<-function(strategy='S1',protect_month=5){
  df<-getInt(strategy) %>% int2Protect(lag=protect_month) %>% matrix_aug() %>% 
    as.data.frame() %>%
    setNames(c(month.abb,month.abb,month.abb[1:11])) %>%
    clean_names() %>%
    set_names(~str_to_sentence(.x)) %>%
    as.data.frame() %>%
    rownames_to_column('bm') %>%
    mutate(bm=month.abb[as.numeric(bm)]) %>%
    pivot_longer(-1,names_to = 'month',values_drop_na = F) %>%
    mutate(bm=factor(bm,levels=month.abb)) %>%
    arrange(desc(bm))
  
  df %>%
    e_charts(month) %>%
    e_heatmap(bm,value,itemStyle = list(emphasis = list(shadowBlur = 10))) %>%
    e_visual_map(value,show=F,color=c('#15607A',"#9EB7DBB6")) %>%
    e_labels(position = 'inside') %>%
    e_tooltip() %>%
    e_x_axis(axisLabel = list(interval = 0, rotate = 45)) %>%
    e_grid(top='20px',bottom='10%',right='20px',left='10%')
}

#plot_int('S4.12.5.6.12')

# Final Model -------------------------------------------------------------
model_for<-function(scenario){
  print(scenario)
  print(scenario$sList)
  params<-scenario
  seed=params$seed
  repetition=params$repetition
  population=params$population
  br=params$br
  efficacy_min=params$efficacy_min
  efficacy_max=params$efficacy_max
  coverage_rate=params$coverage_rate
  protection_month=params$protection_month
  strategy_list=unlist(params$sList)
  
  #
  set.seed(seed)
  seeds<-as.integer(runif(repetition)*1000)
  res_df<-data.frame()
  for(i in 1:repetition){
    print(i)
    pop=getPop(population,br) # population of the simulated cohort
    rateN=getRateN(seeds[i]) # hospitalization rate under nature condition
    efficacy=getEfficacy(seeds[i],efficacy_min,efficacy_max) # efficacy of mAb or vaccine
    casesN=getCasesN(pop,rateN) # RSV-ALRI hospitalization under nature condition
    
    
    int=future_map(strategy_list,~getInt(.x)) # is intervention (mAb or vaccine) or not
    isProtect=future_map2(int,protection_month,~int2Protect(.x,.y)) # within protective duration or not
    VE=future_map(isProtect,~.x*efficacy) # vaccine(or mAb) effectiveness
    rateA=future_map(VE,~getRateA(rateN,.x))
    casesA=future_map(rateA,~getCasesA(pop,.x,coverage_rate)) # RSV-ALRI hospitalization averted by intervention
    doses=future_map(int,~pop*.x*coverage_rate) # doses of vaccine or mAb consumed under intervention
    
    CasesA=future_map_dbl(casesA,sum) # total RSV-ALRI hospitalization averted
    CasesN=sum(casesN) # total RSV-ALRI hospitalization under nature condition
    Doses=future_map_dbl(doses,sum) # total dosed consumed
    NNT=Doses/CasesA # the number of vaccine or mAb doses needed to prevent one case of RSV-ALRI hospitalization
    tmp_df<-data.frame(strategy=strategy_list,seed=seeds[i],CasesN=CasesN,CasesA=CasesA,Doses=Doses,NNT=NNT)
    res_df<-bind_rows(res_df,tmp_df)
  }
  res_df
}

# scenario parameters 
# scenario_df<-tribble(
#   ~seed,~repetition,~population,~br,~efficacy_min,~efficacy_max,~coverage_rate,~protection_month,
#   1234,100,12000000,7/1000,0.6,0.6,0.2,5,
#   1234,100,12000000,7/1000,0.8,0.8,0.2,5,
#   1234,100,12000000,7/1000,0.6,0.8,0.2,5,
#   1234,100,12000000,7/1000,0.6,0.6,0.4,5,
#   1234,100,12000000,7/1000,0.8,0.8,0.4,5,
#   1234,100,12000000,7/1000,0.6,0.8,0.4,5,
#   1234,100,12000000,7/1000,0.6,0.6,0.2,4,
#   1234,100,12000000,7/1000,0.8,0.8,0.2,4,
#   1234,100,12000000,7/1000,0.6,0.8,0.2,4
# ) %>%
#   mutate(sList=map(seed,~sList)) %>%
#   mutate(res_df=NA)


# Run Model ----

#rio::export(scenario_df_all,'shiny/scenario_df_all.rds')
#scenario_df_all<-import('shiny/scenario_df_all.rds')

# scenario_df<-tribble(
#   ~seed,~repetition,~population,~br,~efficacy_min,~efficacy_max,~coverage_rate,~protection_month,
#   1234,100,12000000,7/1000,0.6,0.6,0.2,5,
#   1234,100,12000000,7/1000,0.5,0.5,0.2,5,
#   1234,100,12000000,7/1000,0.7,0.7,0.2,5,
#   1234,100,12000000,7/1000,0.8,0.8,0.2,5,
#   1234,100,12000000,7/1000,0.6,0.6,0.4,5,
#   1234,100,12000000,7/1000,0.6,0.6,0.6,5,
#   1234,100,12000000,7/1000,0.6,0.6,0.8,5,
#   1234,100,12000000,7/1000,0.6,0.6,0.2,3,
#   1234,100,12000000,7/1000,0.6,0.6,0.2,4
# ) %>%
#   mutate(sList=map(seed,~sList)) %>%
#   mutate(res_df=NA)
  
run_model<-function(scenario_df,scenario_df_all){
  new_scenario<-anti_join(scenario_df,scenario_df_all %>%
                            select(-c(res_df,sList)))
  cat('New scenario:',nrow(new_scenario),'\n')
  if(nrow(new_scenario)>0){
    for(l in 1:nrow(new_scenario)){
      new_scenario[[l,'res_df']]<-list(model_for(new_scenario[l,]))
    }
    rio::export(new_scenario,paste0('shiny/','new_scenario-',format(Sys.time(), "%Y-%M-%d %H-%m"),'.rds'))
    df_all<-bind_rows(scenario_df_all,new_scenario)
    rio::export(df_all,paste0('shiny/scenario_df_all.rds'))
  }
}

#run_model(scenario_df,scenario_df_all)

#model_for(scenario_df[1,])
