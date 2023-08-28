source('shiny/result-viz.R')
library(openxlsx)

scenario_df_all<-import('rsv-shiny/scenario_df_all.rds')

res_df<-scenario_df_all[[1,'res_df']][[1]]

scenario_sum<-scenario_df_all %>%
  mutate(sum_res=map(res_df,~summarise_res(.x)),
         sum_res2=map(res_df,~summarise_res2(.x)))

# Sum_res ----
sum_res<-scenario_sum[[1,'sum_res']][[1]]

sum_res %>% count(dominated,dominate,color)

# Dominate S1 ----
sum_res_s1<-sum_res %>% filter(strategy=='S1')
NNT_S1=sum_res_s1$NNT
CasesA_S1=sum_res_s1$CasesA

sum_res %>% filter(dominate==T) %>%
  mutate(s2=paste(n1,n2,sep='.'),
         s3=paste(n3,n4,sep = '.')) %>% 
  mutate(across(c(n1,n2,n3,n4),~as.numeric(.x))) %>% 
  mutate(s3=factor(s3,levels=paste(rep(6:12,time=2),rep(c(6,12),each=7),sep='.'))) %>%
  ggplot(aes(NNT,CasesA,color=factor(n2),shape=factor(n2)))+
  geom_point(size=2)+
  facet_grid(n1~s3)+
  theme_bw()+
  geom_hline(yintercept = c(CasesA_S1),linetype='dotted')+
  geom_vline(xintercept=NNT_S1,linetype='dotted')+
  scale_shape_manual(values = c(1:8))+
  scale_color_lancet()+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Starting month of immunization at birth(s)", breaks = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Month and age threshold of immunization at season(m.a)", breaks = NULL, labels = NULL)) +
  labs(x='NNT',y='Averted Hospitalizations',color='Duration(d)',shape='Duration(d)')+
  theme(axis.text.x =  element_text(size=8))

ggsave('shiny/plot/domitate-s1.png',width = 2000,height=1000,dpi = 150,units = 'px')

sum_res %>% filter(dominate==T) %>%
  mutate(s2=paste(n1,n2,sep='.'),
         s3=paste(n3,n4,sep = '.')) %>% 
  mutate(across(c(n1,n2,n3,n4),~as.numeric(.x))) %>% 
  mutate(s3=factor(s3,levels=paste(rep(6:12,time=2),rep(c(6,12),each=7),sep='.'))) %>%
  ggplot(aes(ratio_nnt-1,ratio_casesa-1,color=factor(n2),shape=factor(n2)))+
  geom_point(size=2)+
  facet_grid(n1~s3)+
  theme_bw()+
  geom_hline(yintercept = 0,linetype='dotted')+
  geom_vline(xintercept=0,linetype='dotted')+
  scale_shape_manual(values = c(1:8))+
  scale_color_lancet()+
  scale_y_continuous(labels = scales::percent_format(),sec.axis = sec_axis(~ . , name = "Starting month of immunization at birth(s)", breaks = NULL, labels = NULL)) +
  scale_x_continuous(labels = scales::percent_format(),sec.axis = sec_axis(~ . , name = "Month and age threshold of immunization at season(m.a)", breaks = NULL, labels = NULL)) +
  labs(x='Percentage reduction in NNT compared to year-round strategy(S1)',
       y='Percentage increase in averted hospitalizations compared to year-round strategy(S1)',
       color='Duration(d)',shape='Duration(d)')+
  theme(axis.text.x =  element_text(size=8))

ggsave('shiny/plot/domitate-s1-percent.png',width = 2000,height=1000,dpi = 150,units = 'px')

# Top 20 ----

top_nnt<-top_ggplot(sum_res,'NNT')
top_avt<-top_ggplot(sum_res,'avt')

top_nnt+top_avt+
  plot_annotation(tag_levels = 'A')+
  plot_layout(tag_level = 'new',guides = "collect") &
  theme(legend.position = 'bottom')

ggsave('shiny/plot/top-20.png',width = 2000,height=1000,dpi = 100,units = 'px')

# Table ----
sum_res %>%
  filter(dominate==T | strategy=='S1') %>%
  arrange(s,ratio_nnt,desc(ratio_casesa)) %>%
  slice_head(n=21) %>%
  transmute(Strategy=strategy,`Hospitalizations without intervention`=CasesN,
            `Averted hospitalizations`=CasesA,`Doses consumed`=Doses,NNT,
            `Percentage decrease in NNT (%)`=round(100*(1-ratio_nnt)),
            `Percentage increase in averted hospitalizations (%)`=round(100*(ratio_casesa-1))) %>%
  write.xlsx('shiny/top-20-nnt.xlsx',asTable = T,overwrite = T)

sum_res %>%
  filter(dominate==T | strategy=='S1') %>% 
  arrange(s,desc(ratio_casesa),ratio_nnt) %>% 
  slice_head(n=21) %>%
  transmute(Strategy=strategy,`Hospitalizations without intervention`=CasesN,
            `Averted hospitalizations`=CasesA,`Doses consumed`=Doses,NNT,
            `Percentage decrease in NNT (%)`=round(100*(1-ratio_nnt)),
            `Percentage increase in averted hospitalizations (%)`=round(100*(ratio_casesa-1))) %>%
  write.xlsx('shiny/top-20-avt.xlsx',asTable = T,overwrite = T)
