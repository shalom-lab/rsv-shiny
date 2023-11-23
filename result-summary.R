source('rsv-shiny/result-viz.R')
library(openxlsx)
library(tidyverse)
library(rio)

scenario_df_all<-import('rsv-shiny/scenario_df_all.rds')

scenario_sum<-scenario_df_all %>%
  mutate(sum_res=map(res_df,~summarise_res2(.x)))

# Sum_res ----
sum_res<-scenario_sum[[1,'sum_res']][[1]]

sum_res %>% count(dominated,dominate,color)

# Top 20 ----

mPlot<-function(sum_res){
  top_nnt<-top_ggplot(sum_res,'NNT')
  top_avt<-top_ggplot(sum_res,'avt')
  
  top_nnt+top_avt+
    plot_annotation(tag_levels = 'A')+
    plot_layout(tag_level = 'new',guides = "collect") &
    theme(legend.position = 'bottom')
}
sum_res<-scenario_sum[[1,'sum_res']][[1]]
mPlot(sum_res)
ggsave('rsv-shiny/plot/top-20.png',width = 2000,height=1000,dpi = 100,units = 'px')
plot_facet(sum_res)+
  labs(y='Total averted RSV-ALRI hospitalizations',
       x='The number of monoclonal antibody doses needed to prevent one case of RSV-ALRI hospitalization (NNT)')
ggsave('rsv-shiny/plot/strategy-facet.png',width = 1500,height=1000,dpi = 200,units = 'px')

scenario_sum[[2,'sum_res']][[1]] %>%
  mPlot() %>% ggsave('rsv-shiny/plot/top-20(efficacy_0.5).png',plot=.,width = 2000,height=1000,dpi = 100,units = 'px')

scenario_sum[[3,'sum_res']][[1]] %>%
  mPlot() %>% ggsave('rsv-shiny/plot/top-20(efficacy_0.7).png',plot=.,width = 2000,height=1000,dpi = 100,units = 'px')

scenario_sum[[5,'sum_res']][[1]] %>%
  mPlot() %>% ggsave('rsv-shiny/plot/top-20(cov_0.4).png',plot=.,width = 2000,height=1000,dpi = 100,units = 'px')

scenario_sum[[6,'sum_res']][[1]] %>%
  mPlot() %>% ggsave('rsv-shiny/plot/top-20(cov_0.6).png',plot=.,width = 2000,height=1000,dpi = 100,units = 'px')

scenario_sum[[8,'sum_res']][[1]] %>%
  mPlot() %>% ggsave('rsv-shiny/plot/top-20(prot_3m).png',plot=.,width = 2000,height=1000,dpi = 100,units = 'px')

scenario_sum[[9,'sum_res']][[1]] %>%
  mPlot() %>% ggsave('rsv-shiny/plot/top-20(prot_4m).png',plot=.,width = 2000,height=1000,dpi = 100,units = 'px')

scenario_sum[[10,'sum_res']][[1]] %>%
  mPlot() %>% ggsave('rsv-shiny/plot/top-20(prot_6m).png',plot=.,width = 2000,height=1000,dpi = 100,units = 'px')

scenario_sum[[11,'sum_res']][[1]] %>%
  mPlot() %>% ggsave('rsv-shiny/plot/top-20(prot_7m).png',plot=.,width = 2000,height=1000,dpi = 100,units = 'px')

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

# Dominate S1 ----
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
ggsave('rsv-shiny/plot/Figure5.png',width = 2000,height=1000,dpi = 150,units = 'px')

# Percent NNT - Averted ----
plot_percent<-sum_res %>% filter(dominate==T) %>%
  mutate(s2=paste(n1,n2,sep='.'),
         s3=paste(n3,n4,sep = '.')) %>% 
  mutate(across(c(n1,n2,n3,n4),~as.numeric(.x))) %>% 
  mutate(s3=factor(s3,levels=paste(rep(6:12,time=2),rep(c(6,12),each=7),sep='.'))) %>%
  ggplot(aes(ratio_nnt-1,ratio_casesa-1,color=factor(n2),
             #shape=factor(n2),
             label=factor(n2)))+
  geom_point(size=1)+
  #geom_text()+
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
  theme(axis.text =  element_text(size=10),
        axis.title = element_text(size=16),
        strip.text = element_text(size=14))

ggsave(plot_percent,'shiny/plot/domitate-s1-percent.png',width = 2000,height=1000,dpi = 150,units = 'px')


# Doses - Avertred ----
sum_res %>% filter(dominate==T) %>%
  mutate(s2=paste(n1,n2,sep='.'),
         s3=paste(n3,n4,sep = '.')) %>% 
  mutate(across(c(n1,n2,n3,n4),~as.numeric(.x))) %>% 
  mutate(s3=factor(s3,levels=paste(rep(6:12,time=2),rep(c(6,12),each=7),sep='.'))) %>%
  ggplot(aes(Doses,CasesA,color=factor(n2),shape=factor(n2),
             label=factor(n2)))+
  #geom_point(size=2)+
  geom_text()+
  facet_grid(n1~s3)+
  theme_bw()+
  geom_hline(yintercept = c(CasesA_S1),linetype='dotted')+
  geom_vline(xintercept=Doses_S1,linetype='dotted')+
  scale_shape_manual(values = c(1:8))+
  scale_color_lancet()+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Starting month of immunization at birth(s)", breaks = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Month and age threshold of immunization at season(m.a)", breaks = NULL, labels = NULL)) +
  labs(x='Doses Consumed',y='Averted Hospitalizations',color='Duration(d)',shape='Duration(d)')+
  theme(axis.text.x =  element_text(size=8))

plot_count/plot_percent+
plot_annotation(tag_levels = 'A')+
  plot_layout(tag_level = 'new',guides = "collect") &
  theme(legend.position = 'right',
        text = element_text(size=40))

ggsave('rsv-shiny/plot/Figure5.png',width = 3000,height=3000,dpi = 200,units = 'px')


ggplot(mtcars,aes(mpg,disp))+
  geom_point()+
  scale_x_continuous(limits=c(5,40),breaks = c(5,10,30),
                     labels = c(5,10,30))

ggplot(mtcars,aes(cyl))+
  geom_bar()+
  coord_polar(theta = 'x')
