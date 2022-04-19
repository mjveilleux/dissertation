


#we have histograms across sectors by weighted and unweighted markups
# I then plot the relationship between markups and labour inputs: finding a negative then positive relationship
# I also look at ag labour share's relationship with markups.

library(tidyverse)
library(readxl)
library(haven)
library(ggfortify)
library(ggrepel)
library(tidyr)



#time series: weighted average of markups by output (consumer impact)
u<- read_dta('/Users/masonveilleux/Dropbox/dissertation/alaska/data/macro_markups/bls_data.dta') %>% 
  na.omit() %>% mutate(year = as.character(year),
                       naics = str_replace_all(naics,',','')) %>% 
                group_by(year) %>% summarise(wavg_u = weighted.mean(mkup_hall,Y))

 


#histogram through time (unweighted)
read_dta('/Users/masonveilleux/Dropbox/dissertation/alaska/data/macro_markups/bls_data.dta') %>% 
  na.omit() %>% mutate(year = as.character(year),
                       naics = str_replace_all(naics,',','')) %>% mutate(VA=Y/(E+K+L+M+S)) %>% group_by(naics)  %>% 
  summarise(mean_u = mean(mkup_hall),
            wavg_u =weighted.mean(mkup_hall,VA)) %>% ggplot()+geom_density(aes(x=mean_u,color="Mean"))+geom_density(aes(x=wavg_u,color = "Weighted Mean"))


read_dta('/Users/masonveilleux/Dropbox/dissertation/alaska/data/macro_markups/bls_data.dta') %>% 
  na.omit() %>% mutate(year = as.character(year),
                       naics = str_replace_all(naics,',','')) %>% mutate(VA=Y/(E+K+L+M+S))  %>% group_by(year)  %>% 
  summarise(mean_u = mean(mkup_hall),
            wavg_u_va =weighted.mean(mkup_hall,VA),
            wavg_u_y =weighted.mean(mkup_hall,Y)) %>% ts() %>% autoplot()



#histogram through time (weighted by Y)
read_dta('/Users/masonveilleux/Dropbox/dissertation/alaska/data/macro_markups/bls_data.dta') %>% 
  na.omit() %>% mutate(year = as.character(year),
                       naics = str_replace_all(naics,',','')) %>% group_by(naics) %>% 
  summarise(wavg_u =weighted.mean(mkup_hall,Y)) %>%
  ggplot()+geom_density(aes(x=wavg_u))


#look at labour force growth

hrs<- read_excel('/Users/masonveilleux/Dropbox/dissertation/alaska/data/macro_markups/US_KLEMS_detailed.xlsx', sheet = 'Labor Hours_Quantity',skip = 1) %>% 
  rename(naics=1,desc=2) %>% pivot_longer(names_to = 'year',cols = 3:35, values_to = 'hrs') %>% na.omit() %>% 
  select(-desc) %>% 
  group_by(year) %>% 
  summarise(mean_hrs = mean(hrs))
  


left_join(hrs,u) %>% mutate(hrs = log(mean_hrs) - dplyr:: lag(log(mean_hrs)),
                            v = log(wavg_u) - dplyr::lag(log(wavg_u)),
                            year = as.numeric(year)) %>% ggplot(aes(x=log(mean_hrs),y=log(wavg_u),label = year))+
  geom_point() + geom_text()+
geom_segment(aes(
  xend=c(tail(log(mean_hrs), n=-1), NA), 
  yend=c(tail(log(wavg_u), n=-1), NA)
)
) +ggtitle('Markups and Labour Inputs',subtitle = '3 distinct periods: 1988-2000 negative, 2001-2008 indifferent, 2009-2017 positive')


#ag labour share over time scattered with markups

share<- read_excel('/Users/masonveilleux/Dropbox/dissertation/alaska/data/macro_markups/US_KLEMS_detailed.xlsx', sheet = 'Labor Hours_Quantity',skip = 1) %>% 
  rename(naics=1,desc=2) %>% pivot_longer(names_to = 'year',cols = 3:35, values_to = 'hrs') %>% na.omit() %>% 
  select(-desc) %>% group_by(year) %>%  mutate(tot_hrs = sum(hrs),
                                               share_lab = hrs/tot_hrs) %>% filter(naics == '111112')

left_join(share,u)  %>% ggplot(aes(y=wavg_u,x=share_lab,label=year))+geom_point() +geom_text()+geom_segment(aes(
  xend=c(tail((share_lab), n=-1), NA), 
  yend=c(tail((wavg_u), n=-1), NA)
)
)

left_join(share,u) %>% select(-naics) %>% ts(start = 1988,frequency = 1) %>% ts(d(wavg_u)~d(share_lab))

left_join(share,u) %>% select(-naics) %>% write_csv('/Users/masonveilleux/Dropbox/dissertation/alaska/data/macro_markups/shareag_u.csv')










