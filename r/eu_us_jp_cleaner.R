

#get markups for eu klems 2021 1995-2020


library(tidyverse)
library(stringr)


#K
k<-eu_k %>% select(1:5,K=Kq_GFCF,pK= K_GFCF)
#S
s<-eu_intang %>% select(1:5,S=Iq_Intang,pS =	I_Intang)

#combine

clean<-eu_na %>% select(1:5,Y=GO_Q,
                 pY= GO_CP,
                 L=H_EMP,
                 pL=COMP,
                 M=II_Q,
                 pM =	II_CP,
                 VA_CP) %>% left_join(k) %>% left_join(s) %>% na.omit()

#check what countries are the full sample
clean %>%
  group_by(geo_name) %>%
  filter(all(1995:2018 %in% year)) %>% select(geo_name) %>% unique()


clean$geo_name %>% unique()














