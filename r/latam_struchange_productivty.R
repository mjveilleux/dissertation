

#Macro MEX: structural change and markups= empirical work

library(tidyverse)
library(readxl)
library(plm)


va<- readxl::read_xlsx('/Users/masonveilleux/Dropbox/dissertation/alaska/data/macro_markups/la_KLEMS/MEX_Basic.xlsx',sheet = 'VA') %>% 
  slice(1:11) %>% pivot_longer(names_to = 'year',cols = 3:31, values_to = 'va') %>% filter(code != 'TOT')


emp<- readxl::read_xlsx('/Users/masonveilleux/Dropbox/dissertation/alaska/data/macro_markups/la_KLEMS/MEX_basic.xlsx',sheet = 'H_EMP') %>% 
  slice(1:11) %>% pivot_longer(names_to = 'year',cols = 3:31, values_to = 'emp')%>% filter(code != 'TOT')

tfp<- readxl::read_xlsx('/Users/masonveilleux/Dropbox/dissertation/alaska/data/macro_markups/la_KLEMS/MEX_GC.xlsx',sheet = 'LP2TFP_I') %>% 
  slice(1:11) %>% pivot_longer(names_to = 'year',cols = 3:31, values_to = 'tfp') %>% filter(code != 'TOT') %>% na.omit()

tfp<- tfp %>%mutate(tfp= tfp+10000) %>% group_by(year) %>%  mutate(tfp_tot = sum(tfp)) %>% ungroup() %>% select(-desc)

#%>% filter(year=='1991'|year=='2017')

df<- va %>% left_join(emp) %>% left_join(tfp,by = c("year","code")) %>% filter(year=='2016'|year=='1991') %>% 
  group_by(year) %>% mutate(total_emp = sum(emp),
                            share_emp = emp/total_emp) %>%
                                               ungroup() %>% 
                                               mutate(stfp=tfp/tfp_tot,
                                                      va_emp = va/emp) %>% 
                                                group_by(code) %>% 
                                            mutate(dshare_emp = share_emp - share_emp[year=='1991'],
                                                   dva_emp = va_emp - va_emp[year=='1991']) %>% 
  mutate(code = str_replace_all(code,'AtB','Agriculture'),
         code = str_replace_all(code,'C','Mining'),
         code = str_replace_all(code,'D','Manufactures'),
         code = str_replace_all(code,'E','Utilities'),
         code = str_replace_all(code,'F','Construction'),
         code = str_replace_all(code,'GtH','Tourism'),
         code = str_replace_all(code,'I','Transport/Comms'),
         code = str_replace_all(code,'JtK','Financial'),
         code = str_replace_all(code,'LtQ','Social Services'))

df%>% filter(year=='2016') %>%   ggplot(aes(x=(dshare_emp),y=log(stfp),label=code,group=year))+geom_text()+geom_smooth(se = F,method = "lm")

df%>% filter(year=='2016') %>% lm(data=.,log(stfp)~(dshare_emp)) %>% summary()




















df_mex <- df_mex %>% filter(year == '1991' | year == '2016')



df_mex<- df_mex %>%group_by(code) %>%  mutate(theta_diff = share_emp-share_emp[year=="1991"],
                                     val = va[year=="1991"]/emp[year=="1991"],
                                     reall = theta_diff*val,
                                     valg = va_emp-val) %>% ungroup()


df_mex<-df_mex %>% group_by(code,year) %>% mutate(dreall = lag(reall),
                                                  dshare_emp = lag(share_emp)) %>% ungroup()

#check total emp X

df_mex %>% filter(year =='2007') %>% select(emp) %>% sum()

#check share emp X

df_mex %>% filter(year=="2005") %>% select(share_emp) %>% sum()

#check theta_diff

a<- df_mex %>% filter(year==2000) %>% select(share_emp) %>% sum()




df<-  right_join(df_mex,tfp,by = c("code",'year','desc'))

df_mex %>% ggplot(aes(x=year,y=reall,group=desc,color=desc))+geom_line()

df %>% ggplot(aes(x=stfp,y=theta_diff))+geom_point()





t<- df %>% mutate(ltfp = log(tfp)) %>% na.omit()

plm((tfp)~reall, data = t,index = c('year','code')) %>% summary()

lm(data=df, tfp~ reall+as.factor(year)) %>% summary()


#va_emp[year==1990]

#theta_diff = share_emp - share_emp[year==1990] 
