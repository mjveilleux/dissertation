
#compare markups with labour productivity, and change in employment share

library(tidyverse)
library(readxl)
library(haven)
library(lmtest)
library(sandwich)
library(plm)


mfp<- read_excel('/Users/masonveilleux/Dropbox/dissertation/alaska/data/macro_markups/US_KLEMS_detailed.xlsx', sheet = 'MFP',skip = 1) %>% 
  rename(naics=1,desc=2) %>% pivot_longer(names_to = 'year',cols = 3:35, values_to = 'mfp') %>% select(-desc)

va<- read_excel('/Users/masonveilleux/Dropbox/dissertation/alaska/data/macro_markups/US_KLEMS_detailed.xlsx', sheet = 'Value Added',skip = 1) %>% 
  rename(naics=1,desc=2) %>% pivot_longer(names_to = 'year',cols = 3:35, values_to = 'va')%>% select(-desc)

lab<- read_excel('/Users/masonveilleux/Dropbox/dissertation/alaska/data/macro_markups/US_KLEMS_detailed.xlsx', sheet = 'Labor Hours_Quantity',skip = 1) %>% 
  rename(naics=1,desc=2) %>% pivot_longer(names_to = 'year',cols = 3:35, values_to = 'lab')

#bring in data from markupest in STATA
us<- read_dta('/Users/masonveilleux/Dropbox/dissertation/alaska/data/macro_markups/bls_data.dta') %>% na.omit() %>% mutate(year = as.character(year),
                  naics = str_replace_all(naics,',','')) %>% group_by(naics)%>% 
  mutate(dmkup = log(mkup_hall)-dplyr::lag(log(mkup_hall)))

#join the other variables
join<- mfp %>% left_join(va) %>% left_join(lab, by = c('year','naics')) %>% 
  mutate(lp = va/lab) %>% group_by(year) %>%  mutate(tot_lab = sum(lab),
                                                     share_lab = lab/tot_lab) %>% 
  group_by(naics) %>% 
  mutate(dshare_lab = 100*(share_lab - dplyr::lag(share_lab)),
         dlp = log(lp) - dplyr::lag(log(lp)),
         dlab = log(lab) - dplyr::lag(log(lab)),
         dmfp = log(mfp) -dplyr:: lag(log(mfp)))

#combine all to make dataframe
df1<- us %>% left_join(join, by= c('year','naics'))

#Look at the relationship between markups, labour share, and productivity: RE
plm(data=df1, log(mkup_hall)~log(share_lab),index = c('desc','year'),model = "random",effect = 'twoways') %>% summary()

#Pooled OLS with robuts standard errors
fit1<- lm(data=df1, log(mkup_hall)~log(share_lab)+log(mfp)+as.factor(desc)+as.factor(year))
coeftest(fit1, vcov = vcovHC(fit1, type="HC1"))
fit1 %>% summary()
#residuals a bit wonky
plot(fit1)

#plot fitted with actual
df1 %>% ungroup() %>% select(mkup_hall) %>% mutate(lmk = log(mkup_hall))  %>% cbind(fit1$residuals)
test<-cbind(fit1$fitted.values,fit1$model)
test$`log(mfp)`
ggplot(test, aes(y=`fit1$fitted.values`,x=`log(mfp)`))+geom_point()



plot<-df1 %>% ggplot(aes(x=year,y=log(mkup_hall),color=naicstitle,group=naicstitle))+geom_line()

plotly::ggplotly(plot)





