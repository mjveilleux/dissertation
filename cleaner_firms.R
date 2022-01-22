



library(tidyverse)
library(lubridate)
library(zoo)
library(readxl)
ts_dataJan22 <- read_excel("Dropbox/ts_dataJan22.xlsx")


ts.df<- ts_dataJan22 %>% mutate(Date = as.Date(Date)) %>% 
  ts(start = 1928-01-03,frequency = 365)

ts.plot(ts.df)

decomp.ts<-decompose(ts.df)

decomp.ts %>% filter(random >)%>% plot()

(ts.df-decomp.ts$seasonal) %>% plot()


dynlm:: dynlm(decomp.ts$random ~ L(decomp.ts$random)+ L(decomp.ts$random, 2)+L(decomp.ts$random, 3)   ) %>% summary()



df<-read_csv('/Users/masonveilleux/Dropbox/Mac (2)/Downloads/zbp19detail.csv')

df %>% filter(stabbr =='AK')
colnames(df)






















  
  