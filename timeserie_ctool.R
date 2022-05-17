library(tidyverse)
library(lubridate)
library(patchwork) #  arranging plots
library(glue)
library(tsibble)
library(feasts) # feature extraction and statistics
library(fable) # forecasting
library(tsibbledata)
library(xts)

out_a <- readRDS("out_a.RDS") 

#### ts ----

out_fil_ts <- out_a %>% 
  filter(croprot_man == 'High', 
         inittime == '100', 
         initC == 'grass',
         soilCinit == 'sC0.9',
         soiltype == 'JB1') %>%
  arrange(year2) %>%
  select(totalC_topsoil) %>%
  ts(start = 1996, frequency = 1)

str(out_fil_ts)
class(out_fil_ts)


y <- ts_SOC %>% filter(id==" 37") 

ts_soc <-ts(y$SoilC_tot,  frequency = 5)

plot(ts_soc)

ts_soc_d <- decompose(ts_soc)

plot(ts_soc_d)

#### zoo ----
#can make a list with all the factors differencieted 

out_fil <- out_a %>% 
  filter(croprot_man == 'High', 
         inittime == '100', 
         initC == 'grass',
         soilCinit == 'sC0.9',
         soiltype == 'JB1') %>%
  arrange(year2) %>%  
  select(totalC_topsoil, year2)

out_fil_xts <- xts(out_fil$totalC_topsoil, order.by = out_fil$year2)
str(out_fil_xts) 


#### tsibble ----

out_tsibble <- out_a %>% 
  mutate(Date = year(year2)) %>%
  rename(Qtr = Date) %>% #must be rename for tsibble index
  tsibble(key = c(soilCinit, soiltype, 
                  croprot, croprot_man,
                  inittime, initC), 
          index = Qtr)

head(out_tsibble)



out_fil_tsibble <- out_tsibble %>% filter(id == "100")


#### naive mode ----
mdl <- out_fil_tsibble %>%
  model(mdl_naive = NAIVE(totalC_topsoil))

mdl %>% broom::augment()

mdl %>% gg_tsresiduals()

out_tsibble %>%
  features(totalC_topsoil, list(mean = mean, quantile = quantile))

out_tsibble %>% features(totalC_topsoil, feat_acf)






