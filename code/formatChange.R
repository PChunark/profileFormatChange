library(readxl)
library(scales)
library(cowplot)
library(grid)
library(gridExtra)
library(tidyverse)

profile <- read_excel("rawdata/exprofile.xlsx",
           # sheet = "Generation profile",
           range = "A2:LYA2",
           col_names = c("vspp", "in_cap_mw","mwh",1:8760)) %>% 
  select(-"in_cap_mw", -"mwh") %>% 
  pivot_longer(-"vspp", names_to = "hourly", values_to = "mw") %>% 
  mutate(datetime = seq(as.POSIXct("2019-01-01 00:30:00", format="%Y-%m-%d %H:%M:%S"), 
                        as.POSIXct("2020-01-01 00:00:00", format="%Y-%m-%d %H:%M:%S"),
                        by="1 hour"),
         year = year(datetime),
         month = month(datetime),
         day = day(datetime),
         hour = hour(datetime)) %>% 
  mutate(seqe = rep(1:24,365))

tt<- profile %>% 
  group_by(seqe) %>% 
  mutate(newmw = mw/2) %>% ungroup() %>% 
  select(newmw)

# USE this
tt%>% slice(rep(1:n(), each = 2))
  data.frame(rep(tt,2))



startyear <- "2019-01-01 00:30:00"
endyear <- "2020-01-01 00:00:00"

test <-
  function(a,b){
  datetime = seq(as.POSIXct(a, format="%Y-%m-%d %H:%M:%S"), 
                 as.POSIXct(b, format="%Y-%m-%d %H:%M:%S"),
                 by="30 min")
  
  }
yt <- test(startyear,endyear)

# https://stackoverflow.com/questions/43688202/r-divide-intervals-in-hourly-slots
x = data.table(start=c("2017-04-18 18:05:00","2017-04-18 18:00:00", 
                       "2017-04-18 21:05:00", "2017-04-18 16:05:00"), 
               end=c("2017-04-18 19:05:00","2017-04-18 21:30:00",
                     "2017-04-18 22:00:00", "2017-04-18 16:10:00"))

find_slots2 <- function(a, b){
  a = fasttime::fastPOSIXct(a)
  b = fasttime::fastPOSIXct(b)
  slots = seq(a-data.table::minute(a)*60-data.table::second(a)*60,
              b-data.table::minute(b)*60-data.table::second(b)*60,
              "hour")
  
  hourseq = c(a, slots[-1], b)
  
  d = difftime(hourseq[-1], hourseq[-length(hourseq)], unit = 'min')
  
  list(slot = slots, Q = d)
}


x[, find_slots2(start, end), by = 1:nrow(x)][order(slot), .(Q = as.numeric(sum(Q))), by = slot]
