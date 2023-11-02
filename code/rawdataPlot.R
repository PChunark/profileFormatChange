library(readxl)
library(scales)
library(cowplot)
library(grid)
library(gridExtra)
library(tidyverse)
library(plotly)
library(writexl) # for reading excel
library(openxlsx)

# Plot raw data 1 hour profile
profile1 <- read_excel("rawdata/GenerationProfile_PF.xlsx",
                       sheet = "Generation profile",
                       range = "B7:LYB100",
                       col_names = c("vspp", "in_cap_mw","mwh",1:8760)) %>% 
  select(-"in_cap_mw", -"mwh") %>% 
  pivot_longer(-"vspp", names_to = "hourly", values_to = "mw") %>% 
  group_by(vspp) %>% 
  mutate(datetime = seq(as.POSIXct("2019-01-01 00:00:00", format="%Y-%m-%d %H:%M:%S"),
                        as.POSIXct("2019-12-31 23:30:00", format="%Y-%m-%d %H:%M:%S"),
                        by="1 hour"),
         year = year(datetime),
         month = month(datetime),
         day = day(datetime),
         hour = hour(datetime)) %>%
  mutate(seqe = rep(1:24,365)) %>%  #daily profile
  filter(vspp == "VSPP Solar C")

a1<-ggplotly(profile1 %>% ggplot() + geom_line(aes(x=datetime, y = mw)))

tt<- profile1 %>% 
  group_by(seqe) %>% 
  mutate(newmw = mw/2) %>% ungroup() %>% 
  select(newmw)
