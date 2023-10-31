library(readxl)
library(scales)
library(cowplot)
library(grid)
library(gridExtra)
library(tidyverse)
library(plotly)
library(writexl)

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

# USE this
tt%>% slice(rep(1:n(), each = 2))
  # data.frame(rep(tt,2))


path <- "rawdata/GenerationProfile_PF.xlsx"
startyear <- "2019-01-01 00:00:00"
endyear <- "2020-12-31 23:30:00"

test <-
  function(path,startyear,endyear){
    profile <- read_excel(path, #Input from user
                          sheet = "Generation profile",
                          range = "B7:LYB100",
                          col_names = c("profileType", "in_cap_mw","mwh",1:8760)) %>% 
      select(-"in_cap_mw", -"mwh") %>% 
      pivot_longer(-"profileType", names_to = "hourly", values_to = "mw") %>% 
      group_by(profileType) %>%
      mutate(seqe = rep(1:24,365))
    
    datetime <- seq(as.POSIXct(startyear, #Input from user 
                               format="%Y-%m-%d %H:%M:%S", tz="UTC"), 
                    as.POSIXct(endyear, #Input from user
                               format="%Y-%m-%d %H:%M:%S", tz = "UTC"),
                 by="30 min")
    
    tt<-
      profile %>% 
      # group_by(seqe) %>% 
      # mutate(newmw = mw/2) %>% 
      # ungroup() %>% 
      slice(rep(1:n(), each = 2)) %>%
      group_by(profileType) %>%
      mutate(datetime,seq2 = rep(1:48,365)) %>% 
      # mutate(seq1 = rep(1:24, 0.5, 365))
      # filter(profileType == "VSPP Solar C") %>%
      # filter(profileType %in% c("VSPP Solar C", "VSPP Solar NE")) %>% 
      mutate(check = seq2 %% 2 == 0) %>% #Check if it is odd number
      mutate(newmw2 = if_else(check == T, 
                              slider::slide_dbl(mw, mean, .before = 1, .after = 1),
                              mw))
    
    energy <- tt %>% group_by(profileType) %>% mutate(energy = (sum(newmw2)/2))
              
    
    # a2 <- ggplotly(
    #   tt %>% 
    #   ggplot()+
    #   geom_line(aes(x=datetime, y = newmw2))
    # )
    
    write_xlsx(split(tt, tt$profileType), path = "processdata/30-min_adjustedGenProfile.xlsx")
  
}
yt <- test(path, startyear,endyear)

