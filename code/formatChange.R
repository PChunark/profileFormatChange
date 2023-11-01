library(readxl)
library(scales)
library(cowplot)
library(grid)
library(gridExtra)
library(tidyverse)
library(plotly)
library(writexl) # for reading excel
library(openxlsx)

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
endyear <- "2019-12-31 23:30:00"

adjProfFrm1HrTo30Min <-
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
    
    # temp1<-
      profile %>% 
      # group_by(seqe) %>% 
      # mutate(newmw = mw/2) %>% 
      # ungroup() %>% 
      slice(rep(1:n(), each = 2)) %>%
      group_by(profileType) %>%
      mutate(datetime,
             seq2 = rep(1:48,365),
             day = wday(datetime, 
                         label = TRUE, 
                         abbr = FALSE),
             weekType = ifelse(wday(datetime) %in% c(1,7), "weekend", "weekday"),
             month = month(datetime),
             date = day(datetime)) %>% 
      # mutate(seq1 = rep(1:24, 0.5, 365))
      filter(profileType == "VSPP Solar C",
             month == 1,
             date == 1) %>%
      # filter(profileType %in% c("VSPP Solar C", "VSPP Solar NE")) %>% 
      mutate(check = seq2 %% 2 == 0) %>% #Check if it is odd number
      replace()
        newmw2 = if_else(check == T, 
                              slider::slide_dbl(mw, ~mean(.x), .before = 1, .after = 1),
                              mw))
    data <-
      temp1 %>% 
      select(profileType, 
             datetime, 
             dailyTimeIndex = seq2, 
             month, 
             date,
             day,
             weekType, 
             mw = newmw2)
    
    energy <- temp1 %>% group_by(profileType) %>% mutate(energy = (sum(newmw2)/2))
              
    
    # a2 <- ggplotly(
    #   tt %>% 
    #   ggplot()+
    #   geom_line(aes(x=datetime, y = newmw2))
    # )
    
    hs <- 
      createStyle(
      textDecoration = "BOLD", 
      fontColour = "black", 
      fontSize = 12,
      fontName = "Tahoma", 
      fgFill = "lightgrey"
      )
      
    write.xlsx(split(data, data$profileType), 
               file = "processdata/30-min_adjustedGenProfile.xlsx",
               firstRow = TRUE,
               colWidths = "auto",
               headerStyle = hs)
  
}
yt <- test(path, startyear,endyear)

toy_data <- tibble(
  var_missing = rnorm(mean = 10, sd = 3, n = 360),
  region = rep(c("A", "B", "C"), each = 2, times = 60),
  gender = rep(c("Male","Female"), times = 180),
  year = rep(2010:2014, each = 72),
  month = rep(1:12, each = 6, times = 5)
) %>%
  mutate(month = month(month, label = TRUE))

toy_data %>% 
  group_by(region, gender, month) %>% 
  mutate(
    var_missing = 
      if_else(
        is.na(var_missing),
        slider::slide_dbl(var_missing, mean, .before = 2, .after = -1),
        var_missing
      )
  ) %>% 
  ungroup()

slider::slide_dbl(mw, ~mean(.x), .before = 1, .after = 1)