library(readxl)
library(scales)
library(cowplot)
library(grid)
library(gridExtra)
library(tidyverse)
library(plotly)
library(writexl) # for reading excel
library(openxlsx)


#Program function

adjGenProFr1HrTo30Min <-
  function(path,startyear,endyear){
    profile <- read_excel(path, #Input from user
                          sheet = "Generation profile",
                          range = "B7:LYB100",
                          col_names = c("profileType", "in_cap_mw","mwh",1:8760)) %>% 
      select(-"in_cap_mw", -"mwh") %>% 
      pivot_longer(-"profileType", names_to = "hourly", values_to = "mw") %>% 
      group_by(profileType) %>%
      mutate(seqe = rep(1:24,365)) %>% 
      ungroup()
    
    datetime <- seq(as.POSIXct(startyear, #Input from user 
                               format="%Y-%m-%d %H:%M:%S", tz="UTC"), 
                    as.POSIXct(endyear, #Input from user
                               format="%Y-%m-%d %H:%M:%S", tz = "UTC"),
                 by="30 min")
    
    
    temp1<-
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
      # filter(profileType == "VSPP Solar C") %>%
      # filter(profileType %in% c("VSPP Solar C", "VSPP Solar NE")) %>% 
      mutate(check = seq2 %% 2 == 0, #Check if it is odd number
             newmw2 = replace(mw, check == T, NA), #replace mw when check == true with NA
             newmw2 = if_else(check == T, # Calcuate mean of newmw2 when check is TRUE
                              slider::slide_dbl(newmw2, 
                                                ~mean(.x, na.rm = T), #Calculate mean by excluding the NA value 
                                                .before = 1, 
                                                .after = 1),
                              newmw2)
                              ) %>% 
      ungroup()
    
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
    
    energy <- data %>% 
              group_by(profileType) %>% 
              mutate(energy = (sum(mw)/2)) %>% 
              ungroup()
    
    data2 <-
      data %>% 
      mutate(time = paste0(hour(datetime),":",minute(datetime)),
             month = month(datetime, label = TRUE)) %>% 
      select(!c(datetime, dailyTimeIndex))  %>% 
      pivot_wider( names_from = "time", values_from = "mw")  
    
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
    
    for (fileName in unique(data2$profileType)) {
      filename <- paste0("processdata/", fileName, ".xlsx")
      write.xlsx(split(data2, data2$month),
               file = filename,
               firstRow = TRUE,
               colWidths = "auto",
               headerStyle = hs)
    }
    
  
  return(list(data=data,energy=energy))
  }


data <- adjGenProFr1HrTo30Min(path, startyear,endyear)

