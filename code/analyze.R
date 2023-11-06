data1<-
  data$data %>% 
  mutate(daydate = date(datetime),
         hour = hour(datetime),
         minute = minute(datetime),
         .after = datetime) %>% 
  mutate(season = if_else(month %in% c("3","4","5"), "Summer",
                          if_else(month %in% c("6","7","8","9","10"), "Rainy",
                                  if_else(month %in% c("11","12","1","2"), "Winter","Others"))),.after = "date") %>% 
  filter(str_detect(profileType, "New VSPP"))  
    unique(data1$profileType)


plot <- list()    
for (i in unique(data1$profileType)) {
    # name <- unique(data1$profileType)
    plot[[i]]<-
    data1 %>%
    mutate(dummy = "dummy") %>%   
    filter(profileType == i)  %>%
    ggplot()+
    geom_line(aes(x = datetime,
                  y = mw,
                  group = dummy,
                  color = season))+
    scale_y_continuous(limits = c(min(data1$mw), max(data1$mw))) +  
    ggtitle(i) +
    ThemeLine +
    labs(x = element_blank(),
         y = paste("Generation", "profile", "(MW)"))
}   
    