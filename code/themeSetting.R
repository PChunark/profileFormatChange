library(tidyverse)

text <- "Kanit"
ThemeLine <- 
  theme_bw() +
  theme(
    panel.border=element_rect(fill=NA),
    panel.grid.minor = element_line(color = NA), 
    #    axis.title=element_text(size=5),
    #    axis.text.x = element_text(hjust=1,size = 10, angle = 0),
    axis.line=element_line(colour="black"),
    panel.background=element_rect(fill = "white"),
    panel.grid.major.x=element_line(linetype="dashed",colour="grey",linewidth = 0.5),
    panel.grid.major.y = element_blank(),
    # panel.grid.major=element_blank(),
    strip.background=element_rect(fill="white", colour="white"),
    strip.text.x = element_text(family = text,size=10, colour = "black", angle = 0,face="bold"),
    axis.text.x=element_text(family = text, size = 10,angle=45, vjust=0.9, hjust=1, margin = unit(c(t = 0.3, r = 0, b = 0, l = 0), "cm")),
    axis.text.y=element_text(family = text, size = 10,margin = unit(c(t = 0, r = 0.3, b = 0, l = 0), "cm")),
    axis.title = element_text(family = text, size = 10),
    legend.text = element_text(family = text,size = 10),
    legend.title = element_text(family = text,size = 10),
    axis.ticks.length=unit(-0.15,"cm")
  )
