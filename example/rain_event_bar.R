library(MASS)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(cowplot)
library(scales)
library(tidyr)

#windows()

setwd("C:\\Users\\31334\\Desktop\\poyanglake\\result\\simulation\\new\\")
df <- read.table("rain_event_13th-18thSep\\rain_event_compare.txt", header = TRUE)
level_order <- c('ERA5','GPM','CN051','Exp2022','CTRL')

p <-ggplot(df, aes(x=1:5, y=Dif,fill=level_order)) +
  geom_bar(width = 0.5,stat='identity', position='dodge')+
  scale_x_continuous(labels = level_order,breaks = 1:5, 
               sec.axis = dup_axis( name=NULL,labels = NULL,breaks = derive() ) )    +
  scale_fill_manual(values=c("#00E5FF", "dodgerblue", "#FFFF00","firebrick1", "#00FF4D")) +

  scale_y_continuous(limits=c(0,30),breaks=seq(0,30,5),
               sec.axis = dup_axis( name=NULL,labels = NULL, breaks=derive() )) +
  
  labs(x = "Classification", y=expression('Sep. 13-18 total precipitation (mm)'),
                             color = NULL, fill = NULL)           +
  theme_bw()                                         +
  theme(legend.position = "none") +
  labs( tag = "(b)" ) +
  theme(panel.border = element_rect(color = "black",  fill = NA,  size = 1) ) +
  theme(legend.background=element_blank(),legend.key = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text.x = element_text(color = "black", size = 12), 
        axis.text.y = element_text( color = "black", size = 12) ) +  
  theme(axis.line = element_line(size = 0.8),axis.title=element_text(size=12)) 



  timedata=seq( as.Date("2022-08-01"), by=1, len=92 )
  x= timedata
  zonename=c('plothourcycle_timeseries_rain.txt','plothourcycle_rain.txt')

  data <- read.table(zonename[1], header = TRUE)
  box_2022 <- data$box_2022
  box_21yr <-data$box_21yr
  df_box  <- data.frame(x=x, y1=box_2022,   y2=box_21yr)

  p1 <- ggplot(data = df_box)    +  
        scale_x_date(labels = date_format("%m-%d"),
               sec.axis = dup_axis( name=NULL,labels = NULL,breaks = derive() ) )    +
        scale_y_continuous(limits=c(0,15),
               sec.axis = dup_axis( name=NULL,labels = NULL,breaks = derive() ))  +
        geom_line(aes(y=y1, x=x,colour = "Exp2022"), lwd=1.0)              +
        geom_line(aes(y=y2, x=x,colour = "CTRL"), lwd=1.0)                 +

        labs(x = "Date", y = 'Daily precipitaion (mm)',color = NULL, fill = NULL)           +
        theme_bw()                                                               +
        theme(legend.position = c(0.8, 0.82), legend.box = "horizontal" ) + 
        theme(legend.background=element_blank(),legend.key = element_blank()) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        theme(axis.text.x = element_text(color = "black", size = 12), 
              axis.text.y = element_text(color = "black", size = 12) ) +  
        theme(axis.line = element_line(size = 0.8),axis.title=element_text(size=12)) +
        theme(text=element_text(size=12),legend.text=element_text(size=12)) +
        labs( tag = "(a)" ) +
        scale_colour_manual(breaks = c("Exp2022", "CTRL"), 
                      labels = c(Exp2022 = "Exp2022", CTRL = "CTRL"),
                      values = c(Exp2022 = "firebrick1", CTRL = "dodgerblue")) +
        guides(color = guide_legend(nrow = 3))

pdf(file="rain_compare.pdf",width=9, height=4)
    plot_grid(p1,p,ncol=2,nrow=1)
dev.off()