#plot timeseries and diurnal cycle for rain
library(MASS)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(cowplot)
library(scales)
library(tidyr)

windows()

setwd("C:\\Users\\31334\\Desktop\\poyanglake\\result\\simulation\\new\\")
timedata=seq( as.Date("2022-08-01"), by=1, len=92 )
x= timedata
x1=seq(0,23)

lab_tag=c("(a)","(b)","(c)","(d)","(e)","(f)")
zonename=c('plothourcycle_timeseries_rain.txt','plothourcycle_rain.txt')
plist=list()

data <- read.table(zonename[1], header = TRUE)
all_2022 <- data$all_2022
all_21yr <- data$all_21yr
lake_2022 <-data$lake_2022
lake_21yr <- data$lake_21yr
box_2022 <- data$box_2022
box_21yr <-data$box_21yr
  
df_all  <- data.frame(x=x, y1=all_2022,   y2=all_21yr)
df_lake <- data.frame(x=x, y1=lake_2022,  y2=lake_21yr)
df_box  <- data.frame(x=x, y1=box_2022,   y2=box_21yr)
  
plist[[1]] <- ggplot(data = df_lake)    +  
    scale_x_date(labels = date_format("%m-%d"),
                 sec.axis = dup_axis( name=NULL,labels = NULL,breaks = derive() ) )    +
    scale_y_continuous(limits=c(0,15),sec.axis = dup_axis( name=NULL,labels = NULL,breaks = derive() ))  +
    geom_line(aes(y=y1, x=x,colour = "Exp2022"), lwd=1.0)              +
    geom_line(aes(y=y2, x=x,colour = "CTRL"), lwd=1.0)                 +
    
    labs(x = "", y = "Precipitation (mm/d)",color = NULL, fill = NULL)           +
    theme_bw()                                                                   +
    theme(legend.position = c(0.8, 0.82), legend.box = "horizontal" ) + 
    theme(legend.background=element_blank(),legend.key = element_blank()) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(axis.text.x = element_text(color = "black", size = 12), 
          axis.text.y = element_text( color = "black", size = 12) ) +  
    theme(axis.line = element_line(size = 0.8),axis.title=element_text(size=12)) +
    theme(text=element_text(size=10),legend.text=element_text(size=10)) +
    labs( tag = lab_tag[1] ) +
    scale_colour_manual(breaks = c("Exp2022", "CTRL"), 
                        labels = c(Exp2022 = "Exp2022", CTRL = "CTRL"),
                        values = c(Exp2022 = "firebrick1", CTRL = "dodgerblue")) +
    guides(color = guide_legend(nrow = 3))
  
  plist[[2]] <- ggplot(data = df_box)    +  
    scale_x_date(labels = date_format("%m-%d"),
                 sec.axis = dup_axis( name=NULL,labels = NULL,breaks = derive() ) )    +
    scale_y_continuous(limits=c(0,15),sec.axis = dup_axis( name=NULL,labels = NULL,breaks = derive() ))  +
    geom_line(aes(y=y1, x=x,colour = "Exp2022"), lwd=1.0)              +
    geom_line(aes(y=y2, x=x,colour = "CTRL"), lwd=1.0)                 +
    
    labs(x = "", y = "",color = NULL, fill = NULL)           +
    theme_bw()                                                                   +
    theme(legend.position = c(0.8, 0.82), legend.box = "horizontal" ) + 
    theme(legend.background=element_blank(),legend.key = element_blank()) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(axis.text.x = element_text(color = "black", size = 12), 
          axis.text.y = element_text( color = "black", size = 12) ) +  
    theme(axis.line = element_line(size = 0.8),axis.title=element_text(size=12)) +
    theme(text=element_text(size=10),legend.text=element_text(size=10)) +
    labs( tag = lab_tag[2] ) +
    scale_colour_manual(breaks = c("Exp2022", "CTRL"), 
                        labels = c(Exp2022 = "Exp2022", CTRL = "CTRL"),
                        values = c(Exp2022 = "firebrick1", CTRL = "dodgerblue")) +
    guides(color = guide_legend(nrow = 3))
  
  plist[[3]] <- ggplot(data = df_all)    +  
    scale_x_date(labels = date_format("%m-%d"),
                 sec.axis = dup_axis( name=NULL,labels = NULL,breaks = derive() ) )    +
    scale_y_continuous(sec.axis = dup_axis( name=NULL,labels = NULL,breaks = derive() ))  +
    geom_line(aes(y=y1, x=x,colour = "Exp2022"), lwd=1.0)              +
    geom_line(aes(y=y2, x=x,colour = "CTRL"), lwd=1.0)                 +
    
    labs(x = "", y = "",color = NULL, fill = NULL)           +
    theme_bw()                                                                   +
    theme(legend.position = c(0.8, 0.82), legend.box = "horizontal" ) + 
    theme(legend.background=element_blank(),legend.key = element_blank()) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(axis.text.x = element_text(color = "black", size = 12), 
          axis.text.y = element_text( color = "black", size = 12) ) +  
    theme(axis.line = element_line(size = 0.8),axis.title=element_text(size=12)) +
    theme(text=element_text(size=10),legend.text=element_text(size=10)) +
    labs( tag = lab_tag[3] ) +
    scale_colour_manual(breaks = c("Exp2022", "CTRL"), 
                        labels = c(Exp2022 = "Exp2022", CTRL = "CTRL"),
                        values = c(Exp2022 = "firebrick1", CTRL = "dodgerblue")) +
    guides(color = guide_legend(nrow = 3))


##plot diural cycle
#*  *  *  * 
data1 <- read.table(zonename[2], header = TRUE)
all_2022  <- data1$all_2022
all_21yr  <- data1$all_21yr
lake_2022 <- data1$lake_2022
lake_21yr <- data1$lake_21yr
box_2022  <- data1$box_2022
box_21yr  <- data1$box_21yr
  
df_all  <- data.frame(x=x1, y1=all_2022,   y2=all_21yr)
df_lake <- data.frame(x=x1, y1=lake_2022,  y2=lake_21yr)
df_box  <- data.frame(x=x1, y1=box_2022,   y2=box_21yr)
  
  plist[[4]] <- ggplot(data = df_lake)    +  
    scale_x_continuous( sec.axis = dup_axis( name=NULL,labels = NULL,breaks = derive() ) )    +
    scale_y_continuous(sec.axis = dup_axis( name=NULL,labels = NULL,breaks = derive() ))  +
    geom_line(aes(y=y1, x=x,colour = "Exp2022"), lwd=1.0)              +
    geom_line(aes(y=y2, x=x,colour = "CTRL"), lwd=1.0)                 +
    
    labs(x = "", y =  "Precipitation (mm/h)",color = NULL, fill = NULL)           +
    theme_bw()                                                                   +
    theme(legend.position = c(0.2, 0.8), legend.box = "horizontal" ) + 
    theme(legend.background=element_blank(),legend.key = element_blank()) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(axis.text.x = element_text(color = "black", size = 12), 
          axis.text.y = element_text( color = "black", size = 12) ) +  
    theme(axis.line = element_line(size = 0.8),axis.title=element_text(size=12)) +
    theme(text=element_text(size=10),legend.text=element_text(size=10)) +
    labs( tag = lab_tag[4] ) +
    scale_colour_manual(breaks = c("Exp2022", "CTRL"), 
                        labels = c(Exp2022 = "Exp2022", CTRL = "CTRL"),
                        values = c(Exp2022 = "firebrick1", CTRL = "dodgerblue")) +
    guides(color = guide_legend(nrow = 3))
  
  plist[[5]] <- ggplot(data = df_box)    +  
    scale_x_continuous(sec.axis = dup_axis( name=NULL,labels = NULL,breaks = derive() ) )    +
    scale_y_continuous(sec.axis = dup_axis( name=NULL,labels = NULL,breaks = derive() ))  +
    geom_line(aes(y=y1, x=x,colour = "Exp2022"), lwd=1.0)              +
    geom_line(aes(y=y2, x=x,colour = "CTRL"), lwd=1.0)                 +
    
    labs(x = "", y = "",color = NULL, fill = NULL)           +
    theme_bw()                                                                   +
    theme(legend.position = c(0.2, 0.8), legend.box = "horizontal" ) + 
    theme(legend.background=element_blank(),legend.key = element_blank()) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(axis.text.x = element_text(color = "black", size = 12), 
          axis.text.y = element_text( color = "black", size = 12) ) +  
    theme(axis.line = element_line(size = 0.8),axis.title=element_text(size=12)) +
    theme(text=element_text(size=10),legend.text=element_text(size=10)) +
    labs( tag = lab_tag[5] ) +
    scale_colour_manual(breaks = c("Exp2022", "CTRL"), 
                        labels = c(Exp2022 = "Exp2022", CTRL = "CTRL"),
                        values = c(Exp2022 = "firebrick1", CTRL = "dodgerblue")) +
    guides(color = guide_legend(nrow = 3))
  
  plist[[6]] <- ggplot(data = df_all)    +  
    scale_x_continuous(sec.axis = dup_axis( name=NULL,labels = NULL,breaks = derive() ) )    +
    scale_y_continuous(sec.axis = dup_axis( name=NULL,labels = NULL,breaks = derive() ))  +
    geom_line(aes(y=y1, x=x,colour = "Exp2022"), lwd=1.0)              +
    geom_line(aes(y=y2, x=x,colour = "CTRL"), lwd=1.0)                 +
    
    labs(x = "", y = "",color = NULL, fill = NULL)           +
    theme_bw()                                                                   +
    theme(legend.position = c(0.2, 0.85), legend.box = "horizontal" ) + 
    theme(legend.background=element_blank(),legend.key = element_blank()) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(axis.text.x = element_text(color = "black", size = 12), 
          axis.text.y = element_text( color = "black", size = 12) ) +  
    theme(axis.line = element_line(size = 0.8),axis.title=element_text(size=12)) +
    theme(text=element_text(size=10),legend.text=element_text(size=10)) +
    labs( tag = lab_tag[6] ) +
    scale_colour_manual(breaks = c("Exp2022", "CTRL"), 
                        labels = c(Exp2022 = "Exp2022", CTRL = "CTRL"),
                        values = c(Exp2022 = "firebrick1", CTRL = "dodgerblue")) +
    guides(color = guide_legend(nrow = 2))


for (i in 4:6) {
  plist[[i]] <- plist[[i]]+ theme(legend.position="none")
}


for (i in 1:3) {
  plist[[i]] <- plist[[i]]+ labs( x = "Date",color = NULL, fill = NULL)
}

for (i in 4:6) {
  plist[[i]] <- plist[[i]]+ labs( x = "Hour (LST)",color = NULL, fill = NULL)
}

for (i in 1:6) {
  plist[[i]] <- plist[[i]] + theme(plot.margin = unit(c(0.2, 0.5, 0.2, 0.1), "cm"))
}

#pdf(file="daily_and_hourly_rain.pdf",width=10, height=6)

#plot_grid(plist[[1]],plist[[2]],plist[[3]],plist[[4]],plist[[5]],plist[[6]],
#          ncol=3,nrow=2)
#dev.off()

plist[[2]]  
#plot_grid(plist[[1]],plist[[2]],ncol=2,nrow=1)
