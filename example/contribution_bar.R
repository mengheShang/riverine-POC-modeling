library(tidyr)
library(reshape2)
library(cowplot)
library(ggplot2)

#windows()

setwd("C:\\Users\\31334\\Desktop\\poyanglake\\result\\simulation\\new\\")

lab_tag=c("(a)","(b)","(c)","(d)")
zonename=c('contribution_lakepixel.txt','contribution_boxmask.txt',
           'contribution_lakepixel_daynight.txt','contribution_boxmask_daynight.txt')
plist=list()
level_order <- c('SWdown','SWup','SH','LH','LWdown','LWup','G')
pixelname=c('Shrinking lake pixels','Sub-region pixels','Shrinking lake pixels','Sub-region pixels')

for (i in 1:2) {
  
  df <- read.table(zonename[i], header = TRUE)
  p <-ggplot(df, aes(x=Var, y=Dif)) +
      geom_bar(width = 0.5,stat='identity', position='dodge',color='black',fill='gray')+
      scale_x_discrete(limits = level_order) 
  plist[[i]] = p
}  

for (i in 3:4 ) {
  
  dfnew <- read.table(zonename[i],header = TRUE)
  dfnew0 <- melt(dfnew, id.vars = 'Var')
  head(dfnew0)
  p <-ggplot(dfnew0) +
      geom_bar(aes(x=Var, y=value, fill=variable),
               colour='black',width=0.75, stat='identity', position='dodge')   +
      scale_x_discrete(limits = level_order) 
  plist[[i]] = p
}

for (i in 1:4 ) {
  
  plist[[i]]<-  plist[[i]] + 
    geom_hline(yintercept=0) +
    labs(x = "", y = expression('Surface heat fluxes (W m'^-2*')'),color = NULL, fill = NULL)           +
    theme_bw()                                         +
    theme(panel.border = element_rect(color = "black",  fill = NA,  size = 1) ) +
    theme(legend.position = c(0.12, 0.88), legend.direction= "vertical" ) + 
    theme(legend.background=element_blank(),legend.key = element_blank()) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(axis.text.x = element_text(color = "black", size = 10), 
          axis.text.y = element_text( color = "black", size = 10) ) +  
    theme(axis.line = element_line(size = 0.8),axis.title=element_text(size=12)) +
    theme(text=element_text(size=10),legend.text=element_text(size=10)) +
    labs( tag = lab_tag[i] )  +
    annotate("text", x = -Inf, y = -Inf, label = pixelname[i], vjust = -1, hjust = -0.2)
      
  
  
}

pdf(file="suface_heat_fluxes.pdf",width=12, height=6)

plot_grid(plist[[1]],plist[[2]],plist[[3]],plist[[4]],ncol=2,nrow=2)

dev.off()

