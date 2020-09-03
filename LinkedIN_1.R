###################### import the data set and attach it
dat<-read.csv(file.choose())
summary(dat)
attach(dat)
###################### install necessary packages
install.packages('dplyr')
install.packages('ggplot2')
library(dplyr)
library(ggplot2)
#################### take a look at your data
str(dat)
names(dat)
dim(dat)
###################### filter out your data for non-existent values of missing categories
dat<-filter(dat, SEX!="", Year!= " ", ALIGN!="")%>%
  filter(!is.na(APPEARANCES),APPEARANCES>100)%>%
  arrange(desc(APPEARANCES)) ##arranging data in decreasing order of appearances


######################## THe following represents a graph of no. of appearances of marvel characters by years
dat_count <- count(dat,Year)
glimpse(dat_count)
shrey2 <- ggplot(data = dat_count)+
  geom_line(aes(Year,n),color='green')
  geom_point(color="red")
shrey2 %>% ggplotly()

library(plotly)
shrey <- ggplot(data=dat_count,aes(n,fill=Year%>%as.factor()))+
  geom_histogram(bins=15)
shrey%>%ggplotly()
######################### The next graph : we will characterize the characters and create multiple plots
dat_count <- count(dat,Year,ALIGN)                          ## we have added align variable here
glimpse(dat_count)

sk<-ggplot(data = dat_count, aes(Year,n))+
  geom_line(color='dark green')+
  geom_point(color="red")+
  facet_wrap(~ALIGN)
ggsave("sk.png",sk)

########################## In the next graph we are adding a classifiction by gender also
dat_count <- count(dat,Year,ALIGN,SEX)                          ## we have added align variable here
glimpse(dat_count)
## you may store your plots in a variable and plot your graph later
g <- ggplot(data = dat_count, aes(Year,n))+
  geom_point(color="red")+
  geom_line(color='dark green')+
  facet_wrap(~ALIGN + SEX,nrow=2)
g
########################## to interpret y-axis as a seperate axis for every graph : use scales= "free_y"

g <- ggplot(data = dat_count, aes(Year,n))+
  geom_point(color="red")+
  geom_line(color='dark green')+
  facet_wrap(~ALIGN + SEX,nrow=2,scales="free_y",nrow=2)
g
########################## to make the graphs a bit more dynamic, WE can alos use the FACET_GRID
dat_count <- count(dat,Year,ALIGN,SEX)                          
glimpse(dat_count)
g <- ggplot(data = dat_count, aes(Year,n))+
  geom_point(color="red")+
  geom_line(color='dark green')+
  facet_grid(SEX ~ ALIGN, space="free", scales = "free") ##### space and scales used here work only with FACET_GRId
g

########################### plotting chracters based on colors

ggplot(data =dat_count, aes(Year, n, color = SEX)) +
  geom_line(size = 1) +
  geom_point() + 
  labs(title = "New Marvel characters by gender")+
  facet_wrap(~ALIGN)



