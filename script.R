#update.packages(ask = FALSE)
#install.packages("tidyverse")
#install.packages("ggthemes")
#install.packages("ggplot2")

library(tidyverse)
library(ggthemes)
library(ggplot2)
#library(reshape2)

setwd("C:/Users/jparep/Documents/proj/r/game-sales-visualization")

games <- read.csv("vgsales.csv", stringsAsFactors = FALSE)
head(games)

str(games)

summary(games)

unique(games$Year)

games <- games[games$Year!='N/A',]
games$Year <- factor(games$Year)

games <- games[,2:11]

head(games)

sumofsales <- games %>%
  group_by(Year) %>%
  summarise(sum_global_sales = sum(Global_Sales),sum_others_sales = sum(Other_Sales),
            sum_jb_sales = sum(JP_Sales),sum_eu_sales = sum(EU_Sales),
            sum_na_sales = sum(NA_Sales),.groups = 'drop')

colors <- c("Global Sales"="red", "North America Sales"="blue", "Europe Sales"="green", "Japan Sales"="orange",
            "The Rest of the World"="yellow")
options(repr.plot.width = 16, repr.plot.height = 8)
ggplot(data=sumofsales, aes(x= Year)) +
  geom_line(aes(y= sum_global_sales,group=1,color="Global Sales"),linetype = "dashed")+
  geom_line(aes(y= sum_na_sales,group=1,color="North America Sales"),linetype = "dashed")+
  geom_line(aes(y= sum_eu_sales,group=1,color="Europe Sales"),linetype = "dashed")+
  geom_line(aes(y= sum_jb_sales,group=1,color="Japan Sales"),linetype = "dashed")+
  geom_line(aes(y= sum_others_sales,group=1,color="The Rest of the World"),linetype = "dashed")+
  geom_point(aes(y= sum_global_sales)) +
  geom_point(aes(y= sum_na_sales)) +
  geom_point(aes(y= sum_eu_sales)) +
  geom_point(aes(y= sum_jb_sales)) +
  geom_point(aes(y= sum_others_sales)) +
  scale_color_manual(name="Sales",values = colors)+
  ggtitle("Sum of Global Sales by Year") +
  xlab("Years") +
  ylab("in millions") +
  theme_stata()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),legend.position="top")

year_count<- games %>%
  group_by(Year) %>%
  summarise(count_year = n())
tail(year_count)

games <- games[games$Year!='2017'& games$Year!='2020',]
head(games)
