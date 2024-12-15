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


games_sales10 <-games %>%
  group_by(Name) %>%
  summarise(sum_global_sales = sum(Global_Sales),.groups = 'drop') %>%
  arrange(desc(sum_global_sales))
games_totalsales <- head(games_sales10,10)

options(repr.plot.width = 16, repr.plot.height = 8)
ggplot(data= games_totalsales, aes(x= Name, y=sum_global_sales)) +
  geom_bar(stat = "identity",  aes(x= Name, y=sum_global_sales,fill=Name))+
  ggtitle("Top-10 Games by Sales") +
  xlab("Games") +
  ylab("in millions") +
  theme_stata()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="none")


genre_sales <-games %>%
  group_by(Genre) %>%
  summarise(sum_global_sales = sum(Global_Sales),.groups = 'drop') %>%
  arrange(desc(sum_global_sales))%>%
  mutate(percent = sum_global_sales/sum(sum_global_sales)*100)

options(repr.plot.width = 18, repr.plot.height = 10)
ggplot(data= genre_sales, aes(x= "",y=percent,fill = Genre))+
  geom_bar(stat="identity", width=1, color="white")+
  coord_polar("y", start=0)+
  ggtitle("Genre by % Global Sales") +
  xlab("") +
  ylab("") +
  theme_stata()+
  theme(legend.position="right")+
  geom_text(aes(label = paste0(round(percent),"%")), position = position_stack(vjust = 0.5),color = "black",size=5)


publisher_sales <-games %>%
  group_by(Publisher) %>%
  summarise(sum_global_sales = sum(Global_Sales),.groups = 'drop') %>%
  arrange(desc(sum_global_sales))
publisher_sales20 <- head(publisher_sales,20)

options(repr.plot.width = 16, repr.plot.height = 8)
ggplot(data= publisher_sales20, aes(x= Publisher, y=sum_global_sales)) +
  geom_bar(stat = "identity",  aes(x= Publisher, y=sum_global_sales,fill=Publisher))+
  coord_flip()+
  ggtitle("Top-20 Publisher by Sales") +
  xlab("Publishers") +
  ylab("in millions") +
  geom_text(aes(label=Publisher), vjust=0.5,hjust=0, color="black",
            position = position_dodge(1), size=4)+
  theme_stata()+
  theme(legend.position="none",axis.text.y=element_blank())


platform_game <-games %>%
  group_by(Year,Platform) %>%
  summarise(count_name = length(unique(Name)),.groups = 'drop') %>%
  arrange(desc(Year))

options(repr.plot.width = 16, repr.plot.height = 8)
ggplot(platform_game, aes(fill=Platform, y=count_name, x=Year)) + 
  geom_bar(position="stack", stat="identity")+
  ggtitle("Platforms Distribution by Yearly Number of Published Games") +
  xlab("Years") +
  ylab("Number of Published Games") +
  theme_stata()+
  theme(legend.position="right",axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

platform_sales <-games %>%
  group_by(Platform) %>%
  summarise(GlobalSales = sum(Global_Sales),
            NA_Sales = sum(NA_Sales),
            EU_Sales = sum(EU_Sales),
            JP_Sales = sum(JP_Sales),.groups = 'drop') %>%
  arrange(desc(GlobalSales))
platform_sales11 <- head(platform_sales,11)
platform_sales11 = melt(platform_sales11)
names(platform_sales11) = c('Platform','SaleDistrict','Sales')

options(repr.plot.width = 16, repr.plot.height = 8)
ggplot(data=platform_sales11,aes(x = SaleDistrict,y = Sales, fill=Platform))+
  geom_bar(stat='identity', position='dodge',colour='black' )+
  ggtitle("Popularity of Platforms by Sales District") +
  xlab("Sales District") +
  ylab("in millions") +
  theme_stata()+
  theme(legend.position="right")


publisher_count <-games %>%
  group_by(Publisher) %>%
  summarise(count_name = length(unique(Name)),.groups = 'drop') %>%
  arrange(desc(count_name)) %>%
  select(Publisher)%>%
  head(6)
publisher_count6 <-as.vector(publisher_count$Publisher)

publisher_genre<- games %>%
  filter(Publisher %in% publisher_count6)%>%
  group_by(Publisher,Genre) %>%
  summarise(count_name = length(unique(Name)),.groups = 'drop') %>%
  arrange(desc(count_name))

options(repr.plot.width = 16, repr.plot.height = 8)
ggplot(data=publisher_genre,aes(x = Publisher,y = count_name, fill=Genre))+
  geom_bar(stat='identity', position='dodge',colour='black' )+
  ggtitle("Genre Distribution by Publishers") +
  xlab("Publishers") +
  ylab("Number of Published Games") +
  theme_stata()+
  theme(legend.position="bottom")

publisher_count <-games %>%
  group_by(Publisher) %>%
  summarise(GlobalSales = sum(Global_Sales),count_game = length(unique(Name)),.groups = 'drop') %>%
  arrange(desc(count_game)) %>%
  select(Publisher)%>%
  head(5)
publisher_count20 <-as.vector(publisher_count$Publisher)

publisher_bubble<- games %>%
  filter(Publisher %in% publisher_count20)%>%
  group_by(Year,Publisher) %>%
  summarise(GlobalSales = sum(Global_Sales),count_game = length(unique(Name)),.groups = 'drop') %>%
  arrange(desc(Year))


options(repr.plot.width = 16, repr.plot.height = 8)
ggplot(publisher_bubble,aes(x=Year, y=GlobalSales, size=count_game, fill=Publisher)) +
  geom_point(alpha=0.5, shape=21, color="black") +
  scale_size(range = c(.1, 24), name="Number of Games") +
  theme_stata() +
  ggtitle("Top-5 Publisher Distribution by Yearly Number of Game and Sales") +
  ylab("in millions") +
  xlab("Year")+
  theme(legend.position="right",axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))