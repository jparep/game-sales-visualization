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
