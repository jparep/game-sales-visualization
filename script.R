library(tidyverse)
library(ggthemes)
library(ggplot2)
#library(reshape2)

setwd("C:/Users/jparep/Documents/proj/r/game-sales-visualization")

games <- read.csv("vgsales.csv", stringsAsFactors = FALSE)
