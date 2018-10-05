#####################################
#######      Exploration      #######
#####################################


#### Load needed libraries ####
library(lubridate)
library(plyr)
library(dplyr) 
library(ggplot2)
library(plotly)

# If a package is not recognized you'll first have to install the package.See example below
#install.packages("plotly")

#### Data preparation ####
# Load data
R1358 <- read.csv("AllData/R1358.csv", stringsAsFactors=FALSE)
R1635 <- read.csv("AllData/R1635.csv", stringsAsFactors=FALSE)
R1642 <- read.csv("AllData/R1642.csv", stringsAsFactors=FALSE)
R2071 <- read.csv("AllData/R2071.csv", stringsAsFactors=FALSE)
R2291 <- read.csv("AllData/R2291.csv", stringsAsFactors=FALSE)
R2296 <- read.csv("AllData/R2296.csv", stringsAsFactors=FALSE)
R2342 <- read.csv("AllData/R2342.csv", stringsAsFactors=FALSE)

# Join data in one dataframe
R1358$Time <- parse_date_time(R1358$Time, orders = "mdyHM") # date is written differently in this file
R1635$Time <- parse_date_time(R1635$Time, orders = "dmyHM")
R1642$Time <- parse_date_time(R1642$Time, orders = "dmyHM")
R2071$Time <- parse_date_time(R2071$Time, orders = "dmyHM")
R2291$Time <- parse_date_time(R2291$Time, orders = "dmyHM")
R2296$Time <- parse_date_time(R2296$Time, orders = "dmyHM")
R2342$Time <- parse_date_time(R2342$Time, orders = "dmyHM")

mylist <- list(R1358 = R1358, # make a list of all files
               R1635 = R1635, # specify the name of each dataframe
               R1642 = R1642,
               R2071 = R2071,
               R2291 = R2291,
               R2296 = R2296,
               R2342 = R2342)


dst <- ldply(mylist) # ldply converts a list (l) into a dataframe (d)
colnames(dst) <- c("ID", "Time", "Pressure")

# Add a depth variable

dst <- filter(dst, !is.na(Pressure)) # remove NA values
dst$Depth <- -dst$Pressure * 1.0094 # 1.03*10^3 * 9.8*10^-4 # P = Patm + Pfluid = r.g.h

# Change the class of the ID variable
dst$ID <- as.factor(dst$ID)



#### Plotting ####
ggplot(data = filter(dst, ID == "R1358")) + theme_bw() + # explore movements of 1 animal
  geom_path(aes(x = Time, y = Depth), size = 0.5)

ggplot(data = dst) + theme_bw() + # explore movements of all animals
  geom_path(aes(x = Time, y = Depth), size = 0.5) +
  facet_grid(ID~.)

lapply(unique(date(dst[dst$ID== "R1358",]$Time)), function(z){ # explore movements of 1 animal per day
  ggplot(data = dst[dst$ID== "R1358"& date(dst$Time)==z,]) +
    geom_point(aes(x = Time, y = Depth), size = 0.5) +
    geom_path(aes(x = Time, y = Depth), size = 0.5) + 
    theme_bw()
})

lapply(sort(unique(date(dst$Time))), function(z){ # explore movements of all animals per day
  ggplot(data = dst[date(dst$Time)==z,]) +
    geom_point(aes(x = Time, y = Depth, colour = ID), size = 0.5) +
    geom_path(aes(x = Time, y = Depth, colour = ID, group = ID), size = 0.5) +
    theme_bw() + ylim(max(dst$Depth, na.rm=T)-1, min(dst$Depth, na.rm=T)+1)
})

#### Interactive plotting ####
p <- plot_ly(dst, x = ~Time, y = ~Depth, color = ~ID) %>% # select parts of the plot manually
  add_lines()
p

p <- plot_ly(dst, x = ~Time, y = ~Depth, color = ~ID) %>% # select parts of the plot in the slider
  add_lines() %>%
  layout(
    title = "DST",
    xaxis = list(
      rangeslider = list(type = "date")),
    yaxis = list(
      title = "Depth"))
p

