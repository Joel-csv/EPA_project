####Physics department Sensor
setwd("C:\\Users\\dell latitude 7400\\Desktop\\sets")
getwd()

library(tidyverse);library(lubridate); library(plotly); library(car);library(hexbin)
library(RColorBrewer); library(scales); library(gridExtra); library(openair)

df = read.csv("physics.csv")
data = read.csv("weather.csv")
object = read.csv("empire.csv")
pm25_ug = read.csv("pm25ug_new.csv") 
emp.25 = read.csv("emp.25.csv") 

df$date = dmy_hm(df$date)
str(df)

ggplot(df, aes(x= date, y = nox_UG)) +
  geom_line(color = "blue")
