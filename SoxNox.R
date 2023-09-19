setwd("C:\\Users\\dell latitude 7400\\Desktop\\sets")
getwd()


library(tidyverse);library(lubridate); library(plotly); library(car)
library(RColorBrewer); library(scales); library(gridExtra); library(openair)

#read data
data = read.csv("ENE022368_left - ENE022368_SOX_NOX_left.csv")

#change data types
data = data %>% 
  mutate(date = dmy_hm(date))
str(data)


#create fictitious ws/wd data
set.seed(123)

n = 43829

ws = rnorm(n, mean = 4.49, sd = 2.40)

ws = abs(ws) 
summary(ws)


wd = runif(n, min = 0, max = 360)

# Round the wind direction to the nearest integer
wd = round(wd)

# Ensure that the values are within the range [0, 360] by taking modulo 360
wd = wd %% 360

# Display the first few rows of the generated data
head(wd)

data$ws = ws
data$wd = wd
str(data)


###Search for and remove outliers from data
attach(data)

plot(sox_ug.m3_ENE02368)
plot(nox_ug.m3_ENE022368)

new_data = data %>% 
  filter(sox_ug.m3_ENE02368 >= 100)
dim(new_data)

summary(data)
summary(new_data)

new_data2 = new_data %>% 
  filter(nox_ug.m3_ENE022368 >= 40)
dim(new_data2)
summary(new_data2$nox_ug.m3_ENE022368)

##outliers have been removed 


####Graphs

#which day was nox/sox concentration very high
calendarPlot(new_data2, pollutant = "nox_ug.m3_ENE022368")
calendarPlot(new_data2, pollutant = "sox_ug.m3_ENE02368")

#add wind speed and direction to the plot
calendarPlot(new_data2, pollutant = "sox_ug.m3_ENE02368", annotate = "ws")
calendarPlot(new_data2, pollutant = "sox_ug.m3_ENE02368", annotate = "wd")
calendarPlot(new_data2, pollutant = "nox_ug.m3_ENE022368", annotate = "ws")
calendarPlot(new_data2, pollutant = "nox_ug.m3_ENE022368", annotate = "wd")


#summary plot
summaryPlot(new_data2, period = "months")
summaryPlot(new_data2, period = "months", avg.time = "hour", type = "density") #changes histogram to density plot


####ScatterPlots (works better when there are atmospheric variables)
scatterPlot(new_data2, x = "nox_ug.m3_ENE022368", y = "sox_ug.m3_ENE02368")
scatterPlot(new_data2, x = "nox_ug.m3_ENE022368", y = "ws", z = "wd")

#we can also specify type as season 
scatterPlot(new_df, x = "PM.2.5.ug.m.3.", y = "NOX.ug.m.3.", type = "season")
##we can have other specifications when we get the data

####Time variations
timeVariation(new_data2, pollutant = "nox_ug.m3_ENE022368")
timeVariation(new_data2, pollutant = "sox_ug.m3_ENE02368")
timeVariation(new_data2, pollutant = "ws")
timeVariation(new_data2, pollutant = "wd")

#we can also plot 2 or more pollutants on TV plots
timeVariation(new_data2, pollutant = c("nox_ug.m3_ENE022368", "sox_ug.m3_ENE02368"), normalise = T)
timeVariation(new_data2, pollutant = c("nox_ug.m3_ENE022368", "sox_ug.m3_ENE02368"), normalise = FALSE)

## difference in concentrations
timeVariation(new_data2, poll= c("nox_ug.m3_ENE022368", "sox_ug.m3_ENE02368"), difference = TRUE)

###subplots from the openair object
myplot <- timeVariation(new_data2, pollutant = c("nox_ug.m3_ENE022368",'sox_ug.m3_ENE02368'))
plot(myplot, subset = "day.hour") 

## plot quantiles and median
timeVariation(new_data2, stati="median", poll= c("nox_ug.m3_ENE022368") , col = "firebrick")
timeVariation(new_data2, stati="median", poll= c("sox_ug.m3_ENE02368") , col = "gold3")


###Trend Level
# trendLevel(new_df, pollutant = "PM.2.5.ug.m.3.", rotate.axis = c(360,0))
# trendLevel(new_df, pollutant = "NOX.ug.m.3.", statistic = mean,stat.args = list(na.rm = TRUE))
# trendLevel(new_df, pollutant = "SOX.ug.m.3.",
#            border = "white", statistic = "max",
#            breaks = c(17.13 , 552.50, 755.72 , 1147.25),
#            labels = c("low", "medium", "high", "very high"),
#            cols = c("forestgreen", "yellow","orange", "red"))
# 
# 
# ###Smooth trend [includes a smoothing line]
# smoothTrend(new_df, pollutant = "PM.2.5.ug.m.3.", avg.time ="month")

###Timeplot
timePlot(new_data2, pollutant = c("nox_ug.m3_ENE022368",'sox_ug.m3_ENE02368'))
timePlot(new_data2, pollutant = c("nox_ug.m3_ENE022368",'sox_ug.m3_ENE02368'),lwd = 2)
timePlot(new_data2, pollutant = c("nox_ug.m3_ENE022368",'sox_ug.m3_ENE02368'), group = TRUE)


###Time prop
timeProp(new_data2, pollutant = "nox_ug.m3_ENE022368", avg.time = "day", proportion = "wd")
timeProp(new_data2, pollutant = "sox_ug.m3_ENE02368", avg.time = "day", proportion = "wd")



# ###Thielsen Plot
# TheilSen(new_data2, pollutant = "nox_ug.m3_ENE022368")

###Correlation plot
corPlot(new_data2)

###Linear relation
linearRelation(new_data2, x = "nox_ug.m3_ENE022368", y = "wd", period = "hour", cols = "firebrick")
linearRelation(new_data2, x = "sox_ug.m3_ENE02368", y = "wd", period = "hour", cols = "firebrick")

###time series from ggplot
p = ggplot(new_data2, aes(x = date)) +
  geom_line(aes(y=nox_ug.m3_ENE022368, linetype = "solid",color= "#eeaeca"))

ggplotly(p)

p = ggplot(new_data2, aes(x = date)) +
  geom_line(aes(y=sox_ug.m3_ENE02368, linetype = "solid",color= "#94e9ad"))

ggplotly(p)


p <- ggplot(new_data2, aes(x = date, y = sox_ug.m3_ENE02368)) +
  geom_line(aes(color = sox_ug.m3_ENE02368)) +
  scale_color_gradient(low = "#94e9ad", high = "red")  # Adjust color scale as needed


# Extract the hour from the date and calculate the hourly average
hourly_avg_sox <- new_data2 %>%
  mutate(hour = hour(date)) %>%
  group_by(hour) %>%
  summarise(avg_sox_ug = mean(sox_ug.m3_ENE02368))

# Create the plot
ggplot(hourly_avg_sox, aes(x = hour, y = avg_sox_ug)) +
  geom_line(aes(color = avg_sox_ug)) +
  scale_color_gradient(low = "red", high = "#34ebb1") +
  labs(title = "Hourly Average SOX_ug.m3_ENE02368", x = "Hour of Day", y = "Average SOX_ug.m3_ENE02368")


hourly_avg_nox <- new_data2 %>%
  mutate(hour = hour(date)) %>%
  group_by(hour) %>%
  summarise(avg_nox_ug = mean(nox_ug.m3_ENE022368))

# Create the plot
ggplot(hourly_avg_nox, aes(x = hour, y = avg_nox_ug)) +
  geom_line(aes(color = avg_nox_ug)) +
  scale_color_gradient(low = "red", high = "#38a832") +
  labs(title = "Hourly Average NOX_ug.m3_ENE02368", x = "Hour of Day", y = "Average SOX_ug.m3_ENE02368")


# Aggregate data by week and plot weekly time series
weekly_avg <- new_data2 %>%
  group_by(week = lubridate::week(date)) %>%
  summarise(avg_sox_ug = mean(sox_ug.m3_ENE02368))

ggplot(weekly_avg, aes(x = week, y = avg_sox_ug)) +
  geom_line() +
  labs(title = "Weekly Time Series of SOX_ug.m3_ENE02368", x = "Week", y = "Average SOX_ug.m3_ENE02368")


# Separate data into weekdays and weekends
new_data2 <- new_data2 %>%
  mutate(weekday = weekdays(date))

# Create separate plots for weekdays and weekends
weekday_plot <- ggplot(new_data2 %>% filter(weekdays(date) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")),
                aes(x = date, y = sox_ug.m3_ENE02368)) + geom_line() +
  labs(title = "Weekdays Time Series of SOX_ug.m3_ENE02368", x = "Date", y = "SOX_ug.m3_ENE02368")

weekend_plot <- ggplot(new_data2 %>% filter(weekdays(date) %in% c("Saturday", "Sunday")),
                       aes(x = date, y = sox_ug.m3_ENE02368)) +
  geom_line() +
  labs(title = "Weekends Time Series of SOX_ug.m3_ENE02368", x = "Date", y = "SOX_ug.m3_ENE02368")

# Display the two plots (you can arrange them side by side if needed)
weekday_plot
weekend_plot



