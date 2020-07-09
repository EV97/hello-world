#analysis for xCELLigence where x axis is time and y axis xCELLIGENCE parameter e.g. beat rate 
#this code will create a grouped barplot (it will need to be altered for anything else)
#any imported data must be in Time|Drug/concentration|xCELLigence parameter table format
#load libraries: ggplot2, bbplot, plotrix, readr, dplyr, tidyr

library(tidyverse)
library(readr)
library(dplyr)
library(tidyr)
library(bbplot)
library(plotrix)

#read and analyze data with read_csv (readr)
#read data. Time and concentration are factor data types, the xCELLigence parameter should be numeric - this needs to be specified on import (see code below for col_factor()). Also concentration should be.
#na.omit() is really important for omitting any non values this will be useful later when you're producing your graph. 
#the head() function will give you a preview of the data set in the console, but use view() to see the whole set in a separate tab
#data is where the data frame you are working with should go

data <- read_csv(data, col_types = cols(`Drug Concentration` = col_factor(levels = c("Control", "Varying", "Concentrations", "go", "here", "(make sure units are the same)")), Time = col_factor(levels = c("0", "Timepoints", "go", "here", "again", "keep", "same", "units", "120", "144"))), na = "null")
na.omit(data)
head(data)

#column names - this is a useful step to help you work with the data as your variable names could differ from this point on. It just prints your column names.
original_col_names <- colnames(data)
print(original_col_names)

#column averages and standard error are needed to plot the baplot. you can use sd() instead of std.error to calculate standard deviation.
#the (na.rm = TRUE) argument is really important as it excludes cells with no values out of your calculations
BR_averages <- data %>%
  group_by(Time, `Drug Concentration`) %>%
  mutate(average_beat_rate = mean(`Beat Rate`, na.rm = TRUE), 
         stan_error_beat_rate = std.error(`Beat Rate`, na.rm = TRUE))

#ggplot is used to produce a graph (explanation of each element below)
ggplot(data = BR_averages, aes(fill = `Drug Concentration`, x = `Time`, y = average_beat_rate)) +
  geom_bar(position = 'dodge', stat='identity') +
  scale_fill_manual(breaks = c('Control', '0.1', '1', '10', '100','1000'), 
                    values = c('black', '#FF9999', '#FF6666', '#FF3333', '#FF0000','#990000'), 
                    name = 'Drug Concentration (nM)')+
  scale_y_continuous(expand = c(0,0), limits = c(0,8))+  
  geom_errorbar(aes(ymin=average_beat_rate-stan_error_beat_rate, 
                    ymax=average_beat_rate+stan_error_beat_rate), 
                width=.2, 
                position=position_dodge(.9)) +
  bbc_style()+
  labs(title = 'drug/cell type', subtitle = 'what is being measured', x = 'Time (hours)', y = 'Beat Rate (BPM)') + 
  theme(plot.subtitle = element_text(margin = ggplot2::margin(0, 1, 0, 1)), 
        axis.title = element_text(size = 16), 
        legend.title = element_text('Drug Concentration (nM)', size = 16), 
        legend.position = "right", 
        legend.text = element_text(size = 14), 
        axis.ticks.x = element_line(colour = "#333333"), 
        axis.ticks.length =  unit(0.3, "cm"), 
        panel.grid.major.y=element_line('#cbcbcb'), 
        axis.line = element_line(size = 1, color = 'black'))


#ggplot argument creates plot with data
#geombar() creates the bar plot
#scale_fill_manual is where you can change colour and labels. to change colours use google to find R colour codes.
#scale_y_continuous is used to place bars on the x axis
#geom_error_bar is where error bars are specified 
#bbc_style is the graphics used
#labs is labels and this is where you can change all the labels on the chart
#the theme argument is used to define other features of the plot - this is where most adjustments can be made if needed
