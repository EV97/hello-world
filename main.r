#this code will create a grouped barplot where time is on x axis, independent variable such as concentration is fill and y axis is the dependent variable such as beat rate
#(it will need to be altered for anything else)
#any imported data must be in the 3 column: 'Time|Independent variable|Dependent variable' table format
#load libraries: ggplot2, bbplot, plotrix, readr, dplyr, tidyr

library(tidyverse)
library(readr)
library(dplyr)
library(tidyr)
library(bbplot)
library(plotrix)
library(RColorBrewer)

#create variable for data frame
pathofile <- "Data/test data.csv"
Independent_variable <- 'This is the legend title'
y_axis_limit <- 50

#read data frame from csv
loaded_data_frame <- read_csv(
  pathofile,
  TRUE,
  cols(
    #declare data types for columns
    `Independent variable` = col_factor(levels = NULL),
    Time = col_factor(levels = NULL),
    `Dependent variable` = col_number(),
    #an extra variable sometimes gets added so lets skip it
    X4 = col_skip()
  )
)
#omit non values
loaded_data_frame <- na.omit(loaded_data_frame)
#head the data to view in console
head(loaded_data_frame)

loaded_data_frame <- loaded_data_frame %>%
  group_by(Time, `Independent variable`) %>%
  mutate(
    #calculate the mean of the dependent variable
    mean_to_plot = mean(`Dependent variable`, na.rm = TRUE),
    #calculates error bars for plot - currently set at standard error but use sd() instead to change to standard deviation
    error_bar_to_plot = std.error(`Dependent variable`, na.rm = TRUE)
  )

#extracts the number of variables from Independent variables
number_of_variables <- length(unique(loaded_data_frame[["Independent variable"]]))
#create colour variable for colour palette
colours_used <- brewer.pal(number_of_variables, "Reds")

#ggplot is used to produce the graph + bbplot() for the aesthetics
ggplot(data = loaded_data_frame,
       #specifies where variables go
       aes(fill = `Independent variable`, x = `Time`, y = mean_to_plot)) +
  #creates grouped bar plot
  geom_bar(position = 'dodge', stat = 'identity') +
  #manually specifies independent variable and colour
  scale_fill_manual(
    values = c('black', colours_used),
    #specify the name of your legend here
    name = Independent_variable
  ) +
  #limits sets the scale of the y axis
  scale_y_continuous(expand = c(0, 0), limits = c(0, y_axis_limit)) +
  #puts error bars on bars
  geom_errorbar(
    aes(
      ymin = mean_to_plot - error_bar_to_plot,
      ymax = mean_to_plot + error_bar_to_plot
    ),
    width = .2,
    position = position_dodge(.9)
  ) +
  bbc_style() +
  #change labels on your graph
  labs(
    title = 'Title of your graph',
    subtitle = 'Subtitle if required',
    x = 'x axis title',
    y = 'y axis title'
  ) +
  theme(
    #can change size and position of subtitle
    plot.subtitle = element_text(margin = ggplot2::margin(0, 1, 0, 1)),
    #alters axis title
    axis.title = element_text(size = 16),
    #changes legend title
    legend.title = element_text('Name of your legend', size = 16),
    #position of legend on graph
    legend.position = "right",
    #changes legend text
    legend.text = element_text(size = 14),
    #adds x axis ticks
    axis.ticks.x = element_line(colour = "#333333"),
    #alters length of axis tick
    axis.ticks.length =  unit(0.3, "cm"),
    #alters grid lines
    panel.grid.major.y = element_line('#cbcbcb'),
    #adds axis lines
    axis.line = element_line(size = 1, color = 'black')
  )

