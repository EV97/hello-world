---
title: "xCELLigence script"
author: "EV"
date: "15/07/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
#this code will create a grouped barplot where time is on x axis, independent variable such as concentration is fill and y axis is the dependent variable such as beat rate
#(it will need to be altered for anything else)
#any imported data must be in 3 column table format
#load libraries: ggplot2, bbplot, plotrix, readr, dplyr, tidyr, RColorBrewer
library(tidyverse)
library(readr)
library(dplyr)
library(tidyr)
library(bbplot)
library(plotrix)
library(RColorBrewer)
library(stringi)
library(tcltk2)
```

```{r}
#loopin 
#choose the files you want to import
#pathofile <- tk_choose.files(multi = TRUE)

#for(i in pathofile) {}

#god <- function(){

```


```{r}
pathofile <- tk_choose.files(multi = TRUE)

# data <- read.csv(pathofile)
data <- read.csv(pathofile)

#extracts name from .csv title
name <- basename(pathofile)
remove_csv <- str_extract(name, '.*(?=\\.csv)')
split_the_title <- str_split(remove_csv, "-", simplify = TRUE)

#assigns the title of your graph
plot_title <- split_the_title[1]

#assigns subtitle if there is one
if (length(split_the_title) <2) {
  warning("expects '-' in .csv file title to seperate graph title and subtitle\n") 
  plot_subtitle <- ""
} else {plot_subtitle <- split_the_title[2]}

#create variable for data frame
y_axis_limit <- 50
axis_text_size <- 16
legend_title_size <- 16
legend_text_size <- 14
```

```{r}
#read data frame from csv
loaded_data_frame <- read_csv(
  pathofile,
  col_names = FALSE,
  #specifies column types: factor, factor, double
  col_types = "ffd"
)
#omit non values
loaded_data_frame <- na.omit(loaded_data_frame)

#head the data to view in console
head(loaded_data_frame)
```

```{r}
#runs though actual column names and integrates into graph  
df_names <- read_csv(
  pathofile,
  col_names = TRUE,
  col_types = "ffd__",
)

column_names <- colnames(df_names)
print(column_names)
Independent_variable_1 <- column_names[1]
Independent_variable_2 <- column_names[2]
Dependent_variable <- column_names[3]

loaded_data_frame <- loaded_data_frame %>%
  group_by(X1, X2) %>%
  mutate(
    #calculate the mean of the dependent variable
    mean_to_plot = mean(X3, na.rm = TRUE),
    #calculates error bars for plot - currently set at standard error but use sd() instead to change to standard deviation
    error_bar_to_plot = std.error(X3, na.rm = TRUE), 
    standard_deviation = sd(X3, na.rm = TRUE)
    )
```



```{r}
#extracts the number of variables from Independent variables
number_of_variables <-
  length(unique(loaded_data_frame[["X2"]]))
if (number_of_variables > 9) {
  stop("Sorry, you have more than 9 independent variables!")
  
}
```

```{r}
#create colour variable for colour palette
colours_used <- brewer.pal(number_of_variables, "YlOrRd")[2:9]
```

```{r}
#ggplot is used to produce the graph + bbplot() for the aesthetics
ggplot(data = loaded_data_frame,
       #specifies where variables go
       aes(fill = `X2`, x = `X1`, y = mean_to_plot)) +
  #creates grouped bar plot
  geom_bar(position = 'dodge', stat = 'identity') +
  #manually specifies independent variable and colour
  scale_fill_manual(values = c('black', colours_used),
                    #specify the name of your legend here
                    name = Independent_variable_2) +
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
    title = plot_title,
    subtitle = plot_subtitle,
    x = Independent_variable_1,
    y = Dependent_variable
  ) +
  theme(
    #can change size and position of subtitle
    plot.subtitle = element_text(margin = ggplot2::margin(0, 1, 0, 1)),
    #alters axis title
    axis.title = element_text(size = axis_text_size),
    #changes legend title
    legend.title = element_text(size = legend_title_size),
    #position of legend on graph
    legend.position = "right",
    #changes legend text
    legend.text = element_text(size = legend_text_size),
    #adds x axis ticks
    axis.ticks.x = element_line(colour = "#333333"),
    #alters length of axis tick
    axis.ticks.length =  unit(0.3, "cm"),
    #alters grid lines
    panel.grid.major.y = element_line('#cbcbcb'),
    #adds axis lines
    axis.line = element_line(size = 1, color = 'black')
  )
```

