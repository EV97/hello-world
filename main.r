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
library(ggpubr)
library(car)
library(multcompView)
library(reshape2)
library(broom)
library(multcomp)
library(officer)
library(flextable)
library(rvg)
library(rstatix)
library(lme4)
library(emmeans)
```

```{r}
#create variable for data frame
axis_text_size <- 16
legend_title_size <- 16
legend_text_size <- 14
```

```{r}
get_titles <- function(path) {
  #extracts name from .csv title
  name <- basename(path)
  remove_csv <- str_extract(name, '.*(?=\\.csv)')
  split_the_title <- str_split(remove_csv, "-", simplify = TRUE)
  
  #assigns the title of your graph
  plot_title <- split_the_title[1]
  
  #assigns subtitle if there is one
  if (length(split_the_title) < 2) {
    warning("expects '-' in .csv file title to seperate graph title and subtitle\n")
    plot_subtitle <- ""
  } else {
    plot_subtitle <- split_the_title[2]
  }
  return(list(title = plot_title, subtitle = plot_subtitle))
}
```

```{r}
read_the_data <- function(path) {
  #read data frame from csv
  loaded_data_frame <- read_csv(
    path,
    col_names = FALSE,
    #specifies column types: factor, factor, double
    col_types = cols_only(
      X1 = "f",
      X2 = "f",
      X3 = "d",
      X4 = "c",
      X5 = "f"
    ),
    skip = 1
  )
  #omit non values
  loaded_data_frame <- na.omit(loaded_data_frame)
  return(loaded_data_frame)
}
```

```{r}
get_column_names <- function(path) {
  #runs though actual column names and integrates into graph
  df_names <- read_csv(path,
                       col_names = TRUE,
                       col_types = cols_only("f", "f", "d", "c", "f"))
  
  column_names <- colnames(df_names)
  
  return(list(
    independent_variable_1 = column_names[1],
    independent_variable_2 = column_names[2],
    dependent_variable = column_names[3]
  ))
}
```

```{r}
summarise_data <- function(data_frame) {
  data_frame <- data_frame %>%
    group_by(X1, X2) %>%
    mutate(
      #calculate the mean of the dependent variable
      mean_to_plot = mean(X3, na.rm = TRUE),
      #calculates error bars for plot - currently set at standard error but use sd() instead to change to standard deviation
      error_bar_to_plot = std.error(X3, na.rm = TRUE),
      standard_deviation = sd(X3, na.rm = TRUE)
    )
  return(data_frame)
}
```

```{r}
get_y_axis_limit <- function(data_frame) {
  #find y value limit from data - using data to get the max limit
  max_mean_to_plot <- max(data_frame$mean_to_plot)
  #adding a 5% ceiling to the highest value
  five_percent_of_max_mean <- max_mean_to_plot / 20
  #setting limit using round() function
  y_axis_limit <-
    round((max_mean_to_plot / 5 + five_percent_of_max_mean), digits = 1) *
    5
  return(y_axis_limit)
}

```

```{r}
get_number_of_variables <- function(data_frame) {
  #extracts the number of variables from Independent variables
  number_of_variables <- length(unique(data_frame[["X2"]]))
  if (number_of_variables > 9) {
    stop("Sorry, you have more than 9 independent variables!")
  }
  return(number_of_variables)
}
```

```{r}
get_the_colours <- function(palette = "YlOrRd", number_of_variables) {
  #create colour variable for colour palette
  colours_used <- brewer.pal(number_of_variables, palette )[2:9]
  return(colours_used)
}
```

```{r}
get_main_plot <-
  function(data_frame, titles, column_names, the_colours) {
    y_axis_limit <- get_y_axis_limit(data_frame)
    
    #ggplot is used to produce the graph + bbplot() for the aesthetics
    main_plot <- ggplot(data = data_frame,
                        #specifies where variables go
                        aes(fill = `X2`, x = `X1`, y = mean_to_plot)) +
      #creates grouped bar plot
      geom_bar(position = 'dodge', stat = 'identity') +
      #manually specifies independent variable and colour
      scale_fill_manual(values = c('black', the_colours),
                        #specify the name of your legend here
                        name = column_names['independent_variable_2']) +
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
        title = titles['title'],
        subtitle = titles['subtitle'],
        x = column_names['independent_variable_1'],
        y = column_names['dependent_variable']
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
    
    
    return(main_plot)
  }
```
 
```{r}
get_box_plot <- function(data_frame, titles, column_names, the_colours) {
#box plot to show variation   
  box_plot <- ggboxplot(
    data_frame,
    x = "X1",
    y = "X3",
    color = "X2",
    palette = c("black", the_colours)
  ) +
    labs(
      title = titles['title'],
      subtitle = "Variation within groups",
      x = column_names['independent_variable_1'],
      y = column_names['dependent_variable'],
      color = column_names['independent_variable_2']
    )
  
  return(box_plot)
}
```

```{r}
get_mixed_mod <- function(data_frame) {
#generation of mixed effects model  
my_mixed_model <- lmer(X3~X2*X1+(1|X5), data = data_frame)

  return(my_mixed_model)
}
```

```{r}
#post hoc analysis uses a mixed effects model with emmeans
get_post_hoc <- function(my_mixed_model) {
 
  post_hoc_means <- emmeans(my_mixed_model, specs = pairwise~X2|X1)

 return(post_hoc_means)
}
```

```{r}
get_percentage_of_controls <- function(path) {
  #new data frame
  modified_data_frame <- read_csv(
    path,
    col_names = FALSE,
    #specifies column types: factor, factor, double
    col_types = cols_only(
      X1 = "f",
      X2 = "f",
      X3 = "d",
      X4 = "f"
    ),
    skip = 1
  )
  #omit non values
  modified_data_frame <- na.omit(modified_data_frame)
  
  new_data_frame <- modified_data_frame %>%
    group_by(X1, X2) %>%
    mutate(mean_of_groups = mean(X3, na.rm = TRUE))
  
  new_data_frame <-
    subset(new_data_frame, select = c(X1, X2, X4, mean_of_groups))
  new_data_frame <- unique(new_data_frame)
  new_data_frame <-
    spread(new_data_frame, key = X4, value = mean_of_groups)
  
  for (row in 1:nrow(new_data_frame)) {
    if (new_data_frame[row, 'X2'] == 'Control') {
      control_mean <- new_data_frame[row, 'Control']
    } else {
      treated_mean <- new_data_frame[row, 'Treated']
      new_data_frame[row, '% of Control'] <-
        (treated_mean - control_mean) / control_mean * 100
    }
  }
  return(new_data_frame)
}
```

```{r}
#output to powerpoint - as png
create_pptx <- function(main_plot, box_plot, percentage_of_controls, path = file.choose()) {
  
  if (!file.exists(path)) {
    
    out <- read_pptx()
    
  } else {
    
    out <- read_pptx(path)
    
  }

  out %>%
    
    add_slide(layout = "Title and Content", master = "Office Theme") %>%
    
    ph_with(
      
      value = dml(ggobj = main_plot),
      
      location = ph_location(
        
        left = 1,
        
        top = 1,
        
        width = 8,
        
        height = 5.4,
        
      )
      
    ) %>%
    
    add_slide(layout = "Title and Content", master = "Office Theme") %>%
    
    ph_with(value = main_plot,
            
            location = ph_location(
              
              left = 1,
              
              top = 1,
              
              width = 9,
              
              height = 5.4,
              
            )) %>%
    
    add_slide(layout = "Title and Content", master = "Office Theme") %>%
    
    ph_with(
      
      value = dml(ggobj = box_plot),
      
      location = ph_location(
        
        left = 1,
        
        top = 1,
        
        width = 8,
        
        height = 5.4,
        
      )
      
    ) %>%
    
    add_slide(layout = "Title and Content", master = "Office Theme") %>%
    
    ph_with(value = box_plot,
            
            location = ph_location(
              
              left = 1,
              
              top = 1,
              
              width = 9,
              
              height = 5.4,
              
            )) %>%
    
    add_slide(layout = "Title and Content", master = "Office Theme") %>%
    
    ph_with(value = percentage_of_controls,
            
            location = ph_location(
              
              left = 1,
              
              top = 1,
              
              width = 9,
              
              height = 5.4,
              
            )) %>%
    
    print(target = path)
}
```


```{r}
pathofiles <- tk_choose.files(multi = TRUE)

for (file in pathofiles) {
  titles <- get_titles(file)
  column_names <- get_column_names(file)
  
  data_frame <- read_the_data(file)
  data_frame <- summarise_data(data_frame)
  
  number_of_variables <- get_number_of_variables(data_frame)
  colours <-
    get_the_colours(number_of_variables = number_of_variables)
  
  main_plot <-
    get_main_plot(data_frame, titles, column_names, colours)
  box_plot <-
    get_box_plot(data_frame, titles, column_names, colours)
  
  the_mixed_model <- get_mixed_mod(data_frame)
  print(the_mixed_model)
  
  the_post_hoc <- get_post_hoc(the_mixed_model)
  print(the_post_hoc)
  
  percentage_of_controls <- get_percentage_of_controls(file)
  
  # powerpoint output
  create_pptx(
    main_plot,
    box_plot,
    percentage_of_controls,
    paste(titles['title'], '.pptx')
  )
}
```
