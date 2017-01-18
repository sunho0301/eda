# Exploratory data analysis (see readme for more detail)
# Feel free to follow these steps, or complete your own EDA

# Set up (install packages if you don't have them)
install.packages("vioplot")
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(vioplot)
setwd('~/Documents/info-370/eda/health-burden/')
risk.data <- read.csv('./data/prepped/risk-data.csv', stringsAsFactors = FALSE) 

######################
### Data Structure ###
######################

## Using a variety of functions, investigate the structure of your data, such as:

# Dimensions, column names, structure, summaries, etc.
# Dimemsions:
nrow(risk.data)
ncol(risk.data)
# Column names:
colnames(risk.data)
#data type of each columns
sapply(risk.data, class)

# Replacing missing values...?


###########################
### Univariate Analysis ###
###########################

## Using a variety of approaches, investigate the structure each (risk column) individually

# Summarize data
summary(risk.data)
 
# Create histograms, violin plots, boxplots
hist(risk.data$drug.use)
hist(risk.data$alcohol.use)
hist(risk.data$high.meat)
hist(risk.data$low.exercise)
hist(risk.data$smoking)

####################################
### Univariate Analysis (by age) ###
####################################

# Investiage how each risk-variable varies by **age group**
by.age <- risk.data %>%
          group_by(age) %>%
          summarise(drug = mean(drug.use, na.rm = TRUE),
                    alcohol = mean(alcohol.use, na.ram = TRUE),
                    h.meat = mean(high.meat, na.rm = TRUE),
                    l.exercise = mean(low.exercise, na.rm = TRUE),
                    smoke = mean(smoking, na.rm = TRUE))

# Create histograms, violin plots, boxplots. Calculate values as needed. 
p <- plot_ly(by.age, x = ~by.age$drug, y = ~by.age$age, type = 'bar', orientation = 'h', name = 'drug',
             marker = list(color = 'rgba(246, 78, 139, 0.6)',
                           line = list(color = 'rgba(246, 78, 139, 1.0)',
                                       width = 3))) %>%
  add_trace(x = ~by.age$alcohol, name = 'alcohol',
            marker = list(color = 'rgba(58, 71, 80, 0.6)',
                          line = list(color = 'rgba(58, 71, 80, 1.0)',
                                      width = 3))) %>%
  add_trace(x = ~by.age$h.meat, name = 'h.meat',
            marker = list(color = 'rgba(100, 50, 80, 0.6)',
                          line = list(color = 'rgba(100, 50, 80, 1.0)',
                                      width = 3))) %>%
  add_trace(x = ~by.age$l.exercise, name = 'l.exercise',
            marker = list(color = 'rgba(50, 100, 80, 0.6)',
                          line = list(color = 'rgba(50, 100, 80, 1.0)',
                                      width = 3))) %>%
  add_trace(x = ~by.age$smoke, name = 'smoking',
            marker = list(color = 'rgba(150, 90, 190, 0.6)',
                          line = list(color = 'rgba(70, 90, 100, 1.0)',
                                      width = 3))) %>%
  layout(barmode = 'stack',
         xaxis = list(title = ""),
         yaxis = list(title =""))


####################################
### Univariate Analysis (by sex) ###
####################################

# Investiage how each risk-variable varies by **sex**
by.sex <- risk.data %>%
  group_by(sex) %>%
  summarise(drug = mean(drug.use, na.rm = TRUE),
            alcohol = mean(alcohol.use, na.ram = TRUE),
            h.meat = mean(high.meat, na.rm = TRUE),
            l.exercise = mean(low.exercise, na.rm = TRUE),
            smoke = mean(smoking, na.rm = TRUE))


# Compare male to female values -- requires reshaping (and dropping population)!


########################################
### Univariate Analysis (by country) ###
########################################

## Investiage how each risk-variable varies by **country**

# Aggregate by country

# Create a choropleth map (see https://plot.ly/r/choropleth-maps/)

###########################
### Bivariate Analysis ####
###########################

# Compare risks-variables to one another (visually)
