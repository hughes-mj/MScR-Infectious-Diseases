library(ggplot2)
library(tidyverse)
library(readxl)
library(readr)
library(viridis)

##Import data
brad <- read_csv("//Users//meganhughes//Library//Mobile Documents//com~apple~CloudDocs//University of Edinburgh//MScR Infectious Diseases//Bradford Data//CSV//25JUL2025_MBradford.csv")

##Plot the BSA standards and determine the linear regression line
#Variable name in backticks so that it's fine with gaps and special characters
Concentration_ugmL <- brad$`Concentration (ug/mL)`
Absorbance_595nm <- brad$`Mean Absorbance (595nm) [blank corrected]`
plot(Concentration_ugmL,Absorbance_595nm)
lm_BSAStandard = lm(Absorbance_595nm~Concentration_ugmL)
summary(lm_BSAStandard)
abline(lm_BSAStandard)

##Generate values that fall along the intervals of the regression lines (i.e., if x= then y=)
approx(Concentration_ugmL, Absorbance_595nm)

# determining intersection of y value with regression line (Sample 1)
abs1 <- brad[18,3]  # insert desired value here
# finding the x-value that results in the specified predicted y value (Sample 1)
conc1 <- solve(lm_BSAStandard$coefficients[2],(abs1-lm_BSAStandard$coefficients[1]))

# determining intersection of y value with regression line (Sample 2)
abs2 <- brad[19,3]  # insert desired value here
# finding the x-value that results in the specified predicted y value (Sample 2)
conc2 <- solve(lm_BSAStandard$coefficients[2],(abs2-lm_BSAStandard$coefficients[1]))

# determining intersection of y value with regression line (Sample 3)
abs3 <- brad[20,3] # insert desired value here
# finding the x-value that results in the specified predicted y value (Sample 3)
conc3 <- solve(lm_BSAStandard$coefficients[2],(abs3-lm_BSAStandard$coefficients[1]))

# determining intersection of y value with regression line (Sample 4)
abs4 <- brad[21,3] # insert desired value here
# finding the x-value that results in the specified predicted y value (Sample 4)
conc4 <- solve(lm_BSAStandard$coefficients[2],(abs4-lm_BSAStandard$coefficients[1]))

# determining intersection of y value with regression line (Sample 5)
abs5 <- brad[22,3] # insert desired value here
# finding the x-value that results in the specified predicted y value (Sample 5)
conc5 <- solve(lm_BSAStandard$coefficients[2],(abs5-lm_BSAStandard$coefficients[1]))

# determining intersection of y value with regression line (Sample 6)
abs6 <- brad[23,3] # insert desired value here
# finding the x-value that results in the specified predicted y value (Sample 6)
conc6 <- solve(lm_BSAStandard$coefficients[2],(abs6-lm_BSAStandard$coefficients[1]))

# determining intersection of y value with regression line (Sample 7)
abs7 <- brad[24,3] # insert desired value here
# finding the x-value that results in the specified predicted y value (Sample 7)
conc7 <- solve(lm_BSAStandard$coefficients[2],(abs7-lm_BSAStandard$coefficients[1]))

# determining intersection of y value with regression line (Sample 8)
abs8 <- brad[25,3] # insert desired value here
# finding the x-value that results in the specified predicted y value (Sample 8)
conc8 <- solve(lm_BSAStandard$coefficients[2],(abs8-lm_BSAStandard$coefficients[1]))

# determining intersection of y value with regression line (Sample 9)
abs9 <- brad[26,3] # insert desired value here
# finding the x-value that results in the specified predicted y value (Sample 9)
conc9 <- solve(lm_BSAStandard$coefficients[2],(abs9-lm_BSAStandard$coefficients[1]))

# determining intersection of y value with regression line (Sample 10)
abs10 <- brad[27,3] # insert desired value here
# finding the x-value that results in the specified predicted y value (Sample 10)
conc10 <- solve(lm_BSAStandard$coefficients[2],(abs10-lm_BSAStandard$coefficients[1]))

# determining intersection of y value with regression line (Sample 11)
abs11 <- brad[28,3] # insert desired value here
# finding the x-value that results in the specified predicted y value (Sample 11)
conc11 <- solve(lm_BSAStandard$coefficients[2],(abs11-lm_BSAStandard$coefficients[1]))

# determining intersection of y value with regression line (Sample 12 - 1/10 dilution)
abs12 <- brad[29,3] # insert desired value here
# finding the x-value that results in the specified predicted y value (Sample 12)
conc12 <- solve(lm_BSAStandard$coefficients[2],(abs12-lm_BSAStandard$coefficients[1]))

# determining intersection of y value with regression line (Sample 13 - the 1/10 dilution x10)
abs13 <- brad[30,3] # insert desired value here
# finding the x-value that results in the specified predicted y value (Sample 13)
conc13 <- solve(lm_BSAStandard$coefficients[2],(abs13-lm_BSAStandard$coefficients[1]))

## Add new conc values to the brad dataframe
#[row,column] <- new datapoint
brad[18,2] <- conc1
brad[19,2] <- conc2
brad[20,2] <- conc3
brad[21,2] <- conc4
brad[22,2] <- conc5
brad[23,2] <- conc6
brad[24,2] <- conc7
brad[25,2] <- conc8
brad[26,2] <- conc9
brad[27,2] <- conc10
brad[28,2] <- conc11
brad[29,2] <- conc12
brad[30,2] <- conc13

## Add new column to specify whether Standard (BSA) or Elution
# Add column blank initially ("NA")
brad$Type <- "NA"
# New column will be column no4, so denote all 1-7 in column 4 "Standard" and 8-10 "Elution"
brad[c(1:17),4] <- "Standard"
brad[c(18:27), 4] <- "Elution"
brad[c(28:30),4] <- "Concentrate"

## Creating a ggplot of the regression above
# Create it in order that the elements appear on the plot from bottom to top

#Basic plot information (e.g., data source, axis)
brad_plot <- ggplot(data=brad,aes(x=`Concentration (ug/mL)`, y=`Mean Absorbance (595nm) [blank corrected]`, ymax = 1)) + 
  # Create regression line (lm) in grey, with no error bars (se), spanning the full range of the graph
  geom_smooth(formula = y ~ x, method = "lm", color="gray", se=FALSE, fullrange=TRUE) +
  # Plot the datapoints by sample type
  geom_point(aes(color = Type), size = 3) +
  #Changing colour of the points to be colourblind friendly and greyscale visible
  scale_color_viridis(discrete=TRUE) +
  theme_bw() +
  
  # Change axis labels and chart title
  ylab("Mean Absorbance (595nm)")+
  xlab(expression(paste("Concentration (", mu, "g/mL)"))) +
  # Create title that splits (/n) over lines
  ggtitle(expression(paste("Concentration by Absorbance at ", lambda, "=595nm"))) +
  # Make it pretty
  theme_minimal() 
