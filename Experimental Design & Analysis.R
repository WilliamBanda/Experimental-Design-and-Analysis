## HEADER ####
## Who: <William Banda>
## What: Experimental Design and Analysis (C7041)
## Last edited: 2024-02-08
####

## CONTENTS ####
## 00 Setup
## 01 Import Data
## 02 Data Tidying
## 03 Exploratory Data Analysis
## 04 Statistical Analysis and Plotting Graphs


## 00 Setup
# Get working directory
getwd() # Prints working directory in Console

## Set working directory
setwd("C:/Users/WilliamBanda1/Documents/C7041")


#Let's get the necessarly libraries
library(readr)

# Load tidyverse
library(tidyverse)

## Load the dplyr package
library(dplyr)

# Load the openxlsx package
library(openxlsx)

# install vizreg package
install.packages("visreg")

# Load Vizreg
library(visreg)

# 01 Import data
Data <- read_csv("C:/Users/WilliamBanda1/Documents/C7041/data_data.csv", # file path
                 
                 show_col_types = TRUE)   # show column types


## 02 Data Tidying

# Filter rows
# Delete columns that we dont need and leave only those we need
Data <- Data[, -c(1, 2, 6, 10, 11, 12, 13, 14, 15, 21, 22, 23, 26, 28, 30)]

# Display the modified data frame
print(Data)

# Rename the columns of the dataframe
# Remove capital letters and change some symbols
Data <- Data %>% 
  rename(site.id = SiteID,  rep = Rep, field.id = Field_ID, year = Year, doy = DOY,  dateid = DateID,  
  wkavewindsp = WkAveWindSp, wkavetemp = WkAveTemp, wkaveminrelhum = WkAveMinRelHum, wkaveprecip = WkAvePrecip, 
         ndvi = NDVI, spi.tot = Spi_Tot, spi.div = Spi_Div, aphids = Aphids, orius = Orius)

# Remove columns with data description and paste it on a second sheet in the document
# Extract columns 22 and 23
cut_columns <- Data[, c(16, 17), drop = FALSE]

# Remove data description columns from the first sheet that has actual data 
Data <- Data[, -c(16, 17)]

# Create a new Excel workbook
wb <- createWorkbook()

# Add the original data to the first sheet
addWorksheet(wb, sheetName = "Data_Sheet")
writeData(wb, sheet = "Data_Sheet", x = Data, startCol = 1, startRow = 1)

# Add the data description columns to the second sheet
addWorksheet(wb, sheetName = "Data_Dictionary")
writeData(wb, sheet = "Data_Dictionary", x = cut_columns, startCol = 1, startRow = 1)

# lets remove rows with missing values
Data <- na.omit(Data)

# Save the Excel workbook with my student ID as its name
saveWorkbook(wb, "23328400.xlsx")

## 03 Exploratory Data Analysis

# View the first few rows of the dataset
head(Data, n=5)

# lets view the last few roles of the dataset
tail(Data, n=5)

# lets view the colunm names in our dataset
names(Data)

# Lets explore the structure of the data frame
str(Data)

# lets view summary statistics for numeric variables and counts
summary(Data)


#lets plot the histograms of the 4 meteorogical variables and ndvi to explore the assumptions of multiple linear regression
# we want to explore the relationship of ndvi with the 4 meteorogical varibles

# Change the background colour to light blue
par(bg = "lightblue")

# lets begin with a histogram of ndvi
hist(Data$ndvi,           # Specify data
     breaks = 5,           # Change number of bins
     col = "blue",     # Change colour
     main = "Histogram of ndvi",  # Add title
     xlab = "ndvi",         # Add x-axis label
     xlim = c(0, 1)         # Change x-axis limits
)

# Create histogram of wkavetemp
hist(Data$wkavetemp,           # Specify data
     breaks = 10,                # Change number of bins
     col = "skyblue",          # Change colour
     main = "Histogram of temp",# Add title
     xlab = "wkavetemp",        # Add x-axis label
     xlim = c(23, 29)            # Change x-axis limits
)

# Create histogram of windspe
hist(Data$wkavewinsp,           # Specify data
     breaks = 5,                          # Change number of bins
     col = "darkblue",                     # Change colour
     main = "Histogram of wkavensp", # Add title
     xlab = "wkavensp",         # Add x-axis label
     xlim = c(0.5, 5),                   # Change x-axis limits
     yaxt = "n",                           # Remove y-axis tick marks
     ylab = "") # Remove y-axis label

# Create histogram of relative humidity
hist(Data$wkaveminrelhum,        # Specify data
     breaks = 8,                  # Change number of bins
     col = "green",               # Change colour
     main = "Histogram of relative humidity",  # Add title
     xlab = "wkaveminrelhum",     # Add x-axis label
     xlim = c(40, 60),            # Change x-axis limits
     ylab = "Frequency"           # Add y-axis label
)

# Create histogram of precipitation
hist(Data$wkaveprecip,           # Specify data
     breaks = 8,                          # Change number of bins
     col = "red",                     # Change colour
     main = "Histogram of Precipitation", # Add title
     xlab = "Precipitation",         # Add x-axis label
     xlim = c(0.5, 15),                   # Change x-axis limits
     ylab = "Frequency"        # Add y-axis label
)
# Lets plot the 4 meteorological variables against ndvi to explore their relationships
# Reshape the data into long format
Data_long <- pivot_longer(Data, cols = c(wkavewindsp, wkavetemp, wkaveminrelhum, wkaveprecip),
                          names_to = "variable", values_to = "value")

# Create a combined scatterplot for the 4 meteorogical variables and NDVI
ggplot(Data_long, aes(x = ndvi, y = value, color = variable)) +
  geom_point() +
  labs(title = "Scatterplots of Meteorogical variables against NDVI",
       x = "ndvi",
       y = "Predictor Variable") +
  facet_wrap(~ variable, scales = "free_y")

# Lets check the assumptions of correlation using scatter plot
# We want to explore if there is a linear relationship between the abundance spiders and orius
ggplot(Data, aes(x = spi.tot, y = orius)) +
  geom_point() +
  labs(title = "Correlation between Total Count of Orius and Total Count of Spiders",
       x = "Total Count of Spiders", y = "Total Count of Orius") +
  geom_smooth(method = "lm")


# Lets test the assumptions of a linear relationship between NDVI and Aphids count to fit a GLM
# Plot the data on Scatter Plot
ggplot(Data, aes(x = ndvi, y = aphids)) +
  geom_point() +
  labs(title = "Relationship between NDVI and Total Count of Aphids",
       x = "NDVI", y = "Total Count of Aphids") +
  geom_smooth(method = "lm")

ggplot(Data, aes(aphids, ndvi)) +
  geom_jitter(color = "blue", size = 3, 
              height = 0.2, width = 0, alpha = 0.5) +
  geom_smooth(method = "loess", size = 1,
              col = "red", lty = 2, se = FALSE) +
  labs(x = "NDVI", y = "Total Count of Aphids") + 
  ggtitle("Scatterplot of NDVI vs. Total Count of Aphids") +
  theme_classic()

## 04 Statistical Analysis and plotting

# Fit the multiple linear regression model
# Do average wind speed, average air temperature, average relative humidity and average precipitation have an influence on NDVI
model <- lm(ndvi ~ wkavewindsp + wkavetemp + wkaveminrelhum + wkaveprecip, data = Data)

# Display the summary of the regression model
summary(model)

plot(model)

# Interpret coefficients
coef(model)

# Assess significance
summary(model)$coefficients

# Residuals vs Fitted Values Plot
plot(model$fitted.values, model$residuals,
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)  # Add a horizontal line at y = 0

# Lets Compute Pearson's correlation coefficient
# There is a significant positive correlation between the abundance of spiders in the field and the abundance of Orius
cor_test_result <- cor.test(Data$spi.tot, Data$orius, method = "pearson")

# Print the results
print(cor_test_result)

# Lets fit a quasipoisson model to the data
# Increase in NDVI has an effect on the total number of aphids in the field.
z <- glm(aphids ~ ndvi, family = quasipoisson(link = "log"), data = Data)

# Extract the coefficients
summary(z)

# Visualise model fit (transformed scale)
visreg(z, xvar = "ndvi")

# Visualise the model fit
visreg(z, xvar = "ndvi", scale = "response", rug = FALSE)
points(jitter(aphids, 0.2) ~ ndvi, data = Data, pch = 1, 
       col = "blue", lwd = 1.5)


# Exponentiate the slope coefficient
exp(-4.900) 

# Exponentiate the slope coefficient using the coef() function
exp(coef(z))

anova(z, test = "Chi")

