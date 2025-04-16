#29/01/2025
#Plots of climate envelope of Svalbard location
#Over one year period

library(ggplot2)
library(readxl)
library(readr)
library(tidyverse)
library(dplyr)
library(hrbrthemes)
library(ggpubr)
library(rstatix)
library(zoo)
library(lubridate)
library(gridExtra)
library(nlme)


#### Processing & Cleaning ####
data_new <- read_excel("Climate/SvalbardStationData.xlsx")
head(data_new)
data_new$Date <- as.Date(data_new$Date)
data_new$Date <- as.POSIXct(data_new$Date)
data_new$AverageDailyPrecipitation <- as.numeric(data_new$AverageDailyPrecipitation)
data_new$AverageRelativeHumidity <- as.numeric(data_new$AverageRelativeHumidity)
data_new$AverageCloudCover <- as.numeric(data_new$AverageCloudCover)
data_new$SnowLevel <- as.numeric(data_new$SnowDepth)
summary(data_new$AverageDailyTemperature)
summary(data_new$AverageDailyPrecipitation)

head(data_new)
data_new$AverageDailyPrecipitation[is.na(data_new$AverageDailyPrecipitation)] <- 0

#Define winter (Jan-Apr) and summer (May-Oct)
data_new <- data_new %>%
  mutate(month = month(Date)) %>%
  mutate(season = case_when(
    month %in% c(1, 2, 3, 4) ~ "Polar Winter",
    month %in% c(5, 6, 7, 8, 9, 10) ~ "Polar Summer",
    TRUE ~ "Other"  # November and December will be ignored
  ))

# Calculate average temperature for winter and summer
seasonal_avg <- data_new %>%
  filter(season %in% c("Polar Winter", "Polar Summer")) %>%
  group_by(season) %>%
  summarise(avg_temp = mean(AverageDailyTemperature, na.rm = TRUE))
seasonal_avg_pr <- data_new %>%
  filter(season %in% c("Polar Winter", "Polar Summer")) %>%
  group_by(season) %>%
  summarise(avg_pr = mean(AverageDailyPrecipitation, na.rm = TRUE))
seasonal_avg_r <- data_new %>%
  filter(season %in% c("Polar Winter", "Polar Summer")) %>%
  group_by(season) %>%
  summarise(avg_r = mean(AverageRelativeHumidity, na.rm = TRUE))
seasonal_avg_sd <- data_new %>%
  filter(season %in% c("Polar Winter", "Polar Summer")) %>%
  group_by(season) %>%
  summarise(avg_sd = mean(SnowDepth, na.rm = TRUE))
seasonal_avg
seasonal_avg_pr
seasonal_avg_r
seasonal_avg_sd
summary(data_new$AverageDailyTemperature)
summary(data_new$AverageDailyPrecipitation)
summary(data_new$AverageRelativeHumidity)
summary(data_new$SnowDepth)

#### Plots ####
#Temperature
temp_plot <- ggplot(data_new, aes(x = Date, y = AverageDailyTemperature)) +
  geom_line(color = "red", size = 1) +
  geom_hline(data = seasonal_avg, aes(yintercept = avg_temp, color = season),
             linetype = "dashed", size = 0.8) + 
  labs(
    title = "",
    x = "",
    y = "Air Temperature (°C)",
    color = "Seasonal Mean"
  ) +
  scale_color_manual(values = c("Polar Winter" = "navy", "Polar Summer" = "red")) +  
  theme_pander()
temp_plot
seasonal_avg
#Precipitation
precipitation_plot <- ggplot(data_new, aes(x = Date, y = AverageDailyPrecipitation)) +
  geom_line(color = "blue", size = 1) +
  geom_hline(data = seasonal_avg_pr, aes(yintercept = avg_pr, color = season),
             linetype = "dashed", size = 0.8) + 
  labs(
    title = "",
    x = "",
    y = "Precipitation (mm)",
    color = "Seasonal Mean"
  ) +
  scale_color_manual(values = c("Polar Winter" = "navy", "Polar Summer" = "red")) +  
  theme_pander()
precipitation_plot

 #Relative Humidity
humidity_plot <- ggplot(data_new, aes(x = Date, y = AverageRelativeHumidity)) +
  geom_line(color = "turquoise", size = 1) +
  geom_hline(data = seasonal_avg_r, aes(yintercept = avg_r, color = season),
             linetype = "dashed", size = 0.8) + 
  labs(
    title = "",
    x = "",
    y = "Relative Humidity (%)",
    color = "Seasonal Mean"
  ) +
  scale_color_manual(values = c("Polar Winter" = "navy", "Polar Summer" = "red")) +  
  theme_pander()
humidity_plot

#Snow Level
snow_plot <- ggplot(data_new, aes(x = Date, y = SnowDepth)) +
  geom_line(color = "goldenrod", size = 1) +
  geom_hline(data = seasonal_avg_sd, aes(yintercept = avg_sd, color = season),
             linetype = "dashed", size = 0.8) +
  labs(
    title = "",
    x = "Time",
    y = "Snow Depth (cm)",
    color = "Seasonal Mean"
  ) +
  scale_color_manual(values = c("Polar Winter" = "navy", "Polar Summer" = "red")) +  
  theme_pander()
snow_plot

#plot as patchwork
grid.arrange(
  temp_plot,
  precipitation_plot,
  humidity_plot,
  snow_plot,
  ncol = 1
)

#### Model Creation ####
## Mixed Effects Modelling ## 
# Convert Date column to a POSIXct object:
model_data <- data_new
model_data <- na.omit(model_data)
model_data$Date <- as.POSIXct(model_data$Date)
# Ensure season is a factor with the proper level ordering
model_data <- model_data %>%
  filter(season != "Other")

model_data$season <- factor(model_data$season, levels = c("Polar Winter", "Polar Summer"))
# Create a continuous time variable (e.g., number of days since the first observation)
model_data$time <- as.numeric(difftime(model_data$Date, min(model_data$Date), units = "days"))
# Mixed-effects model
MEmodelPr <- lme(
  AverageDailyPrecipitation ~ season,
  data = model_data,
  random = ~ 1 | month,                          # Random intercept for each month (adjust if needed)
  correlation = corAR1(form = ~ time)            # AR(1) structure for the residuals across time
)
MEmodelRH <- lme(
  AverageRelativeHumidity ~ season,
  data = model_data,
  random = ~ 1 | month,                          # Random intercept for each month (adjust if needed)
  correlation = corAR1(form = ~ time)            # AR(1) structure for the residuals across time
)
MEmodelT <- lme(
  AverageDailyTemperature ~ season,
  data = model_data,
  random = ~ 1 | month,                          # Random intercept for each month (adjust if needed)
  correlation = corAR1(form = ~ time)            # AR(1) structure for the residuals across time
)
# Show the summary of the fitted model
summary(MEmodelPr)
summary(MEmodelRH)

## GLS Modelling
modelGLSPr <- gls(AverageDailyPrecipitation ~ season, 
             correlation = corAR1(), 
             data = model_data)
modelGLSRH <- gls(AverageRelativeHumidity ~ season, 
                correlation = corAR1(), 
                data = model_data)
modelGLST <- gls(AverageDailyTemperature ~ season, 
                  correlation = corAR1(), 
                  data = model_data)
summary(modelGLSPr)
summary(modelGLSRH)

#null models
null_modelPr <- gls(
  AverageDailyPrecipitation ~ 1,            
  data = model_data,
  correlation = corAR1(form = ~ time),        
  method = "ML"                             
)
null_modelRH <- gls(
  AverageRelativeHumidity ~ 1,            
  data = model_data,
  correlation = corAR1(form = ~ time),        
  method = "ML"                             
)
null_modelT <- gls(
  AverageDailyTemperature ~ 1,            
  data = model_data,
  correlation = corAR1(form = ~ time),        
  method = "ML"                             
)
summary(null_modelPr)

#compare AICs
AIC(null_modelPr, modelGLSPr, MEmodelPr)
AIC(null_modelRH, modelGLSRH, MEmodelRH)
AIC(null_modelT, modelGLST, MEmodelT)

#AIC lower for both gls and so I will use this. 
summary(modelGLSPr)
summary(modelGLSRH)
summary(MEmodelT)

#### Assumptions ####
# Check residuals
plot(modelGLSRH, resid(., type = "normalized") ~ fitted(.), abline = 0)
qqnorm(resid(modelGLSPr, type = "normalized"))
qqline(resid(modelGLSPr, type = "normalized"))
#non-normal look at the top end but the large dataset should reduce these impacts

model_data <- na.omit(model_data[, c("AverageRelativeHumidity", "season")]) ##find somewhere to put this
## RH
#assumptions
data_new %>% shapiro_test(AverageRelativeHumidity)
ggqqplot(data_new, x = "AverageRelativeHumidity")
library(car)
leveneTest(AverageRelativeHumidity ~ season, data = data_new)
boxplot(AverageRelativeHumidity ~ season,
        data = data_new)



#assumptions are not violated except independent data
#instead, I can create linear models for each season
#model <- lm(AverageRelativeHumidity ~ season, data = data_new)
#model <- lm(AverageDailyPrecipitation ~ season, data = filtered_data_p)

summary(model)
install.packages("lmtest")
library(lmtest)

dwtest(model)  # Durbin-Watson test for autocorrelation
#<0.05 so reject null, accept that autocorrelation is greater than 0  
acf(resid(model))  # Autocorrelation plot of residuals
head(data_new)

#autocorrelation needs to be taken into account for RH so a gls should be used instead of lm
clean_data_p <- na.omit(data_new[, c("AverageDailyPrecipitation", "season")])
filtered_data_p <- clean_data_p %>%
  filter(season != "Other")
head(filtered_data_p)
plot(model, which = 1)  # base plot
plot(model, which = 2)
shapiro.test(residuals(model))
#Precipitation shows no autocorrelation but variance assumption is violated too


#using a gls instead for RH & for precip
library(nlme)
clean_data <- na.omit(data_new[, c("AverageRelativeHumidity", "season")])
filtered_data <- clean_data %>%
  filter(season != "Other")
tail(filtered_data)
unique(filtered_data$season)
unique(filtered_data_p$season)
#remove NA and season==Other so I am just comparing summer & winter

#check assumptions
plot(resid(model2) ~ fitted(model2), main = "Residuals vs Fitted")
abline(h = 0, col = "red")
plot(fitted(model2), resid(model2), main = "Homoscedasticity Check")
abline(h = 0, col = "red")
qqnorm(resid(model2))
qqline(resid(model2), col = "red")

shapiro.test(resid(model2))  # Optional, for small to medium sample sizes
#all result okay. Relatively equal variance, normal distr, fine to use gls
model <- gls(AverageDailyPrecipitation ~ season, correlation = corAR1(), data = filtered_data_p)
model2 <- gls(AverageRelativeHumidity ~ season, correlation = corAR1(), data = filtered_data)
summary(model2)
summary(model)