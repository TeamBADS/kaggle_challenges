setwd("~/kaggle_challenges/house_prices")

library(sqldf)
library(devtools)
library(highcharter)
library(data.table)

#read the data into R

train <- fread("train.csv")

#check missing data

summary(is.na(train))

#large missing data are from Alley, FireplaceQu, MiscFeature, Fence, PoolQC

#Visualisation of MSSubClass

# 20	1-STORY 1946 & NEWER ALL STYLES
# 30	1-STORY 1945 & OLDER
# 40	1-STORY W/FINISHED ATTIC ALL AGES
# 45	1-1/2 STORY - UNFINISHED ALL AGES
# 50	1-1/2 STORY FINISHED ALL AGES
# 60	2-STORY 1946 & NEWER
# 70	2-STORY 1945 & OLDER
# 75	2-1/2 STORY ALL AGES
# 80	SPLIT OR MULTI-LEVEL
# 85	SPLIT FOYER
# 90	DUPLEX - ALL STYLES AND AGES
# 120	1-STORY PUD (Planned Unit Development) - 1946 & NEWER
# 150	1-1/2 STORY PUD - ALL AGES
# 160	2-STORY PUD - 1946 & NEWER
# 180	PUD - MULTILEVEL - INCL SPLIT LEV/FOYER
# 190	2 FAMILY CONVERSION - ALL STYLES AND AGES

train$MSSubClass <- as.factor(train$MSSubClass)

hchart(train$MSSubClass, color = "#B71C1C", name = "MSSubClass") %>% 
  hc_chart(borderColor = '#EBBA95',
           borderRadius = 10,
           borderWidth = 2,
           backgroundColor = list(
             linearGradient = c(0, 0, 500, 500),
             stops = list(
               list(0, 'rgb(255, 255, 255)'),
               list(1, 'rgb(200, 200, 255)')
             )))

#Unclear Different in Price based on Subclass (Group further by definition?)

highchart() %>% 
  hc_add_series_boxplot(train$SalePrice, train$MSSubClass,
                        name = "X", color = "#2980b9") 

#Zoning - Most of the property are Residential with Low Density)

# C	Commercial
# FV	Floating Village Residential
# RH	Residential High Density
# RL	Residential Low Density
# RM	Residential Medium Density

train$MSZoning <- as.factor(train$MSZoning)

hchart(train$MSZoning, color = "#B71C1C", name = "MSZoning") %>% 
  hc_chart(borderColor = '#EBBA95',
           borderRadius = 10,
           borderWidth = 2,
           backgroundColor = list(
             linearGradient = c(0, 0, 500, 500),
             stops = list(
               list(0, 'rgb(255, 255, 255)'),
               list(1, 'rgb(200, 200, 255)')
             )))

#Definitely some clear different in average price based on zoning

highchart() %>% 
  hc_add_series_boxplot(train$SalePrice, train$MSZoning,
                        name = "X", color = "#2980b9") 

#LotFrontage/LotArea - Linear Feet of street connected to the property/Lot size in square feet

hchart(train$LotFrontage, color = "#B71C1C", name = "LotFrontage") %>% 
  hc_chart(borderColor = '#EBBA95',
           borderRadius = 10,
           borderWidth = 2,
           backgroundColor = list(
             linearGradient = c(0, 0, 500, 500),
             stops = list(
               list(0, 'rgb(255, 255, 255)'),
               list(1, 'rgb(200, 200, 255)')
             )))

#Quite a lot of property has no frontage?

highchart() %>% 
  hc_add_series_boxplot(train$LotFrontage, train$MSZoning,
                        name = "X", color = "#2980b9") 

hchart(train$LotArea, color = "#B71C1C", name = "LotArea") %>% 
  hc_chart(borderColor = '#EBBA95',
           borderRadius = 10,
           borderWidth = 2,
           backgroundColor = list(
             linearGradient = c(0, 0, 500, 500),
             stops = list(
               list(0, 'rgb(255, 255, 255)'),
               list(1, 'rgb(200, 200, 255)')
             )))

highchart() %>% 
  hc_add_series_boxplot(train$LotArea, train$MSZoning,
                        name = "X", color = "#2980b9") 


hchart(train, "scatter", x = LotFrontage, y = LotArea, group = MSZoning)

#Street and alley is mostly useless with so many NA and singular category

#Lotshape is possible useful, need to investigate further??

train$LotShape <- as.factor(train$LotShape)

highchart() %>% 
  hc_add_series_boxplot(train$SalePrice, train$LotShape,
                        name = "X", color = "#2980b9") 


#Landcontour is too imbalance in term of category distribution. Useless?

train$LandContour <- as.factor(train$LandContour)

hchart(train$LandContour, color = "#B71C1C", name = "LandContour") %>% 
  hc_chart(borderColor = '#EBBA95',
           borderRadius = 10,
           borderWidth = 2,
           backgroundColor = list(
             linearGradient = c(0, 0, 500, 500),
             stops = list(
               list(0, 'rgb(255, 255, 255)'),
               list(1, 'rgb(200, 200, 255)')
             )))




#Visualisation of Year Built

hchart(train$YearBuilt, color = "#B71C1C", name = "Year Built") %>% 
  hc_chart(borderColor = '#EBBA95',
           borderRadius = 10,
           borderWidth = 2,
           backgroundColor = list(
             linearGradient = c(0, 0, 500, 500),
             stops = list(
               list(0, 'rgb(255, 255, 255)'),
               list(1, 'rgb(200, 200, 255)')
             )))
