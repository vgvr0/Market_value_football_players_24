# Predicting Football Player Market Value

![Portada](Top_2024.jpg)

## Table of Contents

1. Libraries and Data Loading
2. Description and Initial Exploratory Analysis
3. Data Transformation and Feature Engineering
4. Variable Selection
5. Modeling and Hyperparameter Tuning
6. Conclusions

## 1. Libraries and Data Loading

In this section, necessary libraries are imported and data related to football players and their market values is loaded.
```R
library(Boruta)
library(caret)
library(corrplot)
library(cowplot)
library(doParallel)
library(dplyr)
library(dummies) 
library(gam)
library(ggplot2)
library(gridExtra)
library(klaR)
library(lubridate)
library(MASS)
library(mlbench)
library(missForest)
library(MXM)
library(naniar)
library(parallel)
library(psych)
library(randomForest)
library(reshape2)
library(RColorBrewer)
library(sas7bdat)
library(VIM)
```
## 2. Description and Initial Exploratory Analysis

This section provides a description of the dataset and conducts initial exploratory analysis to better understand the nature of the data and the characteristics of the football players.

## 3. Data Transformation and Feature Engineering

Here, transformations are applied to the data and new variables are created if necessary to prepare the data for modeling.

## 4. Variable Selection

Variable selection is performed, identifying the most relevant features for predicting the market value of football players.

## 5. Modeling and Hyperparameter Tuning

In this section, modeling techniques (such as linear regression, decision trees, etc.) are applied to predict the market value of football players. Hyperparameters of the models are tuned to improve performance.

## 6. Conclusions

Final conclusions of the analysis are presented, including the most effective models for predicting the market value of football players and possible areas for future research.
