# Predicting Football Player Market Value

![Project Banner](Top_2024.jpg)

## Overview

This project aims to predict the market value of football players using machine learning models. By exploring, engineering features, and applying advanced algorithms, we strive to uncover insights and achieve accurate predictions.

## Table of Contents

1. [Introduction](#introduction)
2. [Setup and Libraries](#setup-and-libraries)
3. [Exploratory Data Analysis](#exploratory-data-analysis)
4. [Data Preprocessing and Feature Engineering](#data-preprocessing-and-feature-engineering)
5. [Feature Selection](#feature-selection)
6. [Modeling and Evaluation](#modeling-and-evaluation)
7. [Results and Conclusions](#results-and-conclusions)
8. [Future Work](#future-work)
9. [How to Run the Project](#how-to-run-the-project)

---

## Introduction

Understanding the market value of football players is crucial for clubs, agents, and analysts. This project leverages machine learning techniques to analyze historical player data, evaluate performance, and predict player market values effectively.

---

## Setup and Libraries

To replicate the analysis or contribute to the project, ensure you have the following libraries installed:

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
library(lubridate)
library(randomForest)
```

Additional libraries may be listed in the source code.

---

## Exploratory Data Analysis

An initial analysis was conducted to understand the dataset's structure and identify key variables:
- Basic statistics and distributions of player features.
- Correlation analysis to explore relationships between variables.
- Visualizations to highlight trends and anomalies.

---

## Data Preprocessing and Feature Engineering

Key steps included:
- Handling missing values using imputation strategies.
- Scaling and normalizing numerical features for consistency.
- Creating dummy variables for categorical data.
- Engineering new features such as `years_remaining` and player `categories`.

---

## Feature Selection

Various methods were explored to select the most relevant features for modeling, including:
1. **Stepwise Selection (AIC and BIC)**
2. **Boruta Algorithm**
3. **Recursive Feature Elimination (RFE)**
4. **Random Forest Feature Importance**

The final selected features differ based on the modeling technique used, ensuring the best predictive performance.

---

## Modeling and Evaluation

Multiple machine learning models were applied and tuned, such as:
- **Linear Regression**: For baseline performance.
- **Random Forest**: To capture non-linear relationships.
- **Artificial Neural Networks (ANNs)**: To model complex interactions.

Evaluation metrics:
- **RMSE (Root Mean Square Error)**
- **R² (Coefficient of Determination)**
- **MAE (Mean Absolute Error)**

Hyperparameter tuning was performed to optimize model performance.

---

## Results and Conclusions

Key findings:
- Models using features like `release_clause_eur_m`, `potential`, and `overall` performed exceptionally well.
- Neural networks provided the best results with optimized parameters, achieving RMSE as low as **0.5115** in cross-validation.


---

## Future Work

Potential improvements include:
- Incorporating additional datasets, such as player injuries or transfer market trends.
- Experimenting with ensemble methods.
- Deploying the model as an API for real-time market value predictions.

---


---

