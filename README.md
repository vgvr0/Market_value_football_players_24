# PlayerValuePredictor
Predicting the market value of football players in 2024 using linear regression and neural networks

Here are a few key observations from analyzing the provided R code:

The code performs extensive feature selection using various methods like stepwise AIC/BIC, Boruta, MXM, SES, random forest importance, etc. This results in several different subsets of predictor variables being identified as potentially important.

Linear regression models are fit and cross-validated using the different variable subsets. The cross-validation results are compared using boxplots of the RMSE. The stepwise AIC and BIC models seem to perform similarly well.
Neural network models are also tuned and cross-validated using the promising variable subsets. Tuning parameters like number of hidden nodes, weight decay are optimized. The cross-validated performance of the neural nets are compared.

A final neural network model is built using the 7 variables selected by random forest importance - release_clause_eur_m, overall, potential, wage_eur_m, movement_reactions, skill_ball_control, age. With 7 inputs, 53 hidden nodes and decay=0.01 gave optimal results in repeated cross-validation:
size decay bag RMSE Rsquared MAE RMSESD RsquaredSD MAESD
> 53 0.01 FALSE 0.7414321 0.9911114 0.164906 0.3217185 0.007714562 0.01499751
> 
This final model outperforms the linear regression models in cross-validation. Further tuning of the max iterations parameter showed some overfit after 1000-1500 iterations.
In summary, a neural network with 7 key predictors, 1 hidden layer of 53 nodes, weight decay regularization of 0.01 and ~1000 iterations provides a robust, high-performing model to predict player value based on this data and methodology. The extensive feature selection and cross-validated model comparisons lend confidence to this being a reliable model.
