# NYSE-Regression-and-Prediction-in-R


In this project a large data set based on the daily closing stock prices of 26 companies were analysed and explored. The aim was to build model to predict Google price from other companies in the dataset. 


1. Summary:

- Corelation coefficient between other 25 companies and Google were calcualted and summarised.
- Linear regression models were built with 4 variable selection techniques: 
		1. Leaps and bounds. 10 varibles were suggested from the plot of adjusted R2 and no. of variable.
		2. Stepwise alternated between forward and backward selection until a model is found
                   that minimises Akaike's information criterion (AIC).
		3. Forward selections started with an intercept-only model, then repeatedlycheck for, and add the                             variable with the largest F-statistic.
		4. Backward selections which an automate process started with full model and to repeatedly check for and                    remove the  variable with the lowest F-statistic.
- Independent varibale transformations were employed following Boxcox theory to improve candidate models. 
- Evaluation metrices R2, Press and Cp were calculated and crossed validate for all candidate models to find the best final model.
- Regression assumptions were check on the final model using Q-Q plot to check if errors were normally distributed, and residuals vs fitted plot to check if residuals were independent and have constant variance. 
 


2. Tool: R
 
3. Algorithms and functions:
- linear regression.
- leap and bound variable selection with function leaps.
- Stepwise selection with function step.
- Forward selection with function add1.
- Backward selection with function drop1.

4. Evaluation: Adjusted R2, Cp, and Press metrices.

5. Dataset:the New York Stock Exchange (NYSE) between 4th January 2010 and 16th December 2015.



