# glmnet
# This example script performes model selection between LASSO, Ridge and Elastic Net Regression.
# The input data is a matrix with rows as samples and columns as dependent/independent variables.
# The output is a table with alpha (0 to 1.0) and MSE (Mean Squared Error) values. Alpha=0 is Ridge, alpha between 0 and 1.0 is Elastic Net and aplha=1.0 is LASSO. The best model will be the one with the lowest MSE.

# Reference

# http://www4.stat.ncsu.edu/~post/josh/LASSO_Ridge_Elastic_Net_-_Examples.html. http://www4.stat.ncsu.edu/. Accessed 29 October 2016. 
