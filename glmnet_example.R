# This example script performes model selection between LASSO, Ridge and Elastic Net Regression.
# The input data is a matrix with rows as samples and columns as dependent/independent variables.
# The output is a table with alpha (0 to 1.0) and MSE (Mean Squared Error) values. Alpha=0 is Ridge, alpha between 0 and 1.0 is Elastic Net and aplha=1.0 is LASSO. The best model will be the one with the lowest MSE.

# Reference

# http://www4.stat.ncsu.edu/~post/josh/LASSO_Ridge_Elastic_Net_-_Examples.html. http://www4.stat.ncsu.edu/. Accessed 29 October 2016. 

library(glmnet)  # Package to fit ridge/lasso/elastic net models

inputFileName="inputMatrix.txt"
inputData=read.table(inputFileName,header=T)
# Number of samples
n=dim(inputData)[1]


# Split data into train and test sets
trainData=as.matrix(inputData[seq(1,0.7*n),])
testData=as.matrix(inputData[-seq(1,0.7*n),])

# Defining dependent and indepenent variable(s)
dep_var=trainData[,1]
indep_var=trainData[,-1]

# 10-fold Cross validation for each alpha = 0, 0.1, ... , 0.9, 1.0
for (i in 0:10) {
    assign(paste("fit", i, sep=""), cv.glmnet(indep_var,dep_var , type.measure="mse",
                                              alpha=i/10,family="gaussian"))
}

# MSE on test set
testDataPred0 <- predict(fit0, s=fit0$lambda.1se, newx=testData[,-1])
testDataPred1 <- predict(fit1, s=fit1$lambda.1se, newx=testData[,-1])
testDataPred2 <- predict(fit2, s=fit2$lambda.1se, newx=testData[,-1])
testDataPred3 <- predict(fit3, s=fit3$lambda.1se, newx=testData[,-1])
testDataPred4 <- predict(fit4, s=fit4$lambda.1se, newx=testData[,-1])
testDataPred5 <- predict(fit5, s=fit5$lambda.1se, newx=testData[,-1])
testDataPred6 <- predict(fit6, s=fit6$lambda.1se, newx=testData[,-1])
testDataPred7 <- predict(fit7, s=fit7$lambda.1se, newx=testData[,-1])
testDataPred8 <- predict(fit8, s=fit8$lambda.1se, newx=testData[,-1])
testDataPred9 <- predict(fit9, s=fit9$lambda.1se, newx=testData[,-1])
testDataPred10 <- predict(fit10, s=fit10$lambda.1se, newx=testData[,-1])


mse0 <- mean((testData[,1] - testDataPred0)^2)
mse1 <- mean((testData[,1] - testDataPred1)^2)
mse2 <- mean((testData[,1] - testDataPred2)^2)
mse3 <- mean((testData[,1] - testDataPred3)^2)
mse4 <- mean((testData[,1] - testDataPred4)^2)
mse5 <- mean((testData[,1] - testDataPred5)^2)
mse6 <- mean((testData[,1] - testDataPred6)^2)
mse7 <- mean((testData[,1] - testDataPred7)^2)
mse8 <- mean((testData[,1] - testDataPred8)^2)
mse9 <- mean((testData[,1] - testDataPred9)^2)
mse10 <- mean((testData[,1] - testDataPred10)^2)

# Table containing alpha and MSE
alpha_mse=NULL
alpha_mse=rbind(alpha_mse,c(0.0,mse0))
alpha_mse=rbind(alpha_mse,c(0.1,mse1))
alpha_mse=rbind(alpha_mse,c(0.2,mse2))
alpha_mse=rbind(alpha_mse,c(0.3,mse3))
alpha_mse=rbind(alpha_mse,c(0.4,mse4))
alpha_mse=rbind(alpha_mse,c(0.5,mse5))
alpha_mse=rbind(alpha_mse,c(0.6,mse6))
alpha_mse=rbind(alpha_mse,c(0.7,mse7))
alpha_mse=rbind(alpha_mse,c(0.8,mse8))
alpha_mse=rbind(alpha_mse,c(0.9,mse9))
alpha_mse=rbind(alpha_mse,c(1.0,mse10))

colnames(alpha_mse)=c("alpha","MSE")
print(alpha_mse)
