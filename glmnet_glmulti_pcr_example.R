# This example script performes model selection between LASSO, Ridge and Elastic Net Regression.
# The input data is a matrix with rows as samples and columns as dependent/independent variables.
# The output is a table with alpha (0 to 1.0) and MSE (Mean Squared Error) values. Alpha=0 is Ridge, alpha between 0 and 1.0 is Elastic Net and aplha=1.0 is LASSO. The best model will be the one with the lowest MSE.

# Reference

# http://www4.stat.ncsu.edu/~post/josh/LASSO_Ridge_Elastic_Net_-_Examples.html. Accessed 29 October 2016. 

library(glmnet)  # Package to fit ridge/lasso/elastic net models

inputFileName="inputMatrix.txt"
inputData=read.table(inputFileName,header=T)
# Number of samples
nrow=dim(inputData)[1]


# Split data into train and test sets
perTrainData=0.7*nrow
randomRow=sample(seq(1,nrow),perTrainData)
trainData=as.matrix(inputData[randomRow,])
testData=as.matrix(inputData[-randomRow,])

# Defining dependent and indepenent variable(s)
target=trainData[,1]
features=trainData[,-1]

# 10-fold Cross validation for each alpha = 0, 0.1, ... , 0.9, 1.0
for (i in 0:10) {
    assign(paste("fit", i, sep=""), cv.glmnet(features,target , type.measure="mse",
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
sel_alpha=which.min(c(mse0,mse1,mse2,mse3,mse4,mse5,mse6,mse7,mse8,mse9,mse10))-1
par(mfrow=c(1,2))
plot(sel_model,xvar="lambda")
plot(eval(parse(text=paste("fit",sel_alpha,sep=""))))
sel_lambda=eval(parse(text=paste("fit",sel_alpha,"$lambda.1se",sep="")))
glmnet_best_model=glmnet(features,target,alpha=sel_alpha/10,lambda=sel_lambda,family="gaussian")
testDataPred_glmnet = predict(glmnet_best_model, s=sel_lambda,newx=testData[,-1])
mse_glmnet <- mean((testData[,1] - testDataPred_glmnet)^2)

glmulti_res = glmulti(rating~.,data=data.frame(trainData),method="g",level=1)
glmulti_best_model=glm(rating ~ calories + protein + fat + sodium + fiber + carbo + sugars + potass + vitamins,data=data.frame(trainData))
testDataPred_glmulti <- predict(glmulti_best_model, newdata=data.frame(testData[,-1]))
mse_glmulti <- mean((testData[,1] - testDataPred_glmulti)^2)

pcr_model = pcr(rating~.,data=data.frame(trainData),scale=T,validation="CV")
pcr_pred = predict(pcr_model, testData[-1], ncomp = 3)
mse_pcr = mean((testData[,1] - pcr_pred)^2) 


