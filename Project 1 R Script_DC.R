setwd("C:/Users/dancr/OneDrive/Documents/Data Science/SMU/MSDS 6372 - Applied Stats/Project 1")
Hitters <- read.csv("Hitters.csv",header=T)

# Libraries
library (glmnet)

# Remove NAs
Hitters <- na.omit(Hitters)

# Log Salary and Career Attributes
Hitters$Salary = log(Hitters$Salary)
Hitters$CAtBat = log(Hitters$CAtBat+1)
Hitters$CHits = log(Hitters$CHits+1)
Hitters$CHmRun = log(Hitters$CHmRun+1)
Hitters$CRBI = log(Hitters$CRBI+1)
Hitters$CRuns = log(Hitters$CRuns+1)
Hitters$CWalks = log(Hitters$CWalks+1)

# Setup X/Y stuff for glmnet
x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary
grid=10^seq(10,-2,length =100)

# Create filter for train/test
set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]

# Build model using training data
lasso.mod=glmnet(x[train,],y[train],alpha =1,lambda=grid)
plot(lasso.mod)

# find lamda that results in smallest cross validation error and the MSE
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha =1)
plot(cv.out)
bestlam =cv.out$lambda.min
bestlam
lasso.pred=predict(lasso.mod ,s=bestlam,newx=x[test ,])
testMSE_LASSO<-mean((y.test-lasso.pred)^2)
testMSE_LASSO

# Refit Lasso against full data set and output coefficients
out=glmnet(x,y,alpha=1,lambda=grid)
coef(out,s=bestlam)