library(ggplot2)
help(iris)
data(iris)
head(iris)
unique(iris$Species)
```

```{r iris}
# 4.0
library(class)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(e1071)
dim(iris)
unique(iris$Species)
set.seed(123)

# Choose all observations for subspecies 'setosa' and 'versicolor'
iris.subset <- filter(iris, Species %in% c("setosa", "versicolor"))
# View(iris.subset)
# Ensure it is a subset of 100 observations by using dim()
dim(iris.subset)

# Randomly select 40 observations for each of the 2 subspecies
trainId=c(sample(1:50,40), sample(51:100,40))
testId = (1:100)[-trainId]
# Put 80 observations into a training set
trainingSet=iris[trainId,]
# Form test set from remaining 20 observations
testSet=iris[testId,1:5]
testLabs=iris$Species[testId]
trainingLabs=iris$Species[trainId]

# 4.1
# Build SVM using training set with cost $c=0.1$
y=trainingSet$Species
# Apply model to test set
svmfit=svm(y~.,data=trainingSet,kernel="linear", cost=0.1,scale=FALSE)
svmfit$index 
# Report classification results on test set
summary(svmfit)
# Provide visualizations - note plot for SVM is not designed for more than 2 features
plot(svmfit, trainingSet, Petal.Width ~ Petal.Length, slice=list(Sepal.Width=3, Sepal.Length=4))

# 4.2
# For 10-fold cross validation
m=10
folds=sample(1:m,nrow(trainingSet),replace=TRUE)

# Cross validation with linear kernel
set.seed(123)
tune.out=tune(svm, Species~., data = trainingSet, kernel = "linear", ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)
bestmod=tune.out$best.model
summary(bestmod)

# TODO: Report classification results
# TODO: Do you think an SVM with a nonlinear decision boundary should be used for this? It is easy to copy & paste this function with a kernel="radial" if you need to
```

```{r IrisNN}
# 4.3 apply 5-fold cross validation to build optimal neural network model with 2 hidden layers of 5 and 7 hidden neurons
library(class)
library(neuralnet)
data(iris)
set.seed(123)
# Set up
iris.subset <- filter(iris, Species %in% c("setosa", "versicolor"))
# View(iris.subset)
# Ensure it is a subset of 100 observations by using dim()
dim(iris.subset)

trainId=c(sample(1:50,40),sample(51:100,40),sample(101:150,40))
testId=(1:150)[-trainId]
trainingSet=iris[trainId,]; testSet=iris[testId,1:4]
testLabs=iris$Species[testId]
trainingLabs=iris$Species[trainId]
m=5
folds=sample(1:m,nrow(trainingSet),replace=TRUE)

nnTrain=neuralnet(Species~.,iris,hidden=c(5,7),act.fct="logistic",linear.output=FALSE)
plot(nnTrain,show.weights=F,information=F,intercept=F, rep="best",col.hidden="blue")

library(neuralnet); set.seed(123)
nnModels = vector("list",m) # save each estimated model
testErrorCV = double(m)
for (s in 1:m) {
   trainingTmp =trainingSet[folds !=s,]
   testTmp =trainingSet[folds==s,]
   testLabsTmp =trainingLabs[folds==s]
   # fit the NN model
   nnetTmp=neuralnet(Species~., trainingTmp, hidden = c(5,7),
                  act.fct = "logistic",linear.output = FALSE)
   nnModels[[s]]=nnetTmp
   ypred = neuralnet::compute(nnetTmp, testTmp)
   yhat = ypred$net.result
   # assign labels
   SpeciesEst=data.frame(
   "labelEst"=ifelse(max.col(yhat[ ,1:3])==1,"setosa",
                     ifelse(max.col(yhat[ ,1:3])==2,
   "versicolor", "virginica")))
   SpeciesEst=factor(SpeciesEst[,1])
   nOfMissObs= sum(1-as.numeric(testLabsTmp==SpeciesEst))
   terror=nOfMissObs/length(testLabsTmp) # test error
   testErrorCV[s]=terror
}

# Report classification results
testErrorCV
mean(testErrorCV)
sd(testErrorCV)

# Apply optimal neural network model to test set
optNNnumber=min(which(testErrorCV==min(testErrorCV)))
optNNnumber

# Extract & save optimal NN model
optNNModel=nnModels[[optNNnumber]]

plot(optNNModel,show.weights=F,information=F,intercept=F, rep="best",col.hidden="blue")
```

```{r IrisPCA}
# 4.4
library(ggplot2)
library(reshape2)
data(iris)
set.seed(123)
iris.species = iris$Species
iris.data = iris[1:4]
Xsd = scale(iris.data, center=TRUE, scale=TRUE)
# PCA for standardized data
pcaXsd=prcomp(Xsd,rank.=nrow(iris.data)-1)
names(pcaXsd)
dim(iris.data) #dimensions of data matrix
dim(pcaXsd$rotation) #dimensions of loading matrix
dim(pcaXsd$x) #dimensions of score matrix
cpve = cumsum(100*(nrow(iris.data)-1)*pcaXsd$sdev^2)/sum(Xsd^2) #CPVE

colnames(pcaXsd$rotation)
TwoLD = pcaXsd$rotation[,1:2]
colnames(TwoLD)=paste(colnames(TwoLD),"loadings",sep="")
colnames(TwoLD)
dstack <- melt(TwoLD)
# Plot first two principal components against each other by coloring each point on plot with corresponding subspecies
lvPlot=ggplot(dstack,aes(x=value))+
   geom_histogram(bins=25)+
   facet_wrap(~Var2,scales="free_x")+theme_bw()
lvPlot

# Plot first 2 PCs
scoresMat=as.data.frame(pcaXsd$x[,1:2])
sMTmp=t(scoresMat)
colnames(sMTmp)=iris.species
scoresMat=t(sMTmp)
scoresMat

scoresMat=as.data.frame(pcaXsd$x[,1:2])
scoresMat$Species=iris.species

pSV=ggplot(scoresMat,aes(PC1,PC2,color=Species,shape=Species))+
   geom_point()+theme_bw()+xlab("PC1 scores")+
   ylab("PC2 scores")+
   geom_hline(yintercept=0,linetype="dotted")+
   geom_vline(xintercept=0,linetype="dotted")+
   geom_text(aes(label=Species),hjust=-0.2,vjust=0.9,size=2)
pSV

# Do these principal components reveal any systematic pattern on the features for any subspecies?
# Plot cumulative percent of variation explained by all successfully ordered principal components
pve=100*pcaXsd$sdev^2/sum(pcaXsd$sdev^2)
pve
cumsum(pve)
par(mfrow=c(1,2),mar=c(9,4,1,3),mgp=c(1.8,0.6,0.3))
plot(1:length(pve), pve,type="o",ylab="PVE",xlab="Principal component",col="blue")
plot(cumsum(pve),type="o",ylab="Cumulative PVE",xlab="Number of first PCs",col="blue")
abline(h = cumsum(pve)[11], col="red", lty=2)
abline(v = 11, col="red", lty=2)
