library(C50)

# Read in data and eliminate unneccesary columns 
dat <- read.csv("pml-training.csv",stringsAsFactors=TRUE)
dat <- dat[,8:160]
dat <- dat[,which(dat[1,]!="NA"&dat[1,]!="")]

# Split into sets
inTrain <- createDataPartition(y=dat$classe,p=0.7,list=FALSE)
training <- dat[inTrain,]
testing <- dat[-inTrain,]
set.seed(123)
ruleModel <- C5.0(x=training[,-53],y=training[,53],trials=10,rules=TRUE)
C5imp(ruleModel)
predRule <- predict(ruleModel,newdata=testing[,-53])
plot(predRule,testing[,53])
plot(as.numeric(predRule),as.numeric(testing[,53]))
sum(as.numeric(predRule)-as.numeric(testing[,53]))
sum(as.numeric(predRule)-as.numeric(testing[,53])!=0)

