library(e1071)
library(lattice)

train<-read.csv("E:/GOM/EMP 508/FINAL PROJECT/titanic/train.csv")
train<-na.omit(train)
View(train)

train<-as.data.frame(train)
train$Gender[train$Sex=="male"]<-1
train$Gender[train$Sex=="female"]<-2

train[is.na(train)]<-0
train.sample<-train[c(13,3,6,7,8,10,2)]
View(train.sample)

train.sample$Live[train.sample$Survived==0]<-"Die"
train.sample$Live[train.sample$Survived==1]<-"Survived"
train.sample$Live<-factor(train.sample$Live)

xyplot(Age~Fare,data=train.sample,groups=Live,auto.key=list(corner=c(1,0)))

model1<-svm(Live~Age+Fare,data=train.sample)
plot(model1,train.sample,Age~Fare)

x<-train.sample[,c(3,6)]
y<-train.sample$Live

model3<-svm(x,y,type =NULL, kernel = "radial", degree = 3, gamma =if (is.vector(x)) 1 else 1 / ncol(x),cross=5)
pred<-predict(model3,x)
table(pred,y)
sum(pred==y)/length(y)
