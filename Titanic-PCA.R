train<-read.csv("E:/GOM/EMP 508/FINAL PROJECT/titanic/train.csv")
train<-na.omit(train)
View(train)

train<-as.data.frame(train)
train$Gender[train$Sex=="male"]<-1
train$Gender[train$Sex=="female"]<-2

train[is.na(train)]<-0
train.sample<-train[c(13,3,6,7,8,10)]
View(train.sample)

train.samplepr<-princomp(train.sample)
summary(train.samplepr)
