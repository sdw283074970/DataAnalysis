library(readxl)
library(e1071)
library(lattice)
approval<- read_excel("E:/GOM/EMP 508/FINAL PROJECT/LendingApproval/LoanStats3a.xlsx")
View(approval)

approval.sample<-approval[c(9,17,3,4,6,13,14,15)]

#clean data
approval.sample$home_ownership[approval.sample$home_ownership=="NONE"]<-0
approval.sample$home_ownership[approval.sample$home_ownership=="OTHER"]<-1
approval.sample$home_ownership[approval.sample$home_ownership=="RENT"]<-2
approval.sample$home_ownership[approval.sample$home_ownership=="MORTGAGE"]<-3
approval.sample$home_ownership[approval.sample$home_ownership=="OWN"]<-4
approval.sample$home_ownership<-as.numeric(approval.sample$home_ownership)

approval.sample$term[approval.sample$term=="36 months"]<-0
approval.sample$term[approval.sample$term=="60 months"]<-1
approval.sample$term<-as.numeric(approval.sample$term)

approval.sample$verification_status[approval.sample$verification_status=="Not Verified"]<-0
approval.sample$verification_status[approval.sample$verification_status=="Source Verified"]<-1
approval.sample$verification_status[approval.sample$verification_status=="Verified"]<-2
approval.sample$verification_status<-as.numeric(approval.sample$verification_status)

approval.sample$loan_status[approval.sample$loan_status=="Charged Off"]<-2
approval.sample$loan_status[approval.sample$loan_status=="Fully Paid"]<-3
approval.sample$loan_status[approval.sample$loan_status=="Does not meet the credit policy. Status:Charged Off"]<-0
approval.sample$loan_status[approval.sample$loan_status=="Does not meet the credit policy. Status:Fully Paid"]<-1
approval.sample$loan_status<-as.numeric(approval.sample$loan_status)


#PCA
approval.sample<-na.omit(approval.sample)
approval.sample<-as.data.frame(approval.sample)
approval.samplepc<-approval.sample[c(-1)]
approval.samplePR<-prcomp(~., data=approval.samplepc,scale = T, center = T)
summary(approval.samplePR)
approval.samplePR2<-princomp(approval.sample[c(2:8)],cor=T)
summary(approval.samplePR2) ##annual_inc and loan_amnt are the two most important variances

#SVM
xyplot(loan_amnt~annual_inc, data = approval.sample, groups = grade,auto.key=list(corner=c(1,0)))
approval.sample$grade<-factor(approval.sample$grade)
## Warning: these following entries may crush computer
##model1<-svm(grade~loan_amnt+annual_inc,data=approval.sample)
##plot(model1,approval.sample,loan_amnt~annual_inc)

x<-approval.sample[,c(2,8)]
y<-approval.sample$grade

## Warning: these following entries may crush computer
model3<-svm(x,y,type = NULL, kernel = "radial", degree = 3, gamma =if (is.vector(x)) 1 else 1 / ncol(x),cross=5)
pred<-predict(model3,x)
table(pred,y)
sum(pred==y)/length(y)
