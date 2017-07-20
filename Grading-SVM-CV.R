library(e1071)
library(lattice)
library(readxl)
grade <- read_xlsx("E:/GOM/EMP 508/FINAL PROJECT/grade.xlsx")

grade$LetterGrade[is.na(grade$LetterGrade)]<-0
grade$LetterGrade[grade$TotalGrade<1000]<-"A"
grade$LetterGrade[grade$TotalGrade<93.5]<-"A-"
grade$LetterGrade[grade$TotalGrade<89.5]<-"B+"
grade$LetterGrade[grade$TotalGrade<86.5]<-"B"
grade$LetterGrade[grade$TotalGrade<82.5]<-"B-"
grade$LetterGrade[grade$TotalGrade<79.5]<-"C+"
grade$LetterGrade[grade$TotalGrade<76.5]<-"C"
grade$LetterGrade[grade$TotalGrade<72.5]<-"C-"
grade$LetterGrade[grade$TotalGrade<69.5]<-"D"
grade$LetterGrade[grade$TotalGrade<60]<-"F"

grade.sample<-grade[c(1:7,20,101)]

grade.sample<-na.omit(grade.sample) #remove all the na.value

xyplot(TotalTimeStudentsspentinCourseinHours~TotalDBMFhits,data=grade,groups = LetterGrade, auto.key=list(corner=c(1,0)))

grade.sample$LetterGrade<-factor(grade.sample$LetterGrade)

model1<-svm(LetterGrade~TotalTimeStudentsspentinCourseinHours+TotalDBMFhits,data = grade.sample)
plot(model1, grade.sample, TotalTimeStudentsspentinCourseinHours~TotalDBMFhits)

grade.sample$TotalTimeStudentsspentinCourseinHours<-as.numeric(grade.sample$TotalTimeStudentsspentinCourseinHours)
grade.sample$TotalDBMFhits<-as.numeric(grade.sample$TotalDBMFhits)
grade.sample$LetterGrade<-as.factor(grade.sample$LetterGrade)

x<-grade.sample[,c(8,9)]

y<-grade.sample$LetterGrade

model3<-svm(x(),y=y, type = NULL, kernel = "radial", degree = 3, gamma =if (is.vector(x)) 1 else 1 / ncol(x),cross=5)

pred<-predict(model3,x)
table(pred,y)
sum(pred==y)/length(y)
