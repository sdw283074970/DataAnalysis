grade<-KAdebowaleGender_SBU_original_050117_test_1_1_
View(grade)
summary(grade)
str(grade)

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

grade[is.na(grade)]<-0

grade.sample<-grade[c(8:164)]

grade.sample<-data.frame(grade.sample)

grade.sample<-grade.sample[which(colSums(grade.sample)!=0)]

grade.pr<-princomp(grade.sample,cor=F)

summary(grade.pr)

View(grade.sample)

grade.hits<-grade[c(7,101)]
cor(grade.hits) #correlation between the final grade and total hits
grade.ttime<-grade[c(7,20)]
cor(grade.ttime) #correlation between the final grade and total times
grade.hitstimes<-grade[c(7,20,101)] #correlation between the final grade and total times and total hits
cor(grade.hitstimes)

grade.totalpr<-princomp(grade.hitstimes[c(2,3)],cor=T) #apply PCA
summary(grade.totalpr)
