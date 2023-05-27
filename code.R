#Q370 Final Project Code
#Robert Kellems

library(dplyr)
library(ez)
library(ggplot2)

data = read.csv('responses.csv')
useData = data.frame(matrix(ncol=6,nrow=0))
colnames(useData) = c('subject','experience','clip','vocals','response','correctResponse')

vocals = c('y','y','n','n','n','y','y','n')
correctResponses = c(6,5,5,4,3,6,4,5)
for (i in 1:nrow(data)) {
  for (j in 1:8) {
    df = data.frame(data[i,2],data[i,3],j,vocals[j],data[i,j+3],correctResponses[j])
    names(df) = c('subject','experience','clip','vocals','response','correctResponse')
    useData = rbind(useData,df)
  }
}
useData$error = useData$response-useData$correctResponse
useData$absError = abs(useData$error)


#Normal error
#do more experienced musicians have better performance?
bySeveral = group_by(useData,subject)
averageError = summarize(bySeveral,experience=mean(experience),error=mean(error))
plot(averageError$experience,averageError$error)
linModel = lm(error ~ experience, useData) #linear regression
linModel
abline(linModel, col="red")
cor.test(averageError$experience,averageError$error) #pearson correlation coefficient

#are less experienced musicians more affected by presence of vocals?
anova = ezANOVA(useData,error,subject,within=vocals,between=experience)
anova
ggplot(useData,aes(x=vocals,y=error))+stat_summary(fun=mean,geom='bar',)+stat_summary(fun.data=mean_cl_normal,geom="errorbar")+xlab("Vocals")+ylab("Error") #vocals comparison
ggplot(useData,aes(x=experience,y=error,fill=factor(vocals)))+stat_summary(fun=mean,geom='bar',position = 'dodge')+xlab("Experience Level")+ylab("Error")+scale_fill_discrete(name="Vocals",labels=c('No','Yes'))


#Absolute error
#do more experienced musicians have better performance?
bySeveral = group_by(useData,subject)
averageError = summarize(bySeveral,experience=mean(experience),absError=mean(absError))
plot(averageError$experience,averageError$absError)
linModel = lm(absError ~ experience, useData) #linear regression
linModel
abline(linModel, col="red")
cor.test(averageError$experience,averageError$absError) #pearson correlation coefficient

#are less experienced musicians more affected by presence of vocals?
anova = ezANOVA(useData,absError,subject,within=vocals,between=experience)
anova
table1 = tapply(X=useData$absError,INDEX=list(useData$experience),FUN=mean)
table2 = tapply(X=useData$absError,INDEX=list(useData$vocals),FUN=mean)

ggplot(useData,aes(x=vocals,y=absError))+stat_summary(fun=mean,geom='bar',)+stat_summary(fun.data=mean_cl_normal,geom="errorbar")+xlab("Vocals")+ylab("Absolute Error") #vocals comparison
ggplot(useData,aes(x=experience,y=absError,fill=factor(vocals)))+stat_summary(fun=mean,geom='bar',position = 'dodge')+xlab("Experience Level")+ylab("Absolute Error")+scale_fill_discrete(name="Vocals",labels=c('No','Yes'))
