#Library
library("caret")
#install.packages("RANN")
library("RANN")
#Load Data
train<-read.csv("Titanic Dataset\\train.csv",stringsAsFactors = T,na.strings=c(""," ","NA"))

#Data Pre - Processing
str(train)
train$PassengerId <- NULL

sum(is.na(train))

#Data Exploration Analysis
colnames(train)
View(train)

#predictor/trained

  #survived - 0 no ,1 yes

  #filtering ship details relatives

    #pclass <- Ticket class
    #Ticket <- Ticket number
    #Fare<- Price
    #cabin<-cabin number
    #embark<-port of embarkment

  #filtering passenger relatives

    #Name<-name
    #Age<-age
    #sex
    #sibsp<-siblings/sprouse
    #parch<-parent/children


#Survived vs pclass
train$Survived<-as.factor(train$Survived)
ggplot(train)+
  geom_bar(aes(Pclass,color=Survived,fill=Survived),position = "dodge")
  #Class - 1 - Higher Suvived
  #Class - 2 - Neutral Survived/Death
  #Class - 3 - Higher Death

#Survived vs Tickets
train$Ticket
data<-dplyr::arrange(train,desc(Ticket))
levels(data$Ticket)<-c(levels(data$Ticket),"Ordinary")
data$Ticket[230:891]<- "Ordinary"
data$Ticket<-factor(data$Ticket)

data$Ticket<-as.character(data$Ticket)
head(data$Ticket)
data$Ticket[grepl("P",data$Ticket)]
data$Ticket[grepl("WE/P|W.E.P",data$Ticket)]<-"W.E.P"
data$Ticket[grepl("W./C.|W/C",data$Ticket)]<-"W.C"
data$Ticket[grepl("SW/PP|S.W./PP",data$Ticket)]<-"S.W.P"
data$Ticket[grepl("STON/",data$Ticket)]<-"S.T.O.N"
data$Ticket[grepl("SC/|SO",data$Ticket)]<-"S.C"
data$Ticket[grepl("S.O",data$Ticket)]<-"S.O"
data$Ticket[grepl("S.P",data$Ticket)]<-"S.P"
data$Ticket[grepl("PP|P/P",data$Ticket)]<-"PP"
data$Ticket[grepl("PC",data$Ticket)]<-"PC"
data$Ticket[grepl("LINE",data$Ticket)]<-"LINE"
data$Ticket[grepl("Fa",data$Ticket)]<-"Fa"
data$Ticket[grepl("F.C",data$Ticket)]<-"FC"
data$Ticket[grepl("C.A",data$Ticket)]<-"C.A"
data$Ticket[grepl("C|C ",data$Ticket)]<-"C"
data$Ticket[grepl("A|A/",data$Ticket)]<-"A"
train$Ticket_G<-as.factor(data$Ticket)
levels(train$Ticket_G)

ggplot(train)+
  geom_bar(aes(Ticket_G,color=Survived,fill=Survived),position = "dodge")

 #Ordinary Ticket- Higher Death

#Survived Vs Embarked
train$Embarked[which.max(train$Embarked)]
train$Embarked[train$Embarked == ""]<-"S"

ggplot(train)+
  geom_bar(aes(Embarked,color=Survived,fill=Survived),position = "dodge")

 #C - High Survived
 #Q - High Death
 #s - Hight Death

#Survived vs Cabin
train$Cabin<-as.character(train$Cabin)
train$Cabin[grepl("A",train$Cabin)]<-"A"
train$Cabin[grepl("B",train$Cabin)]<-"B"
train$Cabin[grepl("C",train$Cabin)]<-"C"
train$Cabin[grepl("D",train$Cabin)]<-"D"
train$Cabin[grepl("E",train$Cabin)]<-"E"
train$Cabin[grepl("F",train$Cabin)]<-"F"
train$Cabin[grepl("G|T",train$Cabin)]<-"G"
train$Cabin[is.na(train$Cabin)]<-"U"

ggplot(train)+
  geom_bar(aes(Cabin,color=Survived,fill=Survived),position = "dodge")
  #Cabin U -high death
  #Cabin B,C,D,E,F-Hight Survial Rate

#Survived Vs Name
train$Name<-as.character(train$Name)
train$Name<-gsub("(.*, |\\..*)","",train$Name)


train$Name[grepl("Mlle|Mme|Ms",train$Name)]<-"Miss"
train$Name[grepl("Dona|Lady|the Countess|Capt|Col|Don|Dr|Major|Rev|Sir|Jonkheer",train$Name)]<-"Rare"
table(train$Name)


ggplot(train)+
  geom_bar(aes(Name,color=Survived,fill=Survived),position = "dodge")
#married women/Single Women/Single Men - high Survivel
#Mr.-High dead

#Survival vs Age.
train$Age[is.na(train$Age) & train$Parch > 0 ]<- 9
train$Age[is.na(train$Age) & train$Parch == 0 ]<- 18
train$Age[is.na(train$Age) & train$family == 1 ]<- 18
train$Age[is.na(train$Age) & train$SibSp >0 ]<- 18
train$AgeGroup[train$Age >= 18]<-"Adult"
train$AgeGroup[train$Age < 18]<-"Child"

ggplot(train)+
  geom_bar(aes(AgeGroup,color=Survived,fill=Survived),position = "dodge")
#20-40 Most Traveller Age
#20-40 High Death 

#Survival Vs Sex
ggplot(train)+
  geom_bar(aes(Sex,color=Survived,fill=Survived),position = "dodge")
#Men hight Death
#Female High Survival

#Suvival vs Sibsp
ggplot(train)+
  geom_bar(aes(SibSp,color=Survived,fill=Survived),position = "dodge")

#Survival vs ParCh
ggplot(train)+
  geom_bar(aes(Parch,color=Survived,fill=Survived),position = "dodge")

train$family<-train$SibSp+train$Parch+1

ggplot(train)+
  geom_bar(aes(family,color=Survived,fill=Survived),position = "dodge")

#survival vs family
train$familyD[train$family == 1]<-"small"
train$familyD[train$family > 1 & train$family < 5 ]<-"medium"
train$familyD[train$family > 4]<-"large"

ggplot(train)+
  geom_bar(aes(familyD,color=Survived,fill=Survived),position = "dodge")
#medium -high survival
#large -high death
#small -high death



#Dummy Variable Creation
?dummyVars
train$Ticket<-NULL
dmy<-dummyVars("~.",train,fullRank = TRUE)
train_transformed<-data.frame(predict(dmy,train))
View(train_transformed)

#Splitting Data
index<-createDataPartition(train_transformed$Survived.1,p=0.75,list=FALSE)
trainSet<-train_transformed[index,]
testSet<-train_transformed[-index,]
trainSet<-na.omit(trainSet)
#Feature Importance

control<-rfeControl(functions = rfFuncs,method = "repeatedcv",repeats = 3,verbose = FALSE)
predictors<-names(trainSet)[!names(trainSet) %in% "Survived.1"]
select_pred<-dplyr::select(trainSet,-Survived.1)

FS<-rfe(select_pred,trainSet[,"Survived.1"],rfeControl = control)
FS

var_sec<-c("Fare","Pclass","Sex.male","family","NameMr","NameMrs","NameMiss","NameRare")

#Model Training

#logestic<-glm(Survived ~ .,data=train,family="binomial")
trainSet$Survived.1<-as.factor(trainSet$Survived.1)
model_glm<-train(trainSet[,var_sec],trainSet[,"Survived.1"],method="glm")
model_rf<-train(trainSet[,var_sec],trainSet[,"Survived.1"],method="rf")

#Summery of Model And future Importance
varImp(model_glm)
plot(varImp(model_glm))
varImp(model_rf)
plot(varImp(model_rf))

#Model Testing
testSetNew<-dplyr::select(testSet,var_sec)
predictions_glm<-predict.train(object=model_glm,testSetNew,type="raw")
predictions_rf<-predict.train(object=model_rf,testSetNew,type="raw")
#Model Parmeters
table(predictions_glm)
table(predictions_rf)
confusionMatrix(predictions_rf,testSet[,"Survived.1"])
#0.7973

#Result
write.csv(predictions_rf,'test.csv')
