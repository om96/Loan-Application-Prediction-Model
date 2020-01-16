###Loan Application Prediction Model
##Shresth Abrol, Trevor Baum, Hyung "Jay" "Ryan" Kim, Om Sharan

##Preparing & Exploring the data

#open the file
loan.df<- read.csv("C:\\Users\\om96\\OneDrive\\Documents\\UTD\\BA w R\\train_ctrUa4K.csv")

#explore data
summary(loan.df)

hist(as.numeric(loan.df$Loan_Amount_Term))

##Data Cleaning

#we have blank values which we are going to set to NA
loan.df[loan.df==""]<-NA

#Convert NA values of LoanAmount and Loan_Amount_Term to its median
loan.df$LoanAmount <- as.numeric(ifelse(is.na(loan.df$LoanAmount),median(loan.df$LoanAmount,na.rm = TRUE),loan.df$LoanAmount))
loan.df$Loan_Amount_Term <- as.numeric(ifelse(is.na(loan.df$Loan_Amount_Term),median(loan.df$Loan_Amount_Term, na.rm = T),loan.df$Loan_Amount_Term))

#convert 10 random NA to Male: Gender
loan.df$Gender <- as.factor(ifelse(loan.df$Gender =="Female",0,1))

#replacing NAs
nu<-loan.df[is.na(loan.df$Gender), ]

s <- sample(row.names(nu), 10)
s <- as.numeric(s)

loan.df [c(s),2] <- as.factor(1)
summary(loan.df)

#Convert the 3 remaning NA to Female: Gender
loan.df$Gender[is.na(loan.df$Gender)] <- 0


#Convert 2 random NA to married: Married
loan.df$Married<- as.factor(ifelse(loan.df$Married == "No",0,1))

nu1<-loan.df[is.na(loan.df$Married), ]

s1 <- sample(row.names(nu1), 2)
s1 <- as.numeric(s1)

loan.df [c(s1),3] <- as.factor(1)
summary(loan.df)

#Convert the 1 remaining NA to Not Married: Married
loan.df$Married[is.na(loan.df$Married)] <- 0

#Convert 2 random NA to self employed: Self_Employed
loan.df$Self_Employed<- as.factor(ifelse(loan.df$Self_Employed == "No",0,1))

nu2<-loan.df[is.na(loan.df$Self_Employed), ]

s2 <- sample(row.names(nu2), 30)
s2 <- as.numeric(s2)

loan.df [c(s2),6] <- as.factor(1)
summary(loan.df)

#Convert the remaining 2 NA to self employed as no: Self_Employed
loan.df$Self_Employed[is.na(loan.df$Self_Employed)] <- 0


#Convert 8 random NA to Credit History Rejected: Credit_History

nu3<-loan.df[is.na(loan.df$Credit_History), ]

s3 <- sample(row.names(nu3), 8)
s3 <- as.numeric(s3)

loan.df [c(s3),11] <- as.factor(0)
summary(loan.df)

#Convert the remaining 42 to Credit History Approved: Credit_History
loan.df$Credit_History[is.na(loan.df$Credit_History)] <- 1

# Converting 10 random NA to 0 Dependence: Dependents

nu4<-loan.df[is.na(loan.df$Dependents), ]

s4.0 <- sample(row.names(nu4), 10)
s4.0 <- as.numeric(s4.0)

loan.df [c(s4.0),4] <- as.factor(0)
summary(loan.df)

# Converting 2 random NA to 1 Dependence: Dependents

nu4.1<-loan.df[is.na(loan.df$Dependents), ]
s4.1 <- sample(row.names(nu4.1), 2)
s4.1 <- as.numeric(s4.1)

loan.df [c(s4.1),4] <- as.factor(1)
summary(loan.df)

# Converting 2 random NA to 2 Dependence: Dependents

nu4.2<-loan.df[is.na(loan.df$Dependents), ]
s4.2 <- sample(row.names(nu4.2), 2)
s4.2 <- as.numeric(s4.2)

loan.df [c(s4.2),4] <- as.factor(2)
summary(loan.df)

# Converting 1 random NA to 3+ Dependence: Dependents

nu4.3<-loan.df[is.na(loan.df$Dependents), ]
s4.3 <- sample(row.names(nu4.3), 1)
s4.3 <- as.numeric(s4.3)

loan.df [c(s4.3),4] <- as.factor("3+")
summary(loan.df)

#rename rows to the LoanID which is found in the first row and then remove that column
rownames(loan.df)<-loan.df[,1]
loan.df<-loan.df[,-1]

#explore classes of each variable using the str function
str(loan.df)

#we want to change the gender, married, dependents, self-employed and education into binary variables. 
loan.df$Education <- as.factor(ifelse(loan.df$Education == "Not Graduate",0,1))
loan.df$Loan_Status <- as.factor(ifelse(loan.df$Loan_Status == "N",0,1))

#we also want to create dummy variables for property area since it should not be levels
xtotal <- model.matrix(~ 0 +Property_Area, data = loan.df)
xtotal <- as.data.frame(xtotal[,-3])
loan.df.dummies<-data.frame(loan.df,(xtotal))

#drop the old property areas column
loan.df.dummies <- loan.df.dummies[,-11]
loan.df.dummies$Loan_Amount_Term<-as.factor(loan.df.dummies$Loan_Amount_Term)
loan.df.dummies$Loan_Amount_Term<-factor(loan.df$Loan_Amount_Term,levels=c(12,36,60,84,120,180,240,300,360,480))

loan.df.dummies$Credit_History<-as.factor((loan.df.dummies$Credit_History))

str(loan.df.dummies)

#make income data to annual and in dollars $USD since some were monthly or in $1000USD
loan.df.dummies$LoanAmount<-loan.df.dummies$LoanAmount*1000
loan.df.dummies$ApplicantIncome<-loan.df.dummies$ApplicantIncome*12
loan.df.dummies$CoapplicantIncome<-loan.df.dummies$CoapplicantIncome*12

#explore data distribution
boxplot(loan.df.dummies[6:8],ylab="Amount in $USD",main="Boxplots of Numerical Variables",col=c(4:6))
boxplot(loan.df.dummies$LoanAmount)

#data tranformations
loan.df.dummies$log_ApplicantIncome <- log(loan.df.dummies$ApplicantIncome)
loan.df.dummies$log_CoapplicantIncome <-ifelse(loan.df.dummies$CoapplicantIncome>0, log(loan.df.dummies$CoapplicantIncome),0)
loan.df.dummies$log_LoanAmount<- log(loan.df.dummies$LoanAmount)

boxplot(loan.df.rf[loan.df.rf$log_CoapplicantIncome>0,11:13],ylab=" Log Amount",main="Boxplots of Logged Numerical Variables",col=c(4:6))

#creating new variables
loan.df.dummies$monthly_est = loan.df.dummies$LoanAmount*(0.05*(1 + 0.05)^as.numeric(loan.df.dummies$Loan_Amount_Term))/((1 + 0.05)^as.numeric(loan.df.dummies$Loan_Amount_Term )- 1)
loan.df.dummies[is.na(loan.df.dummies$monthly_est),17]<-18008
loan.df.dummies$monthly_est<-log(loan.df.dummies$monthly_est)

hist(loan.df.dummies$monthly_est)

loan.df.dummies$dependents_new<-ifelse(loan.df.dummies$Dependents=="3+",3,as.numeric(loan.df.dummies$Dependents)-2)
loan.df.dummies$monthly_ratio<-((loan.df.dummies$monthly_est+1081*(loan.df.dummies$dependents_new))/(loan.df.dummies$ApplicantIncome+loan.df.dummies$CoapplicantIncome))
loan.df.dummies[is.na(loan.df.dummies$monthly_ratio),19]<-0.30
loan.df.dummies$monthly_ratio<-log(loan.df.dummies$monthly_ratio)


#remove variables we don't need since we transformed them
loan.df.rf<- loan.df.dummies[,c(-6:-8,-18)]

#exploring potential other data transformations
loan.df.rf %>%
  group_by(Dependents==0) %>%
  summarise(number = n(),perc_of_loans_approved = mean(as.numeric(Loan_Status)-1,na.rm = T))


loan.df.rf %>%
  group_by(log_CoapplicantIncome>0) %>%
  summarise(number = n(),perc_of_loans_approved = mean(as.numeric(Loan_Status)-1,na.rm = T))

boxplot(loan.df.rf$Loan_Status,loan.df.rf$log_ApplicantIncome+loan.df.rf$log_CoapplicantIncome, main = "Boxplots of Loan Status vs Total Log Income", ylab="Log Applicant Income + Log Co-Applicant Income",xlab="Loan")


#create training and validating datasets
set.seed(1)  
train.index <- sample(c(1:dim(loan.df.rf)[1]), dim(loan.df.rf)[1]*0.6)  #creates a random sample. first val is the data, second val is number of rows. here we have it be 60% of the dataset
train.df <- loan.df.rf[train.index, ] #the sample (60% of the data) is sent to training dataset
valid.df <- loan.df.rf[-train.index, ] #the other 40% is sent to test dataset


## create decision tree
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(dplyr)

default.ct <- rpart(Loan_Status ~ ., data = train.df ,method = "class")

# plot tree
prp(default.ct, type = 1, extra = 2, under = TRUE, split.font = 1, varlen = -10) #plots tree prp(rpart obj, type = labels, extra= extra labels, under = T/F to put labels under the box, varlen = how many to chars to show)
#find tree length
length(default.ct$frame$var[default.ct$frame$var == "<leaf>"])

# use the decision tree to predict on the training dataset
default.ct.point.pred.train <- predict(default.ct,train.df,type = "class") #predict(rules,training, type="class")
# generate confusion matrix for training data
confusionMatrix(default.ct.point.pred.train, as.factor(train.df$Loan_Status))

# repeat the code for the validation set, and the deeper tree
default.ct.point.pred.valid <- predict(default.ct,valid.df,type = "class")
confusionMatrix(default.ct.point.pred.valid, as.factor(valid.df$Loan_Status))

## create random forest
library(randomForest)

rf <- randomForest(as.factor(Loan_Status) ~ ., data = train.df, ntree = 10000, 
                    nodesize = 7, importance = TRUE)  

# variable importance plot
varImpPlot(rf, type = 1) #shows most important variables

# confusion matrix
rf.pred <- predict(rf, valid.df)
confusionMatrix(rf.pred, as.factor(valid.df$Loan_Status))

## create logistic regression
logit.reg <- glm(Loan_Status ~ ., data = train.df, family = "binomial") 

summary(logit.reg)

#improving the logit model by using variable selection

#running the stepwise selection process
smallest <- Loan_Status ~ 0
biggest <- Loan_Status ~ Credit_History + monthly_est + log_LoanAmount + 
  log_ApplicantIncome + monthly_ratio + log_CoapplicantIncome + 
  Property_AreaSemiurban+(log_ApplicantIncome)+(log_LoanAmount)+(log_CoapplicantIncome)

m <- glm(Loan_Status ~ Credit_History, data=train.df,family = "binomial")
stats::step(m, scope=list(lower=smallest, upper=biggest))

#creating our optimal logit model
best_lm<-glm(Loan_Status ~ Credit_History + Property_AreaSemiurban + (log_CoapplicantIncome), family = "binomial", data = train.df)

summary(best_lm)
plot(best_lm)

#using the logit regression to predict the training dataset
logit.reg.pred.train <- predict(best_lm, train.df[, -8], type = "response",na.action = na.pass)
logit.reg.pred.train<-as.factor(ifelse(logit.reg.pred.train>=0.5,1,0))

confusionMatrix(logit.reg.pred.train, as.factor(train.df$Loan_Status))

#removing two levels as they sometimes aren't accounted for depending on the sample/split of the training & valid datasets
valid.df<-valid.df[valid.df$Loan_Amount_Term!=60,]
valid.df<-valid.df[valid.df$Loan_Amount_Term!=240,]

#using the logit regression to predict the valid dataset
logit.reg.pred.valid <- predict(best_lm, valid.df[, -8], type = "response",na.action = na.pass)
logit.reg.pred.valid<-as.factor(ifelse(logit.reg.pred.valid>=0.5,1,0))

confusionMatrix(logit.reg.pred.valid, as.factor(valid.df$Loan_Status))

## occassionally the confusion matrix would result in an error. therefore we replicated how to find the accuracy and misclassification
## rates using the accuracy_logit table created below

#accuracy_logit<-data.frame(actual = train.df$Loan_Status, predicted = logit.reg.pred.train)
# accuracy_logit$bin_pred<-ifelse(accuracy_logit$predicted==1,1,0)
# accuracy_logit$correct<-ifelse(accuracy_logit$actual==accuracy_logit$bin_pred,1,0)
# mean(accuracy_logit$correct)
# 
# accuracy_logit %>%
#   group_by(bin_pred) %>%
#   summarise(number = n(),accuracy = mean(correct,na.rm = T))

## the same process now for the valid dataset
# accuracy_logit_v<-data.frame(actual = valid.df$Loan_Status, predicted = logit.reg.v.pred)
# 
# accuracy_logit_v$bin_pred<-ifelse(accuracy_logit_v$predicted>=0.5,1,0)
# accuracy_logit_v$correct<-ifelse(accuracy_logit_v$actual==accuracy_logit_v$bin_pred,1,0)
# mean(accuracy_logit_v$correct)
# 
# accuracy_logit_v %>%
#   group_by(bin_pred) %>%
#   summarise(number = n(),accuracy = mean(correct,na.rm = T))

## creating the neural network
library(neuralnet)

#use our categorical variables to create a numeric matrix
l_matrix <- model.matrix(~Gender+Married+Dependents+Education
                           +Self_Employed+Property_AreaRural+Property_AreaSemiurban+Credit_History+
                             Loan_Status, data=valid.df)
colnames(l_matrix)
#change column names in order to run the formula function
colnames(l_matrix)[7] <- "Dependents3ormore"
colnames(l_matrix)[10] <- "EducationNotGraduate"

#create list of columns which will be used for the formula
col_list <- paste(c(colnames(l_matrix[,-c(1,13)]),c(colnames(valid.df[,11:15]))),collapse="+")
col_list <- paste(c("Loan_Status1~",col_list),collapse="")

#create formula
f <- formula(col_list)
f

#attach numerical variables to matrix
loan_matrix<-cbind(l_matrix[,-1],valid.df[,11:15])

#create and run neural network
nmodel <- neuralnet(f,data=loan_matrix,hidden=5,
                    threshold = 0.01,
                    learningrate.limit = NULL,
                    learningrate.factor =
                      list(minus = 0.5, plus = 1.2),
                    algorithm = "rprop+")

#plot neural network
plot(nmodel)


##create combined model
rf.pred_v <- predict(rf, valid.df)
both_models<-data.frame(actual = valid.df$Loan_Status, logit = logit.reg.v.pred,rf = rf.pred_v)
both_models$logit.b<-ifelse(both_models$logit>=0.5,1,0)
both_models$match<-ifelse(both_models$logit.b==both_models$rf,"match","no match")
both_models$correct<-ifelse(both_models$match=="match",ifelse(both_models$rf==both_models$actual,1,0),NA)
both_models$logit_correct<-ifelse(both_models$actual==both_models$logit.b,1,0)
both_models$rf_correct<-ifelse(both_models$actual==both_models$rf,1,0)
both_models$nn_pred<-pred
both_models$all_match<-ifelse(both_models$match=="match" &both_models$logit.b==both_models$nn_pred,"match","no match")
both_models$all_correct<-ifelse(both_models$all_match=="match",ifelse(both_models$nn_pred==both_models$actual,1,0),NA)
both_models$nn_correct<-ifelse(both_models$actual==both_models$nn_pred,1,0)
both_models$matching<-ifelse(both_models$all_match=="match","all",ifelse(both_models$match=="match","logit&rf",ifelse(both_models$nn_pred==both_models$logit.b,"logit&nn","rf&nn")))
both_models$rfnn_correct<-ifelse(both_models$matching=="match" | both_models$matching=="rf&nn",both_models$rf_correct,NA)
both_models$logitnn_correct<-ifelse(both_models$matching=="match" | both_models$matching=="logit&nn",both_models$logit_correct,NA)

#neural network overall accuracy
mean(both_models$nn_correct)

#compare positive and negative prediction accuracy between all models
both_models %>%
  group_by(nn_pred) %>%
  summarize(n=n(),accuracy_logit = mean(logit_correct,na.rm = TRUE),accuracy_rf = mean(rf_correct,na.rm=TRUE),accuracy_nn = mean(nn_correct,na.rm = TRUE))

#see how many times the models match
both_models %>%
  group_by(matching) %>%
  summarise(n=n())

#accuracy dependent on matching
both_models %>%
  group_by(matching) %>%
  summarize(n=n(),accuracy_logit = mean(logit_correct,na.rm = TRUE),accuracy_rf = mean(rf_correct,na.rm=TRUE),accuracy_nn = mean(nn_correct,na.rm = TRUE))

#accuracy dependent on matching and whether the model predicts approve or decline
both_models %>%
  group_by(matching,nn_pred) %>%
  summarize(n=n(),accuracy_logit = mean(logit_correct,na.rm = TRUE),accuracy_rf = mean(rf_correct,na.rm=TRUE),accuracy_nn = mean(nn_correct,na.rm = TRUE))
