#http://www.learnbymarketing.com/tutorials/neural-networks-in-r-tutorial/
#open the file
loan.df<- read.csv("C:\\Users\\om96\\OneDrive\\Documents\\UTD\\BA w R\\train_ctrUa4K.csv")

#explore data
summary(loan.df)

#we have blank values which we are going to set to NA
loan.df[loan.df==""]<-NA

loan.df<-na.omit(loan.df)

#rename rows to the LoanID which is found in the first row and then remove that column
rownames(loan.df)<-loan.df[,1]
loan.df<-loan.df[,-1]
loan.df$Credit_History <- as.factor(loan.df$Credit_History)

str(loan.df)

#As neural networks use activation functions between -1 and +1, it's important to scale your variables down. Otherwise, the neural network will have to spend training iterations doing that scaling for you.

#Min Max Normalization
loan.df$ApplicantIncome <-(loan.df$ApplicantIncome-min(loan.df$ApplicantIncome)) /
  (max(loan.df$ApplicantIncome)-min(loan.df$ApplicantIncome))
loan.df$CoapplicantIncome <- (loan.df$CoapplicantIncome-min(loan.df$CoapplicantIncome)) / (max(loan.df$CoapplicantIncome)-min(loan.df$CoapplicantIncome))
loan.df$LoanAmount <- (loan.df$LoanAmount-min(loan.df$LoanAmount)) /
  (max(loan.df$LoanAmount)-min(loan.df$LoanAmount))
loan.df$Loan_Amount_Term <- (loan.df$Loan_Amount_Term-min(loan.df$Loan_Amount_Term)) /
  (max(loan.df$Loan_Amount_Term)-min(loan.df$Loan_Amount_Term))

str(loan.df)

#set.seed(1)
#train.index <- sample(c(1:dim(loan.df)[1]), dim(loan.df)[1]*0.6)  #creates a random sample. first val is the data, second val is number of rows. here we have it be 60% of the dataset
#train.df <- loan.df[train.index, ] #the sample (60% of the data) is sent to training dataset
#valid.df <- loan.df[-train.index, ] #the other 40% is sent to test dataset

#In order to represent factor variables, we need to convert them into dummy variables. 
#table(loan.df$Gender)
#levels(loan.df$Gender)

#In order to make this factor variable useful for the neuralnet package, we need to use the model.matrix() function.

#head(model.matrix(~Gender, data=loan.df))

#loan.df$Gender <- relevel(loan.df$Gender, ref = "Female")
#head(model.matrix(~Gender, data=loan.df))



bnk_matrix <- model.matrix(~Gender+Married+Dependents+Education
                           +Self_Employed+Property_Area+Credit_History+
                             Loan_Status, data=loan.df)
colnames(bnk_matrix)
colnames(bnk_matrix)[9] <- "Dependents3ormore"
colnames(bnk_matrix)[10] <- "EducationNotGraduate"

col_list <- paste(c(colnames(bnk_matrix[,-c(1,16)]),c(colnames(loan.df[,6:9]))),collapse="+")
col_list <- paste(c("Loan_StatusY~",col_list),collapse="")
f <- formula(col_list)
f
library(neuralnet)

loan_matrix<-cbind(bnk_matrix,loan.df[,6:9])

nmodel <- neuralnet(f,data=loan_matrix,hidden=1,
                    threshold = 0.01,
                    learningrate.limit = NULL,
                    learningrate.factor =
                      list(minus = 0.5, plus = 1.2),
                    algorithm = "rprop+")

plot(nmodel)

Predict=compute(nmodel,loan_matrix)
Predict$net.result
prob <- Predict$net.result
pred <- ifelse(prob>=0.5, 1, 0)
pred

loan.df$Prediction <- pred
loan.df$Loan_Status <- ifelse(loan.df$Loan_Status=="Y",1,0)
loan.df$Prediction <- as.factor(loan.df$Prediction)
loan.df$Loan_Status <-as.factor(loan.df$Loan_Status)

View(loan.df)
loan.df$Result <- ifelse(loan.df$Prediction==loan.df$Loan_Status,1,0)

summary(loan.df$Result)

      