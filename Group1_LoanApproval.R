rm( list=ls() )
install.packages("readr")    
install.packages("dplyr")    
install.packages("tidyverse") 
install.packages("ggplot2")
install.packages("lattice")

Loan <- read.csv("~/Downloads/archive/Loan.csv")

dim(Loan) #Dimensions of "Loan": 20000 rows, 36 columns
str(Loan)
summary(Loan)

----#DATA PREPOCESSING#------------------

#Check missing values pattern by heatmap
install.packages("Amelia")
library(Amelia)
missmap(Loan)

# Boxplots of all continuous variables BEFORE removing outliers
continuous_columns <- names(Loan)[sapply(Loan, is.numeric)]

par(mfrow = c(4,8 ))
for (col in continuous_columns) {
  boxplot(Loan[[col]], main = paste("Before:", col))
}

library(dplyr)

target_var <- "LoanApproved"
continuous_columns <- setdiff(continuous_columns, target_var)

Loan_original <- Loan  # Save original before removing outliers

remove_outliers <- function(data, col) {
  Q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  data <- data %>% filter(.data[[col]] >= lower_bound & .data[[col]] <= upper_bound)
  return(data)
}

# Loop through continuous columns excluding the target
for (col in continuous_columns) {
  if (col %in% names(Loan)) {
    Loan <- remove_outliers(Loan, col)
  }
}

# Compare before and after
cat("Original rows:", nrow(Loan_original), "\n")
cat("Rows after outlier removal:", nrow(Loan), "\n")
cat("Total rows removed:", nrow(Loan_original) - nrow(Loan), "\n")

# Boxplots of all continuous variables AFTER removing outliers
par(mfrow = c(4, 8))  # Adjust based on number of variables
for (col in continuous_columns) {
  if (col %in% names(Loan)) {
    boxplot(Loan[[col]], main = paste("After:", col))
  }
}

##CATEGORICAL VARIABLES

# Step 1: Check frequency tables
table(Loan$EmploymentStatus)
table(Loan$EducationLevel)
table(Loan$MaritalStatus)
table(Loan$HomeOwnershipStatus)
table(Loan$LoanPurpose)

# Step 2: Combine rare categories into 'Other'
combine_rare_levels <- function(column, threshold = 0.05) {
  freq <- prop.table(table(column))
  rare_levels <- names(freq[freq < threshold])
  new_col <- as.character(column)
  new_col[new_col %in% rare_levels] <- "Other"
  return(as.factor(new_col))
}

Loan$EmploymentStatus     <- combine_rare_levels(Loan$EmploymentStatus)
Loan$EducationLevel       <- combine_rare_levels(Loan$EducationLevel)
Loan$MaritalStatus        <- combine_rare_levels(Loan$MaritalStatus)
Loan$HomeOwnershipStatus  <- combine_rare_levels(Loan$HomeOwnershipStatus)
Loan$LoanPurpose          <- combine_rare_levels(Loan$LoanPurpose)

# Double check after combining
table(Loan$EmploymentStatus)
table(Loan$EducationLevel)
table(Loan$MaritalStatus)
table(Loan$HomeOwnershipStatus)
table(Loan$LoanPurpose)

# Step 3: Create dummy variables
library(fastDummies)
Loan_clean <- dummy_columns(Loan, 
                            select_columns = c('EmploymentStatus', 'EducationLevel', 
                                               'MaritalStatus', 'HomeOwnershipStatus', 'LoanPurpose'),
                            remove_most_frequent_dummy = TRUE,
                            remove_selected_columns = TRUE)


##MULTICOLLINEARITY CHECK
install.packages("corrplot")
install.packages("caret")
library(ggplot2)
library(lattice)
library(corrplot)
library(caret)

# Step 1: Keep only numeric variables (excluding date or non-numeric fields)
loan_numeric <- Loan_clean[, !names(Loan_clean) %in% c("ApplicationDate")]
loan_numeric <- loan_numeric[sapply(loan_numeric, is.numeric)]
#Warning message: In cor(loan_numeric, use = "complete.obs") : the standard deviation is zero
# Check the standard deviation for each column
constant_columns <- sapply(loan_numeric, function(x) sd(x, na.rm = TRUE) == 0)

# Get the names of constant columns
constant_column_names <- names(loan_numeric)[constant_columns] #[1] "BankruptcyHistory"    "PreviousLoanDefaults"
#Remove constant columns (with zero standard deviation)
loan_numeric <- loan_numeric[, sapply(loan_numeric, function(x) sd(x, na.rm = TRUE) > 0)]

# Step 2: Correlation matrix
cor_matrix <- cor(loan_numeric, use = "complete.obs")

# Step 3: Visualize correlations
corrplot(cor_matrix, method = "color", tl.cex = 0.6, number.cex = 0.7, type = "upper")

# Step 4: Identify highly correlated variables
high_corr <- findCorrelation(cor_matrix, cutoff = 0.8, names = TRUE)

print("Highly correlated variables to remove:")  # Removing Multicollinearity
print(high_corr)
#[1] "RiskScore"        "BaseInterestRate" "MonthlyIncome"    "Age"      "LoanAmount"       "NetWorth"

# Step 5: Remove highly correlated variables
loan_filtered <- loan_numeric[, !(names(loan_numeric) %in% high_corr)]

# Step 6: Plot filtered correlation matrix
cor_matrix_filtered <- cor(loan_filtered, use = "complete.obs")
corrplot(cor_matrix_filtered, method = "color", tl.cex = 0.6, number.cex = 0.7, type = "upper")

# Step 7: Show before/after column count
length(high_corr)               # Number of variables removed
ncol(loan_numeric)              # Before
ncol(loan_filtered)            # After

## Final: Save cleaned dataset
Loan <- loan_filtered

#Export - to be used for Random Forest (separated Rscript)
write.csv(loan_final, file = "loan_final.csv", row.names = FALSE)

-----------#LOGISTIC REGRESSION#-------------------

library(dplyr)
library(ggplot2)

set.seed(123) 

sample_index <- sample(1:nrow(Loan), 0.7 * nrow(Loan))

train <- Loan[sample_index, ]  
test <- Loan[-sample_index, ] 

full_model <- glm(LoanApproved ~ ., data = train, family = "binomial")
summary(full_model)

# Forward
forward_model <- step(glm(LoanApproved ~ 1, data = train, family = "binomial"),
                      scope = list(lower = ~1, upper = formula(full_model)),
                      direction = "forward")

# Backward
backward_model <- step(full_model, direction = "backward")

# Stepwise
stepwise_model <- step(glm(LoanApproved ~ 1, data = train, family = "binomial"),
                       scope = list(lower = ~1, upper = formula(full_model)),
                       direction = "both")

pred_forward <- predict(forward_model, newdata = test, type = "response")
pred_backward <- predict(backward_model, newdata = test, type = "response")
pred_stepwise <- predict(stepwise_model, newdata = test, type = "response")

evaluate_model <- function(probs, actual, cutoff) {
  pred <- ifelse(probs > cutoff, 1, 0)
  cm <- table(Predicted = pred, Actual = actual)
  
  accuracy <- mean(pred == actual)
  sensitivity <- cm["1", "1"] / sum(cm[, "1"])
  specificity <- cm["0", "0"] / sum(cm[, "0"])
  
  return(c(Accuracy = round(accuracy, 3),
           Sensitivity = round(sensitivity, 3),
           Specificity = round(specificity, 3)))
}

evaluate_model(pred_forward, test$LoanApproved, 0.5)
evaluate_model(pred_forward, test$LoanApproved, 0.3)
evaluate_model(pred_forward, test$LoanApproved, 0.1)

evaluate_model(pred_backward, test$LoanApproved, 0.5)
evaluate_model(pred_backward, test$LoanApproved, 0.3)
evaluate_model(pred_backward, test$LoanApproved, 0.1)

evaluate_model(pred_stepwise, test$LoanApproved, 0.5)
evaluate_model(pred_stepwise, test$LoanApproved, 0.3)
evaluate_model(pred_stepwise, test$LoanApproved, 0.1)

# Forward table
forward_tbl <- summary(forward_model)$coefficients
forward_tbl <- cbind(forward_tbl, OR = exp(forward_tbl[, 1]))
round(forward_tbl, 3)

# Backward table
backward_tbl <- summary(backward_model)$coefficients
backward_tbl <- cbind(backward_tbl, OR = exp(backward_tbl[, 1]))
round(backward_tbl, 3)

# Stepwise table
stepwise_tbl <- summary(stepwise_model)$coefficients
stepwise_tbl <- cbind(stepwise_tbl, OR = exp(stepwise_tbl[, 1]))
round(stepwise_tbl, 3)


-------------------#Knn#-----------------------------------
loan_scaled <- Loan

loan_scaled[ , !(names(loan_scaled) %in% "LoanApproved")] <- 
  scale(loan_scaled[ , !(names(loan_scaled) %in% "LoanApproved")])

head(loan_scaled)
str(loan_scaled)

set.seed(1)

n.train = floor( nrow(loan_scaled)*0.70 )
ind.train = sample(1:nrow(loan_scaled), n.train)
ind.test = setdiff(1:nrow(loan_scaled), ind.train)

require(class)

Xtrain = loan_scaled[ind.train, !names(loan_scaled) %in% c("LoanApproved")]
Xtest = loan_scaled[ind.test,!names(loan_scaled) %in% c("LoanApproved")]

ytrain = loan_scaled[ind.train, "LoanApproved"]

ytrain <- as.factor(ytrain)

ypred = knn(Xtrain, Xtest, ytrain, k=3, prob=T)

head(ypred)
head(attr(ypred, "prob"))

ytest = loan_scaled[ind.test,"LoanApproved"]
table(ytest, ypred)

get.prob = function(x) {
  prob = attr(x, 'prob')
  ind = which(x == 0)
  prob[ind] = 1 - prob[ind]
  return(prob)
}

knn.bestK = function(train, test, y.train, y.test, k.grid = 1:20, ct = .5) {
  # browser()
  fun.tmp = function(x) {
    y.tmp = knn(train, test, y.train, k = x, prob=T) # run knn for each k in k.grid
    prob = get.prob(y.tmp)
    y.hat = as.numeric( prob > ct )
    return( sum(y.hat != as.numeric(y.test)) )
  }
  ## create a temporary function (fun.tmp) that we want to apply to each value in k.grid
  error = unlist(lapply(k.grid, fun.tmp))
  names(error) = paste0('k=', k.grid)
  ## it will return a list so I need to unlist it to make it to be a vector
  out = list(k.optimal = k.grid[which.min(error)], 
             error.min = min(error)/length(y.test),
             error.all = error/length(y.test))
  return(out)
}

obj1 = knn.bestK(Xtrain, Xtest, ytrain, ytest, seq(1, 18, 2), .5)
obj1

ypred1 = knn(Xtrain, Xtest, ytrain, k=obj1$k.optimal, prob=T)
obj1$k.optimal
table(ytest, ypred1)


sen = function(ytest, ypred) {
  ind1 = which(ytest == 1)
  mean(ytest[ind1] == ypred[ind1])
}

spe = function(ytest, ypred) {
  ind1 = which(ytest == 0)
  mean(ytest[ind1] == ypred[ind1])
}

fpr = function(ytest, ypred) {
  ind1 = which(ytest == 0)
  mean(ytest[ind1] != ypred[ind1])
}

fnr = function(ytest, ypred) {
  ind1 = which(ytest == 1)
  mean(ytest[ind1] != ypred[ind1])
}

sen(ytest, ypred1)
spe(ytest, ypred1)
fpr(ytest, ypred1)
fnr(ytest, ypred1)


performance = function(ytest, ypred) {
  measures = c(mean(ytest == ypred),
               sen(ytest, ypred),
               spe(ytest, ypred),
               fpr(ytest, ypred),
               fnr(ytest, ypred))
  names(measures) = c('Accuracy', 'Sensitivity', 'Specificity', 'FPR', 'FNR')
  return(measures)
}

performance(ytest, ypred1)

----------------------------------- LOG_REGression    ->    #FORWARD
  
  selected_variable_forward = c("TotalDebtToIncomeRatio", "InterestRate", "AnnualIncome", "LenghtOfCreditHistory", "CreditScore", "TotalAssets", "EducationLevel_High.School",
                                "EducationLevel_Associate","EmploymentStatus_Unemployed", "Experience", "EducationLevel_Other", "EducationLevel_Master", "HomeOwnershipStatus_Rent",
                                "HomeOwnershipStatus_Other", "PaymentHistory", "MonthlyDebtPayments", "TotalLiabilities", "LoanDuration", "EmploymentStatus_Self.Employment",
                                "MonthlyLoanPayment", "MaritalStatus_Single", "SavingsAccountBalance", "LoanPurpose_Auto", "LoanPurpose_Debt.Consolidation")

#selected_variable_stepwise = selected_variable_stepwise[selected_variable_stepwise %in% names(loan_scaled)]

Xtrain = loan_scaled[ind.train, names(loan_scaled) %in% selected_variable_forward]
Xtest = loan_scaled[ind.test,names(loan_scaled) %in% selected_variable_forward]

ytrain = loan_scaled[ind.train, "LoanApproved"]
ytrain <- as.factor(ytrain)

ytest = loan_scaled[ind.test,"LoanApproved"]
#table(ytest, ypred)

knn.bestK = function(train, test, y.train, y.test, k.grid = 1:20, ct = .3) {
  # browser()
  fun.tmp = function(x) {
    y.tmp = knn(train, test, y.train, k = x, prob=T) # run knn for each k in k.grid
    prob = get.prob(y.tmp)
    y.hat = as.numeric( prob > ct )
    return( sum(y.hat != as.numeric(y.test)) )
  }
  ## create a temporary function (fun.tmp) that we want to apply to each value in k.grid
  error = unlist(lapply(k.grid, fun.tmp))
  names(error) = paste0('k=', k.grid)
  ## it will return a list so I need to unlist it to make it to be a vector
  out = list(k.optimal = k.grid[which.min(error)], 
             error.min = min(error)/length(y.test),
             error.all = error/length(y.test))
  return(out)
}

obj_fwd = knn.bestK(Xtrain, Xtest, ytrain, ytest, seq(1, 20, 2), .3)
obj_fwd

ypred_fwd = knn(Xtrain, Xtest, ytrain, k=obj1$k.optimal, prob=T)

table(ytest, ypred_fwd)
performance(ytest, ypred_fwd)


----------------------------------- LOG_REGression    ->    #BACKWARD
  
  selected_variable_backward = c("AnnualIncome", "CreditScore", "Experience", "LoanDuration", "MonthlyDebtPayments","PaymentHistory", "LengthOfCreditHistory", "TotalAssets",
                                 "SavingsAccountBalance", "TotalLiabilities", "InterestRate", "MonthlyLoanPayment", "TotalDebtToIncomeRatio", "EmploymentStatus_Self.Employed",
                                 "EmploymentStatus_Unemployed", "EducationLevel_Associate", "EducationLevel_High.School", "EducationLevel_Master", "EducationLevel_Other",
                                 "MaritalStatus_Single", "HomeOwnershipStatus_Other", "HomeOwnershipStatus_Rent", "LoanPurpose_Auto", "LoanPurpose_Debt.Consolidation")

#selected_variable_stepwise = selected_variable_stepwise[selected_variable_stepwise %in% names(loan_scaled)]

Xtrain = loan_scaled[ind.train, names(loan_scaled) %in% selected_variable_backward]
Xtest = loan_scaled[ind.test,names(loan_scaled) %in% selected_variable_backward]

ytrain = loan_scaled[ind.train, "LoanApproved"]
ytrain <- as.factor(ytrain)

ytest = loan_scaled[ind.test,"LoanApproved"]
#table(ytest, ypred)

knn.bestK = function(train, test, y.train, y.test, k.grid = 1:20, ct = .5) {
  # browser()
  fun.tmp = function(x) {
    y.tmp = knn(train, test, y.train, k = x, prob=T) # run knn for each k in k.grid
    prob = get.prob(y.tmp)
    y.hat = as.numeric( prob > ct )
    return( sum(y.hat != as.numeric(y.test)) )
  }
  ## create a temporary function (fun.tmp) that we want to apply to each value in k.grid
  error = unlist(lapply(k.grid, fun.tmp))
  names(error) = paste0('k=', k.grid)
  ## it will return a list so I need to unlist it to make it to be a vector
  out = list(k.optimal = k.grid[which.min(error)], 
             error.min = min(error)/length(y.test),
             error.all = error/length(y.test))
  return(out)
}

obj_back = knn.bestK(Xtrain, Xtest, ytrain, ytest, seq(1, 20, 2), .5)
obj_back

ypred_back = knn(Xtrain, Xtest, ytrain, k=obj1$k.optimal, prob=T)

table(ytest, ypred_back)
performance(ytest, ypred_back)

----------------------------------- LOG_REGression    ->    #STEPWISE
  
  selected_variable_stepwise = c("TotalDebtToIncomeRatio", "InterestRate", "AnnualIncome", "LengthOfCreditHistory", "CreditScore", "TotalAssets","EducationLevel_High.School",
                                 "EducationLevel_Associate", "EmploymentStatus_Unemployed", "Experience", "EducationLevel_Other", "EducationLevel_Master", "HomeOwnershipStatus_Rent",
                                 "HomeOwnershipStatus_Other", "PaymentHistory", "MonthlyDebtPayments", "TotalLiabilities", "LoanDuration", "EmploymentStatus_Self.Employed",
                                 "MonthlyLoanPayment", "MaritalStatus_Single", "SavingsAccountBalance", "LoanPurpose_Auto", "LoanPurpose_Debt.Consolidation")

#selected_variable_stepwise = selected_variable_stepwise[selected_variable_stepwise %in% names(loan_scaled)]

Xtrain = loan_scaled[ind.train, names(loan_scaled) %in% selected_variable_stepwise]
Xtest = loan_scaled[ind.test,names(loan_scaled) %in% selected_variable_stepwise]

ytrain = loan_scaled[ind.train, "LoanApproved"]
ytrain <- as.factor(ytrain)

ytest = loan_scaled[ind.test,"LoanApproved"]
#table(ytest, ypred)

knn.bestK = function(train, test, y.train, y.test, k.grid = 1:20, ct = .5) {
  # browser()
  fun.tmp = function(x) {
    y.tmp = knn(train, test, y.train, k = x, prob=T) # run knn for each k in k.grid
    prob = get.prob(y.tmp)
    y.hat = as.numeric( prob > ct )
    return( sum(y.hat != as.numeric(y.test)) )
  }
  ## create a temporary function (fun.tmp) that we want to apply to each value in k.grid
  error = unlist(lapply(k.grid, fun.tmp))
  names(error) = paste0('k=', k.grid)
  ## it will return a list so I need to unlist it to make it to be a vector
  out = list(k.optimal = k.grid[which.min(error)], 
             error.min = min(error)/length(y.test),
             error.all = error/length(y.test))
  return(out)
}

obj_step = knn.bestK(Xtrain, Xtest, ytrain, ytest, seq(1, 20, 2), .1)
obj_step

ypred_step = knn(Xtrain, Xtest, ytrain, k=obj1$k.optimal, prob=T)

table(ytest, ypred_step)
performance(ytest, ypred_step)

-----------------------------------------
  #SCREENED_VARIABLES
  
  # Ensure y is numeric 0/1
  loan_scaled$LoanApproved <- as.numeric(loan_scaled$LoanApproved)

# Select numeric predictors only
predictors <- loan_scaled[ , sapply(loan_scaled, is.numeric) & names(loan_scaled) != "LoanApproved"]
str(predictors)
# Compute correlation with the output variable
cor_vals <- sapply(predictors, function(x) cor(x, loan_scaled$LoanApproved))

# Filter predictors with correlation >0.2 or <-0.2
screened_variables <- names(cor_vals[cor_vals > 0.2 | cor_vals < -0.2])

# Show selected predictors
print(screened_variables)

Xtrain = loan_scaled[ind.train, names(loan_scaled) %in% screened_variables]
Xtest = loan_scaled[ind.test,names(loan_scaled) %in% screened_variables]

ytrain = loan_scaled[ind.train, "LoanApproved"]
ytrain <- as.factor(ytrain)

ytest = loan_scaled[ind.test,"LoanApproved"]
#table(ytest, ypred)

obj_screened = knn.bestK(Xtrain, Xtest, ytrain, ytest, seq(1, 20, 2), .3)
obj_screened

ypred_screened = knn(Xtrain, Xtest, ytrain, k=obj1$k.optimal, prob=T)

table(ytest, ypred_screened)
performance(ytest, ypred_screened)


-----------------#CART#--------------------
#Addition train and validate data
set.seed(100)
id.loantrain= sample(1:nrow(Loan), nrow(Loan)*.7)
id.loantest= setdiff(1:nrow(Loan), id.loantrain)
loan.train= Loan[id.loantrain,]
loan.test= Loan[id.loantest,]

library(rpart)
library(rpart.plot)

ctree= rpart(LoanApproved ~., method="class", data= loan.train)


ctree.me= prune(ctree, cp= ctree$cptable[which.min(ctree$cptable[,"xerror"]),"CP"])
rpart.plot(ctree.me, main = 'Min Error Tree')

K= 8
ind = which.min(ctree$cptable[,"xerror"])
se1 = ctree$cptable[ind,"xstd"]/sqrt(K)
xer1 = min(ctree$cptable[,"xerror"]) + se1
ind0 = which.min(abs(ctree$cptable[1:ind,"xerror"] - xer1))
ctree.bp = prune(ctree, cp = ctree$cptable[ind0,"CP"])
rpart.plot(ctree.bp, main = 'Best Pruned Tree')

# Predict using minimum error tree
yhat1 = predict(ctree.me, loan.test, type="class") # cut off 0.5
prob2 = predict(ctree.me, loan.test, type = "prob") [,2]
yhat2 = as.numeric (prob2 > .3) # cut off 0.3 
prob3 = predict(ctree.me, loan.test, type= 'prob') [,2]
yhat3 = as.numeric (prob3 > .1) # cut off 0.1
# Predict using best pruned tree
yhat4 = predict(ctree.bp, loan.test, type='class') # cut off 0.5
prob5 = predict(ctree.bp, loan.test, type='prob') [,2]
yhat5 = as.numeric (prob5 > .3) # cut off 0.3
prob6 = predict(ctree.bp, loan.test, type='prob') [,2]
yhat6 = as.numeric (prob6 > .1) # cut off 0.1
Performance <- function(predicted, actual) {
  # Create a confusion matrix
  confusion_matrix <- table(Predicted = predicted, Actual = actual)
  
  # Extract values from confusion matrix
  true_positive <- confusion_matrix["1", "1"]
  true_negative <- confusion_matrix["0", "0"]
  false_positive <- confusion_matrix["1", "0"]
  false_negative <- confusion_matrix["0", "1"]
  
  # Calculate accuracy
  accuracy <- (true_positive + true_negative) / sum(confusion_matrix)
  
  # Calculate sensitivity 
  sensitivity <- true_positive / (true_positive + false_negative)
  
  # Calculate specificity
  specificity <- true_negative / (true_negative + false_positive)
  
  # Print the results
  cat("Accuracy:", accuracy, "\n")
  cat("Sensitivity:", sensitivity, "\n")
  cat("Specificity:", specificity, "\n")
}

Performance(yhat1, loan.test$LoanApproved)
Performance(yhat2, loan.test$LoanApproved)
Performance(yhat3, loan.test$LoanApproved)
Performance(yhat4, loan.test$LoanApproved)
Performance(yhat5, loan.test$LoanApproved)
Performance(yhat6, loan.test$LoanApproved)

