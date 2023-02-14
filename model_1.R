#===============================================================================
# Script to answer assessment 2
# Created by: Jayden Dzierbicki
#===============================================================================




# Clear global enviroment 
rm(list=ls())

# SETUP=========================================================================
getRversion()
# Load in required packages
install.packages("e1071")
library(e1071)
library(ggplot2)
library(dplyr)
library(quadprog)
library(readxl)
library(lattice)
library(caret)
library(randomForest)
library(ggplot2)
library(ggparty)
library(rpart)
library(tree)
library(dplyr)
library(MASS)
library(pROC)
library(ipred)
library(gridExtra)
library(pROC)
library(doParallel)
library(parallel)
library(kernlab)
library(rminer)

# Start Q1======================================================================

# Load in data frame
df <- data.frame(x1 = c(3,4,3.5,5,4,6,2,-1,3,3,-2,-1),
                 x2 = c(2,0,-1,1,-3,-2,5,7,6.5,7,7,10),
                 y = c(-1,-1,-1,-1,-1,-1, 1,1,1,1,1,1)) 




# Plot df, X1 verticle, X2 horizontal and Y grouped by colour
plot_1 <- ggplot(data = df, mapping = aes(x = x2, y=x1, colour = factor(y))) +
  geom_point(size = 5) + # Increase size for ease of reading
  scale_colour_manual(values = c("blue", "red"), name = "y",
                      labels = c("-1", "1")) +
  ggtitle("Scatter Plot of Training Data")

print(plot_1)


# Note:
# We know that sole.qp() package offers optimisation for:
# min_b: -d^T*b + 0.5*b^T*D*b
# Such that. A^T*b > b0
#
#
# We can map our problam such that
# d = 0
# b' = [w, b]
# D = [I,0;0,0]
# A^T = [y*x_1, y*x_2, -y]
# b_0 = [1,1,1,...,1]
# Source: https://biodatascience.github.io/statcomp/ml/svm.html 


# Spit data into features and target 
x <- df[, c("x2", "x1")]
y <- df$y

# Obtain demsnions of x
n <- nrow(x)
p <- ncol(x)

# Create matrix x
x <- as.matrix(df[,2:1]) # Since X2 is on X axis and X1 on y axis we select these positions
y <- c(df$y)

# find number observations
n <- length(y)

# find number varibles
p <- ncol(x)

# Create Dmat matrix (nxn)
Dmat <- matrix(0, nrow=p+1, ncol=p+1)

# Make D matrix Identity
diag(Dmat) <- 1
Dmat[p+1, p+1] <- 1e-6

# Create D vector
dvec <- matrix(0, nrow=p+1)

# Create A^T matrix
Amat <- t(cbind(x * y, -y))

# Create b vectors (all ones)
bvec <- matrix(1, nrow=n)

# Solve the quadratic problem
qp_output <- solve.QP(Dmat, dvec, Amat, bvec)

# Fitted w and b
w <- qp_output$solution[1:p]
b <- qp_output$solution[p+1]

# Plot model
ggplot(df, aes(y = x1, x= x2,colour= factor(y))) + 
  geom_point(size = 5) +
  geom_abline(intercept=(b+1)/w[2],slope=-w[1]/w[2],alpha=.2,linetype=2) +
  geom_abline(intercept=(b-1)/w[2],slope=-w[1]/w[2],alpha=.2,linetype=2) +
  geom_abline(intercept=b/w[2],slope=-w[1]/w[2],linetype=3) +
  scale_colour_manual(values = c("blue", "red"), name = "y",
                      labels = c("-1", "1")) 

# Obtain margin value
w_norm <- sqrt(sum(w^2))
margin <- 2/ w_norm

# END Q1========================================================================
















# Start Q2 =====================================================================

# Load in credit card data
credit_card <- read_excel("CreditCard_Data.xls") # Load in data
credit_card <- credit_card[-1,] # Delete  row 1 
credit_card <- credit_card[,-1] # Delete column 1 - ID


# We also observe whilst the data is generally clean, some groupings have repetition
# X3: encodes, 4 as other & 5/6 as unknown
# X4: encodes 0 as unknown, and 3 as other
# For this purpose we will assume other and unkown are the same thing from an
# analysis purpose and group these together
# This is confirmed thoruhg orignal research paper

credit_card <- credit_card %>% 
  mutate(X3 = case_when(X3 == "0" | X3 == "4" | X3 == "5" | X3 == "6" ~ "4",
                        TRUE ~ X3)) %>% 
  mutate(X4 = case_when(X4 == "0" | X4 == "3" ~ "3",
                        TRUE ~ X4) )

barchart(credit_card$Y) # Confrim worked
barchart(credit_card_t$X3) # Confirm worked

# Data exploration, wrangling and pre-prossesing 
(na_count <- sapply(credit_card, function(y) sum(length(which(is.na(y))))) ) # No NA values observed, data is compelte 

summary(credit_card) # Currently all data is of character type, will possibly need to convert to numric, int or factor. ID could be turned into rowname

credit_card_clean <- credit_card %>% 
  mutate(X1 = as.integer(X1),
         X2 = as.factor(X2),
         X3 = as.factor(X3),
         X4 = as.factor(X4),
         X5 = as.integer(X5),
         X6 = as.factor(X6),
         X7 = as.factor(X7),
         X8 = as.factor(X8),
         X9 = as.factor(X9),
         X10 = as.factor(X10),
         X11 = as.factor(X11),
         X12 = as.integer(X12),
         X13 = as.integer(X13),
         X14 = as.integer(X14),
         X15 = as.integer(X15),
         X16 = as.integer(X16),
         X17 = as.integer(X17),
         X18 = as.integer(X18),
         X19 = as.integer(X19),
         X20 = as.integer(X20),
         X21 = as.integer(X21),
         X22 = as.integer(X22),
         X23 = as.integer(X23),
         Y = as.factor(Y) ) 

# Are they easily seperable?
ggplot(credit_card_clean, aes(y = (X21), x= (X13),colour= factor(Y))) + 
  geom_point(size = 2)

# Extract integer/factor columns to do easy visuals in one plot
integer_cols <- names(credit_card_clean)[sapply(credit_card_clean, is.integer)]
facotr_cols <- names(credit_card_clean)[sapply(credit_card_clean, is.factor)]

# Loop through each integer column and create a density plot
plots <- list()
plots_facotr <- list()

# Loop through and plot
for (i in 1:length(integer_cols)) {
  plots[[i]] <- ggplot(credit_card_clean, aes_string(x = integer_cols[i], fill = "Y")) +
    geom_density(alpha = 0.5) +
    scale_x_continuous(limits = range(credit_card_clean[, integer_cols[i]])) +
    ggtitle(paste("Density Plot of", integer_cols[i], "Grouped by Y")) +
    xlab(integer_cols[i]) +
    ylab("Density") +
    facet_wrap(~Y, nrow = 1)
}

for (i in 1:length(facotr_cols)) {
  plots_facotr[[i]] <- ggplot(credit_card_clean, aes_string(x = facotr_cols[i], fill = "Y")) + 
    geom_bar(stat = "count", position = "fill") + 
    ggtitle(paste0(facotr_cols[i], " against Y")) +
    scale_y_continuous(labels = scales::percent)
}

library(corrplot)
corrplot((cor(credit_card_clean[integer_cols])) )# Some strong cor, thus keep in mind for assumption testing

# Print the plots on one output
grid.arrange(grobs = plots, ncol = 4)
grid.arrange(grobs = plots_facotr, ncol = 4)



# Select a random sample of 70% of the full dataset as the training data
set.seed(123)  # For reproducability 

# Split data & generate a train and test data set
split <- createDataPartition(credit_card_clean$Y, p = 0.7, list = F)
train_data <- credit_card_clean[split,]
test_data <- credit_card_clean[-split,]


# Print out the dimesnion of the train data
dim <- dim(train_data)
print(paste0("The number of rows in the training data set is: ", dim(train_data)[1], " This is ", 100*dim(train_data)[1]/nrow(credit_card_clean), "% of the total number of rows in the data set",
             ". The number of columns in the training data set is: ", dim(train_data)[2]))

# We could consider various classification methods, though the one we will select
# is popular due to it's ability to handle continous/categorical varibles
# as well as it's ability to overcome limitations of other CART methods.
# In addition, it is widly used in the litraute 
# https://link.springer.com/chapter/10.1007/978-981-15-5285-4_18
# 



# Random forrest model ===
set.seed(123)
start_time <- Sys.time()

# Define the control parameters for the random forest
# Hyper parameters - just demoing hyper-parameter selection
p <- ncol(credit_card_clean) - 1 # Number of predcitors, less y
sqrt_p <- sqrt(p) # Will be used in mtry
ctrl <- trainControl(method = "cv", number = 10)

# Define the grid of ntree and mtry values to search
mtry_grid <- c(sqrt(p), p / 2, p / 4) # Compare some p-values

# Perform the grid search using the train function
rf_hyperparameter_selection <- train(Y~., data = train_data, 
                                     method = "rf",
                                     trControl = ctrl,
                                     tuneGrid = grid,
                                     importance = T)

end_tune <- Sys.time() - start_time # 58 mins 
best_mtry <- rf_hyperparameter_selection$bestTune$mtry # Extract best mtry 5.75
plot(rf_hyperparameter_selection)

set.seed(123)  # For reproducability 
start_time <- Sys.time()
model_random_forest <- randomForest(Y ~., data = train_data, ntree = 500, mtry = 5.75, importance = T) # Produce random forest model
model_rf_run_time <- Sys.time() - start_time # Takes over 1min on current data set.
summary(model_random_forest)
print(model_random_forest) # OOB error


# Varible importance
# Mean decrease accuracy: 
#         Gives estimate of the loss in prediction performance when that
#         particular varible is ommted from the training set
#
# Mean decrease GINI:
#         GINI is a measure of node impurity, if we use this feature to split
#         the data, how pure will the nodes be? 
#         higher purity means that each node
#         contains elements of only a single class
varImpPlot(model_random_forest) # Print variable importance 


# Produce prediction on test data set
yhat_random_forest_test <- predict(model_random_forest, newdata = test_data, type = "class")
yhat_random_forest_train <- predict(model_random_forest, newdata = train_data, type = "class")

# Confusion matrix
(confusion_matrix_random_forest_test <- confusionMatrix(as.factor(yhat_random_forest_test), as.factor(test_data$Y),  positive ="1"))
(precision <- confusion_matrix_random_forest_test$byClass["Pos Pred Value"])
(recall <- confusion_matrix_random_forest_test$byClass["Sensitivity"])

# Confusion matrix - train
(confusion_matrix_random_forest_train <- confusionMatrix(yhat_random_forest_train, train_data$Y,positive ="1"))





# Bagging method: Just to select model based on training ======================= 
# Just using defult settings
set.seed(123)  # For reproducability 
start_time <- Sys.time()
model_bagging <- randomForest(Y ~., data = train_data, ntree = 500, mtry = p, importance = T) # Produce random forest model
model_bagging_run_time <- Sys.time() - start_time # Takes over 1min on current data set.
summary(model_bagging)
print(model_bagging) # OOB error: 18.06%

# Predict and plot confusion matrix
yhat_bagging_train <- predict(model_bagging, new_data = train_data, type = "class")

(confusion_matrix_bagging_train <- confusionMatrix(yhat_bagging_train, train_data$Y, positive ="1" ))




# Simple classification tree: Just to select model based on training results====
# Just using default setting
set.seed(123)  # For reproducability 
start_time <- Sys.time()
M1 <- rpart(Y ~ ., data = train_data, method = "class")
M1_run_time <- Sys.time() - start_time
plotcp(M1) # No need to refine model
library(rattle)
rattle::fancyRpartPlot(M1)

# Predict and plot confusion matrix
yhat_m1_train <- predict(M1, newdata = train_data, type = "class")

(confusion_matrix_m1_train <- confusionMatrix(yhat_m1_train, train_data$Y, positive ="1" ))










#SVM Hyper-paramter grid search demo============================================

# Will do analysis on 10% of data due to contratins and then train full model===
# Split data & generate a train and test data set, 10% due to proccess time=====

# SVM Should be scaled
num_vars <- select_if(credit_card_clean, is.numeric)
preprocess_info <- preProcess(num_vars, method = c("center", "scale"))
scaled_num_vars <- predict(preprocess_info, num_vars)
scaled_df <- bind_cols(scaled_num_vars, select_if(credit_card_clean, is.factor))

# Partition data with set seed 123 & create data parititon 
set.seed(123)  # For reproducability 

# Set SVM split based on requirment (this is mentioned in the paper why we toggle)
#svm_pre_split <- createDataPartition(scaled_df$Y, p = 0.1, list = F) # For tunning
svm_pre_split <- createDataPartition(scaled_df$Y, p = 0.7, list = F) # For final model
svm_pre_train <- as.data.frame(scaled_df[svm_pre_split,])
svm_pre_test <- (scaled_df[-svm_pre_split,])

# Define a list of cost parameters to try
gammas <- c(0.01, 0.1, 1, 2) # Test gama
costs <- c(seq(0.001, 2, length = 10))

# Train the model using the tune() function and cross-validation================
set.seed(123)
start_time <- Sys.time()
model <- tune(svm, Y ~ ., data = svm_pre_train, ranges = list(cost = costs, gamma = gammas), kernel = "radial")
model_poly <- tune(svm, Y ~ ., data = svm_pre_train, ranges = list(cost = costs, gamma = gammas), kernel = "polynomial", degree = 2)
model_sigmoid <- tune(svm, Y ~ ., data = svm_pre_train,ranges = list(cost = costs, gamma = gammas), kernel = "sigmoid")
svm_end_time <- Sys.time() - start_time # 2.0 hours/ 10 hours on 70% 

# Select the best model based on the cross-validated accuracy===================
# Extract the performance metric from each model
model_performance <- model$best.performance
model_linear_performance <- model_linear$best.performance
model_poly_performance <- model_poly$best.performance 
model_sigmoid_performance <- model_sigmoid$best.performance 
(best_model_index <- which.min(c(model_performance, model_linear_performance, model_poly_performance, model_sigmoid_performance))) # Radial
plot(model) # Plot to demonstrate visually

# Demonstrate var importnace, not we could have tuned everything in train(), but
# had issues etracting var impoortance
# Identify and remove constant variables
var_importance <- train(Y ~ ., data = svm_pre_train, method = "svmRadial")
varImp(var_importance$coefnames)

# Produce two final SVM models, one based on tuning, and one based on default===
# Use 70 data set as seen in random forest

# Tuned SVM Model
start_time <- Sys.time()
svm_model_sig <- svm(Y ~ ., data = svm_pre_train, cost = 1.555778 , gama = 0.1, kernel = "radial")
model_svm_run_time_sigmod <- Sys.time() - start_time
summary(svm_model_sig)
print(svm_model_sig)

# Defult SVM model
start_time <- Sys.time()
svm_model_basic <- svm(Y ~ ., data = svm_pre_train) # Defult settings
model_svm_run_time <- Sys.time() - start_time


# Tuned model - train confision matrix
yhat_rad <- predict(svm_model_sig, newdata = svm_pre_train)
confusionMatrix(yhat_rad, svm_pre_train$Y, positive ="1" )

# SVM Generic model - train confision matrix
yhat_svm <- predict(svm_model_basic, newdata = svm_pre_train)
confusionMatrix(yhat_svm, svm_pre_train$Y, positive ="1" )


# Compare models of RF and SVM against test data================================

# Obtain SVM prediction
yhat_svm_test <- predict(svm_model_sig, newdata = svm_pre_test)

# Print confusion matrix for test data set
confusionMatrix(yhat_svm_test, svm_pre_test$Y, positive ="1") # 897 predicted yes
confusion_matrix_random_forest_test # 1177 predicted yes

# Comapre svm vs random forest model
table(yhat_svm_test, yhat_random_forest_test)







# END Q2========================================================================



