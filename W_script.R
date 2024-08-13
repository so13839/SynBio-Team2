# Load necessary libraries
library(xgboost)
library(caret)
library(dplyr)

df <- Acinetobacter_regression
# Assuming you've already loaded the data into a dataframe `df`
names(df)

#df$Erythromycin <- as.factor(df$Erythromycin)

# Adjust the Erythromycin values
df$Erythromycin <- gsub(">8", "8", df$Erythromycin)
df$Erythromycin <- gsub(">=16", "16", df$Erythromycin)
df$Erythromycin <- gsub(">4", "4", df$Erythromycin)
df$Erythromycin <- gsub("<=0.12", "0.12", df$Erythromycin)
df$Erythromycin <- gsub("<=0.25", "0.125", df$Erythromycin)

dfE <- df[,c(1:2,4,6)]
Final_dfE <- dfE
library(dplyr)
library(rlang)

convert_column <- function(data, col_name) {
  
  new_col_name <- paste0(col_name,".num")
  data %>% mutate(!!new_col_name := as.numeric(!!sym(col_name)))
}

Final_dfE <- dfE %>% convert_column("Erythromycin")
Final_dfE$Gender <- as.factor(Final_dfE$Gender)
Final_dfE$Gender <- as.numeric(Final_dfE$Gender)
Final_dfE$Age.Group <- as.factor(Final_dfE$Age.Group)
Final_dfE$Age.Group <- as.numeric(Final_dfE$Age.Group)
#Final_dfE$Speciality <- as.factor(Final_dfE$Speciality)
#Final_dfE$Speciality <- as.numeric(Final_dfE$Speciality)
Final_dfE$Source <- as.factor(Final_dfE$Source)
Final_dfE$Source <- as.numeric(Final_dfE$Source)
#Final_dfE$In...Out.Patient <- as.factor(Final_dfE$In...Out.Patient)
#Final_dfE$In...Out.Patient <- as.numeric(Final_dfE$In...Out.Patient)

Final_dfE <- Final_dfE[,-4]
# Split the data into training and testing sets

# Adjust the Erythromycin values
df$Penicillin <- gsub(">8", "8.5", df$Penicillin)
df$Penicillin <- gsub("<=0.06", "0.06", df$Penicillin)

Final_dfE <- df %>% convert_column("Penicillin")
Final_dfE$Gender <- as.factor(Final_dfE$Gender)
Final_dfE$Gender <- as.numeric(Final_dfE$Gender)
Final_dfE$Age.Group <- as.factor(Final_dfE$Age.Group)
Final_dfE$Age.Group <- as.numeric(Final_dfE$Age.Group)
Final_dfE$Speciality <- as.factor(Final_dfE$Speciality)
Final_dfE$Speciality <- as.numeric(Final_dfE$Speciality)
Final_dfE$Source <- as.factor(Final_dfE$Source)
Final_dfE$Source <- as.numeric(Final_dfE$Source)
Final_dfE$In...Out.Patient <- as.factor(Final_dfE$In...Out.Patient)
Final_dfE$In...Out.Patient <- as.numeric(Final_dfE$In...Out.Patient)

Final_dfEL <- Final_dfE[,-c(6:8)]

Final_dfEL <- Final_dfE
set.seed(123)
trainIndex <- createDataPartition(Final_dfEL$Erythromycin.num, p = .8, 
                                  list = FALSE, 
                                  times = 1)

train_data <- Final_dfEL[ trainIndex,]
test_data  <- Final_dfEL[-trainIndex,]

# Prepare data for XGBoost
train_matrix <- xgb.DMatrix(data = as.matrix(train_data[, -ncol(train_data)]), label = train_data$Erythromycin.num)
test_matrix <- xgb.DMatrix(data = as.matrix(test_data[, -ncol(test_data)]), label = test_data$Erythromycin.num)

# Train XGBoost model
xgb_model <- xgboost(data = train_matrix, 
                     max.depth =10000, 
                     eta = 0.0001, 
                     nround = 1000, 
                     objective = "reg:squarederror", 
                     verbose = 50)

# Make predictions on the test set
predictions <- predict(xgb_model, test_matrix)

# Compare predicted vs actual values
comparison <- data.frame(Actual = test_data$Erythromycin.num, Predicted = predictions)

# Print the comparison
print(comparison)


#performance metrics on the test data

mean((test_data$Penicillin.num - predictions)^2) #mse - Mean Squared Error

caret::RMSE(test_data$Penicillin.num, predictions) #rmse - Root Mean Squared Error

y_test_mean = mean(test_data$Penicillin.num)
# Calculate total sum of squares
tss =  sum((test_data$Erythromycin.num - y_test_mean)^2 )
# Calculate residual sum of squares
rss =  sum(residuals^2)
# Calculate R-squared
rsq  =  1 - (rss/tss)
cat('The R-square of the test data is ', round(rsq,3), '\n')

x = 1:length(test_y)                   # visualize the model, actual and predicted data
plot(x, test_y, col = "red", type = "l")
lines(x, pred_y, col = "blue", type = "l")
legend(x = 1, y = 38,  legend = c("original test_y", "predicted test_y"), 
       col = c("red", "blue"), box.lty = 1, cex = 0.8, lty = c(1, 1))

library(randomForest)

rf = randomForest(Erythromycin.num~., data = train_data , ntree = 10000)
predictions <- as.numeric(predict(rf, Final_dfEL, type = 'response'))


library(pls)
gas1 <- plsr(Erythromycin.num ~., ncomp = 2, data = train_data, validation = "LOO")
gas1

predict(gas1, ncomp = 2, newdata = test_data)

LM <- lm(Erythromycin.num ~., data = train_data)
summary(LM)
