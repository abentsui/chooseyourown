if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(nnet)) install.packages("nnet", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(tinytex)) install.packages("tinytex", repos = "http://cran.us.r-project.org")

library(readxl)
library(caret)
library(tidyverse)
library(nnet)
library(gridExtra)
library(tinytex)

# ---------------------------------------------------------------------------------------------------------------
#  STEP 1: Create the Datasets
#  Download files from UCI Machine Learning Repository.
#  Read the excel using read_excel().
#  The data frame is stored in drybeans.rda to avoid re-running these step during code development
# ---------------------------------------------------------------------------------------------------------------

# Download Dry Bean Dataset
# If the script is already run, we can get from the rda file.
if (file.exists("drybeans.rda") == TRUE) {
  # load the rda file
  load("drybeans.rda")
  
} else {
  # Else we download from the link
  dl <- tempfile()
  # dl <- getwd()
  # download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00602/DryBeanDataset.zip", "./DryBeanDataset.zip")
  download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00602/DryBeanDataset.zip", dl)
  
  # excel_file <- unzip("./DryBeanDataset.zip", "DryBeanDataset/Dry_Bean_Dataset.xlsx")
  excel_file <- unzip(dl, "DryBeanDataset/Dry_Bean_Dataset.xlsx")
  drybeans <- read_excel(excel_file)
  
  # Save file for faster processing
  save(drybeans, file = "drybeans.rda")
  
  rm(dl)
}


# ---------------------------------------------------------------------------------------------------------------
#  STEP 2: Data wrangling
# ---------------------------------------------------------------------------------------------------------------

# Make Class as a factor
drybeans <- drybeans %>%
  mutate(Class = as.factor(Class))

# Divide the set into train and test set
# 90% train set, 10% test set
set.seed(1) #Random sampling
test_index <- createDataPartition(y = drybeans$Class, times = 1, p = 0.1, list = FALSE)
test_set <- drybeans[test_index,]
train_set <- drybeans[-test_index,]


# ---------------------------------------------------------------------------------------------------------------
#  STEP 3: Data exploration
# ---------------------------------------------------------------------------------------------------------------

# number of rows & columns in drybeans
nrow(drybeans)
ncol(drybeans)

# summary
summary(drybeans)

# The first 4 rows of the data
drybeans[1:4,] %>%
  knitr::kable()

# Barchart of Class
drybeans %>%
  ggplot(aes(Class)) +
  geom_bar()


# ---------------------------------------------------------------------------------------------------------------
#  STEP 3a: Box plots of each feature
# ---------------------------------------------------------------------------------------------------------------

# Box plots of each feature
# First store it in g1,g2,...
# Then display in grid form

# Area
g1 <- drybeans %>%
  ggplot(aes(Class, Area, fill = Class)) +
  geom_boxplot() +
  theme(axis.text.x = element_blank())

# Perimeter
g2 <- drybeans %>%
  ggplot(aes(Class, Perimeter, fill = Class)) +
  geom_boxplot() +
  theme(axis.text.x = element_blank())

# MajorAxisLength
g3 <- drybeans %>%
  ggplot(aes(Class, MajorAxisLength, fill = Class)) +
  geom_boxplot() +
  theme(axis.text.x = element_blank())

# MinorAxisLength
g4 <- drybeans %>%
  ggplot(aes(Class, MinorAxisLength, fill = Class)) +
  geom_boxplot() +
  theme(axis.text.x = element_blank())

# AspectRation
g5 <- drybeans %>%
  ggplot(aes(Class, AspectRation, fill = Class)) +
  geom_boxplot() +
  theme(axis.text.x = element_blank())

# Eccentricity
g6 <- drybeans %>%
  ggplot(aes(Class, Eccentricity, fill = Class)) +
  geom_boxplot() +
  theme(axis.text.x = element_blank())

# ConvexArea
g7 <- drybeans %>%
  ggplot(aes(Class, ConvexArea, fill = Class)) +
  geom_boxplot() +
  theme(axis.text.x = element_blank())

# EquivDiameter
g8 <- drybeans %>%
  ggplot(aes(Class, EquivDiameter, fill = Class)) +
  geom_boxplot() +
  theme(axis.text.x = element_blank())

# Extent
g9 <- drybeans %>%
  ggplot(aes(Class, Extent, fill = Class)) +
  geom_boxplot() +
  theme(axis.text.x = element_blank())

# Solidity
g10 <- drybeans %>%
  ggplot(aes(Class, Solidity, fill = Class)) +
  geom_boxplot() +
  theme(axis.text.x = element_blank())

# roundness
g11 <- drybeans %>%
  ggplot(aes(Class, roundness, fill = Class)) +
  geom_boxplot() +
  theme(axis.text.x = element_blank())

# Compactness
g12 <- drybeans %>%
  ggplot(aes(Class, Compactness, fill = Class)) +
  geom_boxplot() +
  theme(axis.text.x = element_blank())

# ShapeFactor1
g13 <- drybeans %>%
  ggplot(aes(Class, ShapeFactor1, fill = Class)) +
  geom_boxplot() +
  theme(axis.text.x = element_blank())

# ShapeFactor2
g14 <- drybeans %>%
  ggplot(aes(Class, ShapeFactor2, fill = Class)) +
  geom_boxplot() +
  theme(axis.text.x = element_blank())

# ShapeFactor3
g15 <- drybeans %>%
  ggplot(aes(Class, ShapeFactor3, fill = Class)) +
  geom_boxplot()

# ShapeFactor4
g16 <- drybeans %>%
  ggplot(aes(Class, ShapeFactor4, fill = Class)) +
  geom_boxplot() +
  theme(axis.text.x = element_blank())

grid.arrange(g1, g2, g3, g4, ncol = 2)
grid.arrange(g5, g6, g7, g8, ncol = 2)
grid.arrange(g9, g10, g11, g12, ncol = 2)
grid.arrange(g13, g14, g15, g16, ncol = 2)


# ---------------------------------------------------------------------------------------------------------------
#  STEP 3b: Check Multivariate Normal Assumption
# ---------------------------------------------------------------------------------------------------------------

# Use scatter plot to check if mulitvariate normal assumption holds for conditional probability of predictors given Class.
# Area vs Perimeter
drybeans %>%
  ggplot(aes(Area, Perimeter, color = Class)) +
  geom_point() +
  stat_ellipse(type = "norm", lwd = 0.5)

# Area vs Solidity
drybeans %>%
  ggplot(aes(Area, Solidity, color = Class)) +
  geom_point() +
  stat_ellipse(type = "norm", lwd = 0.5)

# Area vs ShapeFactor4
drybeans %>%
  ggplot(aes(Area, ShapeFactor4, color = Class)) +
  geom_point() +
  stat_ellipse(type = "norm", lwd = 0.5)




# ---------------------------------------------------------------------------------------------------------------
#  STEP 4: Model fitting
# ---------------------------------------------------------------------------------------------------------------

# "BARBUNYA" "BOMBAY" "CALI" "DERMASON" "HOROZ" "SEKER" "SIRA" 
class_vec <- levels(drybeans$Class)

# ---------------------------------------------------------------------------------------------------------------
#  STEP 4a: Method 1 - Guessing with proportion of each class in train set
# ---------------------------------------------------------------------------------------------------------------

# Method 1: Get the proportion of each class in train set
# and we use it as the probability vector in sampling
prop <- train_set %>%
  group_by(Class) %>%
  summarize(n = n()) %>%
  .$n

# Probability vector
prop <- prop/nrow(train_set)
prop

# Sampling
y_hat_guess <- sample(class_vec, nrow(test_set), replace = TRUE, prob = prop) %>%
  factor()

# Use confustionMatrix() to get accuracy
conf_mat <- confusionMatrix(data = y_hat_guess, reference = test_set$Class)
conf_mat

# Accuracy table to compare results of different models
acc_results <- tibble("Method" = "Guessing with Proportion",
                      Accuracy = conf_mat$overall["Accuracy"])
acc_results %>% knitr::kable()


# ---------------------------------------------------------------------------------------------------------------
#  STEP 4b: Method 2 - Multinomial Logistic Regression
# ---------------------------------------------------------------------------------------------------------------

# Method 2: Multinomial Logistic Regression
# Create a duplicate of train and test set. We will relevel and set the base line level to do regression.
train_set2 <- train_set
train_set2$Class <- relevel(train_set2$Class, ref = "BARBUNYA")

test_set2 <- test_set
test_set2$Class <- relevel(test_set2$Class, ref = "BARBUNYA")

# Perform multinomial logistic regression
fit_multinom <- multinom(Class ~ ., data = train_set2)
summary(fit_multinom)

# Predict probability with multinorm model
p_hat_multinom <- predict(fit_multinom, newdata = test_set2, type = "probs")

# Pick the highest probability one as our prediction
y_hat_multinom <- class_vec[apply(p_hat_multinom, 1, which.max)] %>%
  factor()
conf_mat <- confusionMatrix(data = y_hat_multinom, reference = test_set$Class)

# Append results to accuracy table
acc_results <- bind_rows(acc_results, tibble("Method" = "Multinomial Logistic Regression",
                      Accuracy = conf_mat$overall["Accuracy"]))
acc_results %>% knitr::kable()


# ---------------------------------------------------------------------------------------------------------------
#  STEP 4c: Model 3 - Multinomial Logistic Regression with Cross Validation
# ---------------------------------------------------------------------------------------------------------------

# Model 3: Multinom with train
# Set control with train: 5 validation samples comprise of 10% of observations each
control <- trainControl(method = "cv", number = 5, p = 0.9)

# train with multinom model
fit_multinom_tr <- train(Class ~ .,
                         data = train_set,
                         method = "multinom",
                         trControl = control,
                         trace = FALSE)
# Predicted probability
p_hat_multinom_tr <- predict(fit_multinom_tr, newdata = test_set, type = "prob")
# Predicted outcome
y_hat_multinom_tr <- predict(fit_multinom_tr, newdata = test_set, type = "raw")

conf_mat <- confusionMatrix(data = y_hat_multinom_tr, reference = test_set$Class)

# Append results to accuracy table
acc_results <- bind_rows(acc_results, tibble("Method" = "Multinomial Logistic Regression with CV",
                                             Accuracy = conf_mat$overall["Accuracy"]))
acc_results %>% knitr::kable()


# ---------------------------------------------------------------------------------------------------------------
#  STEP 4d: Model 4 - Knn Model with Cross Validation
# ---------------------------------------------------------------------------------------------------------------

# Method 4: Knn with train()
fit_knn_tr <- train(Class ~ .,
                    data = train_set,
                    method = "knn",
                    trControl = control,
                    tuneGrid = data.frame(k = seq(1, 21, by = 2)))
ggplot(fit_knn_tr, highlight = TRUE)

# Predicted probability
p_hat_knn_tr <- predict(fit_knn_tr, newdata = test_set, type = "prob")
# Predicted outcome
y_hat_knn_tr <- predict(fit_knn_tr, newdata = test_set, type = "raw")

conf_mat <- confusionMatrix(data = y_hat_knn_tr, reference = test_set$Class)

# Append results to accuracy table
acc_results <- bind_rows(acc_results, tibble("Method" = "Knn with CV",
                                             Accuracy = conf_mat$overall["Accuracy"]))
acc_results %>% knitr::kable()


# ---------------------------------------------------------------------------------------------------------------
#  STEP 4e: Model 5 - QDA Model with Cross Validation
# ---------------------------------------------------------------------------------------------------------------

# Model 5: QDA
# Normal assumption
fit_qda_tr <- train(Class ~ ., method = "qda", data = train_set,
                    trControl = control)

# Predicted probability
p_hat_qda_tr <- predict(fit_qda_tr, newdata = test_set, type = "prob")
# Predicted outcome
y_hat_qda_tr <- predict(fit_qda_tr, newdata = test_set, type = "raw")

conf_mat <- confusionMatrix(data = y_hat_qda_tr, reference = test_set$Class)

# Append results to accuracy table
acc_results <- bind_rows(acc_results, tibble("Method" = "QDA with CV",
                                             Accuracy = conf_mat$overall["Accuracy"]))
acc_results %>% knitr::kable()


# ---------------------------------------------------------------------------------------------------------------
#  STEP 4f: Model 6 - LDA Model with Cross Validation
# ---------------------------------------------------------------------------------------------------------------

# Model 6: LDA
fit_lda_tr <- train(Class ~ ., method = "lda", data = train_set,
                    trControl = control)

# Predicted probability
p_hat_lda_tr <- predict(fit_lda_tr, newdata = test_set, type = "prob")
# Predicted outcome
y_hat_lda_tr <- predict(fit_lda_tr, newdata = test_set, type = "raw")

conf_mat <- confusionMatrix(data = y_hat_lda_tr, reference = test_set$Class)

# Append results to accuracy table
acc_results <- bind_rows(acc_results, tibble("Method" = "LDA with CV",
                                             Accuracy = conf_mat$overall["Accuracy"]))
acc_results %>% knitr::kable()


# ---------------------------------------------------------------------------------------------------------------
#  STEP 4g: Model 7 - Classification Tree Model with Cross Validation
# ---------------------------------------------------------------------------------------------------------------

# Model 7: Classification Tree
fit_rpart_tr <- train(Class ~ ., method = "rpart", data = train_set,
                      trControl = control,
                      tuneGrid = data.frame(cp = seq(0.01, 0.05, leng = 25)))

# Predicted probability
p_hat_rpart_tr <- predict(fit_rpart_tr, newdata = test_set, type = "prob")
# Predicted outcome
y_hat_rpart_tr <- predict(fit_rpart_tr, newdata = test_set, type = "raw")

conf_mat <- confusionMatrix(data = y_hat_rpart_tr, reference = test_set$Class)

plot(fit_rpart_tr$finalModel)
text(fit_rpart_tr$finalModel, cex = 0.75)

# Append results to accuracy table
acc_results <- bind_rows(acc_results, tibble("Method" = "Classification Tree with CV",
                                             Accuracy = conf_mat$overall["Accuracy"]))
acc_results %>% knitr::kable()


# ---------------------------------------------------------------------------------------------------------------
#  STEP 4h: Model 8 - Random Forest Model with Cross Validation
# ---------------------------------------------------------------------------------------------------------------

# Model 8: Random Forest
# This may take 30 mins to run
# May set minNode to seq(3, 50), but take longer time
fit_rf_tr <- train(Class ~ ., method = "Rborist", data = train_set,
                   trControl = control,
                   tuneGrid = data.frame(predFixed = 2, minNode = seq(3, 10)))

ggplot(fit_rf_tr, highlight = TRUE)

# Predicted probability
p_hat_rf_tr <- predict(fit_rf_tr, newdata = test_set, type = "prob")
# Predicted outcome
y_hat_rf_tr <- predict(fit_rf_tr, newdata = test_set, type = "raw")

conf_mat <- confusionMatrix(data = y_hat_rf_tr, reference = test_set$Class)

# Append results to accuracy table
acc_results <- bind_rows(acc_results, tibble("Method" = "Random Forest with CV",
                                             Accuracy = conf_mat$overall["Accuracy"]))
acc_results %>% knitr::kable()


# ---------------------------------------------------------------------------------------------------------------
#  STEP 4i: Model 9 - Ensemble
# ---------------------------------------------------------------------------------------------------------------

# Model 9: Ensemble
# Take average of the previous models and predict based on the maximum probability.

# Predicted probability
p_hat_ensemble <- (p_hat_multinom_tr + p_hat_knn_tr +
                     p_hat_qda_tr + p_hat_lda_tr +
                     p_hat_rpart_tr + p_hat_rf_tr) / 6

# The maximum probability
p_max_ensemble <- apply(p_hat_ensemble, 1, max)

# Predicted outcome
y_hat_ensemble <- class_vec[apply(p_hat_ensemble, 1, which.max)] %>%
  factor()

conf_mat <- confusionMatrix(data = y_hat_ensemble, reference = test_set$Class)

# Append results to accuracy table
acc_results <- bind_rows(acc_results, tibble("Method" = "Ensemble with CV",
                                             Accuracy = conf_mat$overall["Accuracy"]))
acc_results %>% knitr::kable()



# ---------------------------------------------------------------------------------------------------------------
#  STEP 5: Results
# ---------------------------------------------------------------------------------------------------------------

# Display final results
acc_results %>% knitr::kable()


# ---------------------------------------------------------------------------------------------------------------
#  STEP 5a: Further Study Why We Make Errors
# ---------------------------------------------------------------------------------------------------------------

# Check the cases we are so sure about but we make a wrong prediction
ind <- which(y_hat_ensemble != test_set$Class)
ind <- ind[order(p_max_ensemble[ind], decreasing = TRUE)]

# Table showing the probability in ensemble and our prediction and the actual value.
data.frame(p_hat_ensemble[ind[1:10],]) %>%
  mutate(predict = y_hat_multinom_tr[ind[1:10]],
         actual = test_set$Class[ind[1:10]],
         row = ind[1:10])

# Prediction under different methods
data.frame(multinom = y_hat_multinom_tr[ind[1:10]],
           knn = y_hat_knn_tr[ind[1:10]],
           qda = y_hat_qda_tr[ind[1:10]],
           lda = y_hat_lda_tr[ind[1:10]],
           rpart = y_hat_rpart_tr[ind[1:10]],
           rf = y_hat_rf_tr[ind[1:10]],
           actual = test_set$Class[ind[1:10]]) %>%
  knitr::kable()
  

# Study of the the fisrt item which we make a wrong prediction
myrow <- ind[1]

mytable <- test_set %>%
  slice(myrow)
mytable

# Compare of CALI & HOROZ Area
mytable$Area

# The value of the sample is marked as the red line
drybeans %>%
  filter(Class %in% c("CALI", "HOROZ")) %>%
  ggplot(aes(Class, Area, fill = Class)) +
  geom_boxplot() +
  theme(axis.text.x = element_blank()) +
  geom_hline(yintercept = mytable$Area, linetype = "dashed", color = "red")

# Compare of CALI & HOROZ AspectRation
mytable$AspectRation

# The value of the sample is marked as the red line
drybeans %>%
  filter(Class %in% c("CALI", "HOROZ")) %>%
  ggplot(aes(Class, AspectRation, fill = Class)) +
  geom_boxplot() +
  theme(axis.text.x = element_blank()) +
  geom_hline(yintercept = mytable$AspectRation, linetype = "dashed", color = "red")


# How many SDs does the statistics of the sample deviate from Class average
cali_mean <- drybeans %>%
  filter(Class %in% c("CALI")) %>%
  select(-Class) %>%
  summarize_all(mean)

cali_sd <- drybeans %>%
  filter(Class %in% c("CALI")) %>%
  select(-Class) %>%
  summarize_all(sd)

horoz_mean <- drybeans %>%
  filter(Class %in% c("HOROZ")) %>%
  select(-Class) %>%
  summarize_all(mean)

horoz_sd <- drybeans %>%
  filter(Class %in% c("HOROZ")) %>%
  select(-Class) %>%
  summarize_all(sd)

# How many SDs does the statistics of the sample deviate from Cali average
v1 <- mytable %>%
  select(-Class)
v1 <- (v1 - cali_mean)/cali_sd
v1

# Number of features that are within 2 SD from the class average
sum(abs(v1) <= 2)

# How many SDs does the statistics of the sample deviate from Horoz average
v2 <- mytable %>%
  select(-Class)
v2 <- (v2 - horoz_mean)/horoz_sd
v2

# Number of features that are within 2 SD from the class average
sum(abs(v2) <= 2)

