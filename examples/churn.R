
#=============================================================
# Load required packages
#=============================================================
require(readr)
require(h2o)
# h2o.removeAll()

#=============================================================
# Env. config.
#=============================================================
#Sys.setenv(JAVA_HOME="D:/Program Files/Java/jre7") # for win os
Sys.setenv(HTTPS_PROXY="") # if proxy was set
Sys.setenv(HTTP_PROXY="")  # if proxy was set

#=============================================================
# Init H2O (connect to a running H2O cluster)
#=============================================================
h2o.init(port = 54321,
         username = "aghorbani", 
         password = "aghorbani",  
         startH2O = FALSE)

#=============================================================
# Load data
#=============================================================
# setwd("E:/Users/aghorbani/Documents/presentation/h2o")
setwd("~/github/H2O/examples/")
data  <- read_csv("data/attrition.csv")

#=============================================================
# Force classification
#=============================================================
data$Churn  <- as.factor(data$Churn)

#=============================================================
# Upload the data into H2O
#
# one can also use :
#    data_frame <- h2o.uploadFile("data/attrition.csv",destination_frame = "data_frame")  
#=============================================================
data_frame <- as.h2o(data, destination_frame = "data_frame")


#data_frame$Churn  <- as.factor(data_frame$Churn)

#=============================================================
# Split data into training and validation
#=============================================================
split_df  <- h2o.splitFrame(data_frame, 0.7, 
                            destination_frames = c("train_frame","valid_frame"), 
                            seed=2016)

train_frame <- split_df[[1]]
valid_frame <- split_df[[2]]
  
#=============================================================
# Build the model
#=============================================================

y <- "Churn"
x <- setdiff(names(data_frame), 
             c(y,"CustID"))
#========================
# Logistic Regression
#========================
glm <- h2o.glm(
  x                = x,
  y                = y,
  training_frame   = train_frame,
  validation_frame = valid_frame,
  family           = "binomial",
  link             = "logit",
  lambda           = 0, #1e-05,
  compute_p_values = TRUE,
  model_id         = "yooHoo_my_awesome_GLM")

#========================
# Gradient boosting machine
#========================
gbm <- h2o.gbm(
  x                = x,
  y                = y,
  training_frame   = train_frame,
  validation_frame = valid_frame,
  ntrees           = 50,
  max_depth        = 3,
  learn_rate       = 0.03,
  sample_rate      = 0.8,
  col_sample_rate  = 0.7,
  model_id         = "yooHoo_my_awesome_GBM")

#========================
# distributed random forest
#========================
drf <- h2o.randomForest(
  x                = x,
  y                = y,
  training_frame   = train_frame,
  validation_frame = valid_frame,
  model_id         = "yooHoo_my_awesome_drf")

#========================
# deeplearning
#========================
dl <- h2o.deeplearning(
  x                     = x,
  y                     = y,
  training_frame        = train_frame,
  validation_frame      = valid_frame,
  activation            = "TanhWithDropout",
  epochs                = 600,
  input_dropout_ratio   = 0.2,
  hidden                = c(10,10), 
  hidden_dropout_ratios = c(0.2,0.2),
  score_interval        = 0.0001,
  model_id              = "yooHoo_my_awesome_dl")


#========================
# Plot AUC
#========================
shist <- drf@model$scoring_history[, c("duration", "validation_MSE")]
shist$algorithm <- "RandomForest" 
scoring_history <- shist

shist <- gbm@model$scoring_history[, c("duration", "validation_MSE")]
shist$algorithm <- "GBM" 
scoring_history <- rbind(scoring_history,shist)

# shist <- glm@model$scoring_history[, c("duration", "validation_MSE")]
# shist$algorithm <- "GLM" 
# scoring_history <- rbind(scoring_history,shist)

shist <- dl@model$scoring_history[, c("duration", "validation_MSE")]
shist$algorithm <- "DeepLearning" 
scoring_history <- rbind(scoring_history,shist)

scoring_history$duration <- as.numeric(
  gsub("sec", "", scoring_history$duration))

require(ggplot2)
ggplot(data = scoring_history, 
       aes(x     = duration, 
           y     = validation_MSE, 
           color = algorithm,
           group = algorithm)) + 
  geom_line() + geom_point()
