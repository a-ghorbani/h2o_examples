
library(rpart)	
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(partykit)
library(caret)
library(party)
library(rBayesianOptimization)

#=============================================================
# Load required packages
#=============================================================
require(readr)
require(h2o)
require(data.table)
h2o.removeAll()

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

h2o.init(max_mem_size = "10G")
setwd("~/github/H2O/examples/churn/")

#=============================================================
# Upload the data into H2O
#
# one can also use :
#    data_frame <- h2o.uploadFile("data/attrition.csv",destination_frame = "data_frame")  
#=============================================================
data_frame <- h2o.uploadFile("churn.csv",
                             sep = ",", 
                             destination_frame = "data_frame")

#=============================================================
# Split data into training and validation
#=============================================================
split_df  <- h2o.splitFrame(data_frame, 0.5, 
                            destination_frames = c("train_frame","valid_frame"), 
                            seed=2016)

train_frame <- split_df[[1]]
valid_frame <- split_df[[2]]
  
#=============================================================
# Build the model
#=============================================================

y <- "Churn?"
x <- setdiff(names(data_frame),  c(y))

#========================
# GLRM
#========================
#glrm <- h2o.glrm(train_frame, c(y,x), )

#========================
# Parameter Tuning
#========================
h2o_bayes <- function(
  max_depth, learn_rate, sample_rate, 
  col_sample_rate, balance_classes){
  bal.cl <- as.logical(balance_classes)
  gbm <- h2o.gbm(
    x                = x,
    y                = y,
    training_frame   = train_frame,
    #validation_frame = valid_frame,
    nfolds           = 5,
    ntrees           = 900,
    max_depth        = max_depth,
    learn_rate       = learn_rate,
    sample_rate      = sample_rate,
    col_sample_rate  = col_sample_rate,
    stopping_rounds  = 10,
    stopping_metric  = "logloss",
    stopping_tolerance = 0.005,
    balance_classes  = bal.cl)
    
  list(Score = gbm@model$cross_validation_metrics@metrics$AUC,
       Pred  = gbm@model$cross_validation_predictions)
}

OPT_Res <- BayesianOptimization(
  h2o_bayes,
  bounds = list(
    max_depth   = c(2L, 8L), 
    learn_rate  = c(0, 0.2),
    sample_rate = c(0.4, 1), 
    col_sample_rate = c(0.4, 1), 
    balance_classes = c(0L, 1L)),
  init_points = 10,  n_iter = 20,
  acq = "ucb", kappa = 2.576, eps = 0.0,
  verbose = TRUE)

OPT_Res2 <- BayesianOptimization(
  h2o_bayes,
  bounds = list(
    max_depth   = c(2L, 8L), 
    learn_rate  = c(0, 0.2),
    sample_rate = c(0.4, 1), 
    col_sample_rate = c(0.4, 1), 
    balance_classes = c(0L, 1L)),
  init_points = 10,  n_iter = 20,
  acq = "ei", kappa = 2.576, eps = 0.0,
  verbose = TRUE)
OPT_Res3 <- BayesianOptimization(
  h2o_bayes,
  bounds = list(
    max_depth   = c(2L, 8L), 
    learn_rate  = c(0, 0.2),
    sample_rate = c(0.4, 1), 
    col_sample_rate = c(0.4, 1), 
    balance_classes = c(0L, 1L)),
  init_points = 10,  n_iter = 20,
  acq = "poi", kappa = 2.576, eps = 0.0,
  verbose = TRUE)
#========================
# Gradient boosting machine
#========================
gbm <- h2o.gbm(
  x                = x,
  y                = y,
  training_frame   = train_frame,
  validation_frame = valid_frame,
  ntrees           = 900,
  max_depth        = 5,
  learn_rate       = 0.005,
  sample_rate      = 1.0,
  col_sample_rate  = 0.7,
  #balance_classes  = TRUE,
  model_id         = "my_awesome_GBM")

gbm2 <- h2o.gbm(
  x                = x,
  y                = y,
  training_frame   = train_frame,
  validation_frame = valid_frame,
  ntrees           = 100,
  max_depth        = 4,
  learn_rate       = 0.2,
  model_id         = "my_awesome_GBM2")

#========================
# var.imp
#========================
var.imp <- gbm@model$variable_importances$variable[
  gbm@model$variable_importances$scaled_importance > 0.001]

#========================
# Gradient boosting machine for var.imp
#========================
gbm_var.imp <- h2o.gbm(
  x                = var.imp,
  y                = y,
  training_frame   = train_frame,
  validation_frame = valid_frame,
  ntrees           = 900,
  max_depth        = 5,
  learn_rate       = 0.005,
  sample_rate      = 1.0,
  col_sample_rate  = 0.7,
  model_id         = "my_awesome_GBM_var.imp")

#========================
# predict on all data and 
# select those with high certain prediction 
#========================
pred <-  h2o::h2o.predict(gbm_var.imp, data_frame)

data_frame_pred <- h2o.cbind(data_frame, pred)

data_frame2 <- data_frame_pred[
  data_frame_pred$True. < 0.2 | 
  data_frame_pred$True. > 0.8,]

data_frame2 <- h2o.assign(data_frame2, "data_frame2")

#=============================================================
# Split data into training and validation
#=============================================================
split_df2 <- h2o.splitFrame(data_frame2, 0.5, 
                            destination_frames = c("train_frame2","valid_frame2"), 
                            seed=2016)

train_frame2 <- split_df2[[1]]
valid_frame2 <- split_df2[[2]]

#========================
# Gradient boosting machine for var.imp
#========================
tree <- h2o.gbm(
  x                = var.imp,
  y                = y,
  training_frame   = train_frame2,
  validation_frame = valid_frame2,
  ntrees           = 1,
  max_depth        = 10,
  learn_rate       = 1.,
  sample_rate      = 1.0,
  col_sample_rate  = 1.0,
  model_id         = "my_awesome_tree")


train_df <- as.data.frame(train_frame2)
valid_df <- as.data.frame(valid_frame2)
form <- as.formula(predict ~ .)
tmp.df <- train_df[, -which(names(train_df) 
                            %in% c("Churn.","False.","True.", "Phone" ))]
tree.2 <- rpart(form, tmp.df, maxdepth = 5, cp=0.02 )
fancyRpartPlot(tree.2)
prp(tree.2) 

#========================
# Plot AUC
#========================
shist <- gbm@model$scoring_history[, c("duration", "validation_MSE")]
shist$algorithm <- "GBM" 
scoring_history <- shist
#scoring_history <- rbind(scoring_history,shist)

shist <- gbm2@model$scoring_history[, c("duration", "validation_MSE")]
shist$algorithm <- "GBM 2" 
scoring_history <- rbind(scoring_history,shist)

shist <- gbm_var.imp@model$scoring_history[, c("duration", "validation_MSE")]
shist$algorithm <- "GBM with var.imp" 
scoring_history <- rbind(scoring_history,shist)

shist <- tree@model$scoring_history[, c("duration", "validation_MSE")]
shist$algorithm <- "single tree with sign pred data" 
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
