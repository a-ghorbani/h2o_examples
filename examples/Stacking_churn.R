#=============================================================
# Load required packages
#=============================================================
require(h2oEnsemble)
require(readr)
require(ggplot2)
require(h2o)

#=============================================================
# Init H2O (connect to a running H2O cluster)
#=============================================================
h2o.init(port = 54324,
         username = "aghorbani", 
         password = Sys.getenv("h2oPass"),  
         startH2O = FALSE)

#h2o.removeAll()

#=============================================================
# Load data
#=============================================================
# setwd("E:/Users/aghorbani/Documents/presentation/h2o")
setwd("~/github/notebooks/H2O/examples/")
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

#=============================================================
# Split data into training and validation
#=============================================================
split_df  <- h2o.splitFrame(data_frame, 0.7, 
                            destination_frames = c("train_frame","valid_frame"), 
                            seed=2016)

train_frame <- split_df[[1]]
valid_frame <- split_df[[2]]


#=============================================================
# Specify base learners
#=============================================================
h2o.gbm.1 <- 
  function(..., 
           ntrees           = 50,     learn_rate       = 0.03, 
           max_depth        = 3,      col_sample_rate  = 0.65, 
           sample_rate      = 0.8,    seed             = seed,
           stopping_rounds  = 150,    stopping_metric  = "AUC",
           stopping_tolerance = 0.0005) 
    h2o.gbm.wrapper(..., 
            ntrees	           = ntrees,            learn_rate      = learn_rate, 
            max_depth          = max_depth,         col_sample_rate = col_sample_rate, 
            sample_rate        = sample_rate,       seed            = seed,
            stopping_rounds    = stopping_rounds,   stopping_metric = stopping_metric,
            stopping_tolerance = stopping_tolerance)

h2o.gbm.2 <- 
  function(..., 
           ntrees           = 25,     learn_rate       = 0.03, 
           max_depth        = 5,      col_sample_rate  = 0.65, 
           sample_rate      = 0.8,    seed             = seed,
           stopping_rounds  = 150,    stopping_metric  = "AUC",
           stopping_tolerance = 0.0005) 
    h2o.gbm.wrapper(..., 
            ntrees	           = ntrees,            learn_rate      = learn_rate, 
            max_depth          = max_depth,         col_sample_rate = col_sample_rate, 
            sample_rate        = sample_rate,       seed            = seed,
            stopping_rounds    = stopping_rounds,   stopping_metric = stopping_metric,
            stopping_tolerance = stopping_tolerance)

h2o.drf.1 <- 
  function(...,
           binomial_double_trees = TRUE,
           ntrees           = 50,     max_depth        = 3,     
           sample_rate      = 0.8,    stopping_rounds  = 150,    
           stopping_metric  = "AUC",  stopping_tolerance = 0.0005) 
    h2o.randomForest.wrapper(...,
           binomial_double_trees = binomial_double_trees,
           ntrees	            = ntrees,          max_depth          = max_depth,   
           sample_rate        = sample_rate,     stopping_rounds    = stopping_rounds,    
           stopping_metric    = stopping_metric, stopping_tolerance = stopping_tolerance)

h2o.drf.2 <- 
  function(...,
           binomial_double_trees = TRUE,
           ntrees           = 20,     max_depth        = 5,     
           sample_rate      = 0.8,    stopping_rounds  = 150,    
           stopping_metric  = "AUC",  stopping_tolerance = 0.0005) 
    h2o.randomForest.wrapper(...,
           binomial_double_trees = binomial_double_trees,
           ntrees	            = ntrees,          max_depth          = max_depth,   
           sample_rate        = sample_rate,     stopping_rounds    = stopping_rounds,    
           stopping_metric    = stopping_metric, stopping_tolerance = stopping_tolerance)

h2o.dl.1 <- 
  function(...,
           activation            = "TanhWithDropout",  epochs                = 600,
           hidden                = c(10,10),           hidden_dropout_ratios = c(0.2,0.2),
           input_dropout_ratio   = 0.2,                score_interval        = 0.0001)
    h2o.deeplearning.wrapper(...,
         activation            = activation,          epochs                = epochs ,
         hidden                = hidden,              hidden_dropout_ratios = hidden_dropout_ratios,
         input_dropout_ratio   = input_dropout_ratio, score_interval        = score_interval)
    
h2o.dl.2 <- 
  function(...,
           activation            = "TanhWithDropout",  epochs                = 600,
           hidden                = c(50,50),           hidden_dropout_ratios = c(0.5,0.5),
           input_dropout_ratio   = 0.2,                score_interval        = 0.0001)
    h2o.deeplearning.wrapper(...,
           activation            = activation,          epochs                = epochs ,
           hidden                = hidden,              hidden_dropout_ratios = hidden_dropout_ratios,
           input_dropout_ratio   = input_dropout_ratio, score_interval        = score_interval)

#=============================================================
# Create base learner vector 
#=============================================================
learner <- c("h2o.gbm.1", "h2o.gbm.2",
             "h2o.drf.1", "h2o.drf.2")
            # "h2o.dl.1",  "h2o.dl.2")

#=============================================================
# Specify meta-learner 
#=============================================================
# metalearner <- "SL.glm"
metalearner <- "h2o.glm.wrapper"

#=============================================================
# Fit Stacked learners
#=============================================================
family <- "binomial"
ensemble.fit <- h2o.ensemble(
  x              = x, 
  y              = y, 
  training_frame = train_frame, 
  family         = family, 
  learner        = learner, 
  metalearner    = metalearner,
  cvControl      = list(V = 5, shuffle = TRUE))

#=============================================================
# Save the models if you want
#=============================================================
# h2o.save_ensemble(fit, path = "ens_models/", export_levelone = TRUE)

perf <- h2o.ensemble_performance(
   object            = ensemble.fit, 
   newdata           = valid_frame,
   score_base_models = TRUE)

#=============================================================
# Print stacked fit AUC score
#=============================================================
print(perf, metric = "AUC")

#=============================================================
# Print stacked fit MSE score
#=============================================================
print(perf, metric = "MSE")

