#=============================================================
# Load required packages
#=============================================================
require(h2oEnsemble)
require(ggplot2)
require(h2o)

#=============================================================
# Init H2O (connect to a running H2O cluster)
#=============================================================
h2o.init(port = 54321, startH2O = FALSE)
h2o.removeAll()

#=============================================================
# Load data
#=============================================================
data_frame <- 
  h2o.importFile(
    path              = "http://www.dataminingconsultant.com/data/churn.txt",
    sep               = ",", 
    destination_frame = "data_frame")

# remove special characters from column names
colnames(data_frame) <- gsub(" ", "_", trimws(gsub("[[:punct:]]", " ", names(data_frame))))

#=============================================================
# Force classification
#=============================================================
data_frame$Churn  <- as.factor(data_frame$Churn)

#=============================================================
# Split data into training and validation
#=============================================================
split_df  <- h2o.splitFrame(data_frame, 0.7, 
                            destination_frames = c("train_frame","valid_frame"), 
                            seed=2016)

train_frame <- split_df[[1]]
valid_frame <- split_df[[2]]

#=============================================================
# Target and predictors
#=============================================================
y <- "Churn"
x <- setdiff(names(data_frame),  y)

#=============================================================
# DL Grid search
#=============================================================
dl.grid <- h2o.grid(
  algorithm             = "deeplearning",
  grid_id               = "dl.grid.search",
  x                     = x,
  y                     = y,
  training_frame        = train_frame,
  validation_frame      = valid_frame,
  loss                  = c("CrossEntropy"),
  classification_stop   = -1,
  stopping_rounds       = 0,
  hyper_params          = list(
    activation            = c("RectifierWithDropout", "TanhWithDropout"),
    epochs                = c(500),
    rho                   = c(0.999,0.99),
    epsilon               = c(1e-11,1e-7),
    balance_classes       = c(FALSE, TRUE),
    hidden                = list(c(100,100), c(50,50), c(50)),
    hidden_dropout_ratios = list(c(0.2,0.2), c(0.5,0.5), c(0.2), c(0.5)),
    input_dropout_ratio   = c(0.2, 0.5)
  ))
#=============================================================
# GBM Grid search
#=============================================================
gbm.grid <- h2o.grid(
  algorithm             = "gbm",
  grid_id               = "gbm.grid.search",
  x                     = x,
  y                     = y,
  training_frame        = train_frame,
  validation_frame      = valid_frame,
  stopping_rounds       = 2,
  score_tree_interval   = 5,
  stopping_tolerance    = 0.001,
  stopping_metric       = "AUC",
  ntrees                = 1000,     
  hyper_params          = list(
    learn_rate       = c(0.1, 0.05, 0.01), 
    max_depth        = c(2, 4), 
    col_sample_rate  = c(0.6, 0.8), 
    sample_rate      = c(0.6, 0.8)
  ))
#=============================================================
# RF Grid search
#=============================================================
sqrtp <- ceiling(sqrt(length(x)))
rf.grid <- h2o.grid(
  algorithm             = "randomForest",
  grid_id               = "rf.grid.search",
  x                     = x,
  y                     = y,
  training_frame        = train_frame,
  validation_frame      = valid_frame,
  stopping_rounds       = 2,
  score_tree_interval   = 5,
  stopping_tolerance    = 0.001,
  stopping_metric       = "AUC",
  hyper_params          = list(
    ntrees              = c(10, 50, 100),     
    mtries              = c(ceiling(sqrtp/2), sqrtp, 2*sqrtp),
    max_depth           = c(5, 10, 15), 
    sample_rate         = c(0.632, 0.8), 
    col_sample_rate_per_tree   = c(0.8, 1)
  ))

#=============================================================
# Specify base learners
#=============================================================

funcNames <-  NULL

#========================
# Choose top 3 GBM
#========================
for(i in 1:3){
  x_ <- gbm.grid@summary_table[i,]
  funcName <- paste0("h2o.gbm.", i)
  func <- paste('function(...,  
           ntrees             = 1000,                learn_rate          = ',x_$learn_rate,', 
           max_depth          = ',x_$max_depth,',    col_sample_rate     = ',x_$col_sample_rate,', 
           sample_rate        = ',x_$sample_rate,',  stopping_rounds     = 2,
           stopping_metric    = "AUC",               score_tree_interval = 5,
           stopping_tolerance = 0.0001) 
    h2o.gbm.wrapper(..., 
                    ntrees	            = ntrees,               learn_rate         = learn_rate, 
                    max_depth           = max_depth,            col_sample_rate    = col_sample_rate, 
                    sample_rate         = sample_rate,          seed               = seed,
                    stopping_rounds     = stopping_rounds,      stopping_metric    = stopping_metric,
                    score_tree_interval = score_tree_interval,  stopping_tolerance = stopping_tolerance)
  ')
  
  funcNames <- c(funcNames, funcName)
  assign(funcName, eval(parse(text=func)))
}

#========================
# Choose top 3 RF
#========================
for(i in 1:3){
  x_ <- rf.grid@summary_table[i,]
  funcName <- paste0("h2o.rf.", i)
  func <- paste('function(...,
           col_sample_rate_per_tree =', x_$col_sample_rate_per_tree, ',
           ntrees       =', x_$ntrees    ,',  mtries             =', x_$mtries,',
           max_depth    =', x_$max_depth ,',  sample_rate        =', x_$sample_rate,',
           stopping_rounds     = 2,           stopping_metric    = "AUC",
           score_tree_interval = 5,           stopping_tolerance = 0.0001) 
    h2o.randomForest.wrapper(...,
           col_sample_rate_per_tree = col_sample_rate_per_tree, 
           ntrees       = ntrees,     mtries       = mtries,
           max_depth    = max_depth,  sample_rate  = sample_rate,
           stopping_rounds     = stopping_rounds,      stopping_metric    = stopping_metric,
           score_tree_interval = score_tree_interval,  stopping_tolerance = stopping_tolerance)
  ')
  
  funcNames <- c(funcNames, funcName)
  assign(funcName, eval(parse(text=func)))
}

#========================
# Choose top 3 DL
#========================
for(i in 1:3){
  x_ <- dl.grid@summary_table[i,]
  funcName <- paste0("h2o.rf.", i)
  func <- paste('function(...,
           activation         =', x_$activation,',     epochs                =', x_$epochs,',
           hidden             =', x_$hidden,',         hidden_dropout_ratios =', x_$hidden_dropout_ratios,',
           rho                =', x_$rho,',            epsilon               =', x_$epsilon,',
           balance_classes    =', x_$balance_classes,' input_dropout_ratio   =', x_$input_dropout_ratio,',
           classification_stop  = -1,                  stopping_rounds        = 0)
    h2o.deeplearning.wrapper(...,
           activation         = activation,           epochs                = epochs,
           hidden             = hidden,               hidden_dropout_ratios = hidden_dropout_ratios,
           rho                = rho,                  epsilon               = epsilon,
           balance_classes    = balance_classes,      input_dropout_ratio   = input_dropout_ratio,
           classification_stop = classification_stop, stopping_rounds       = stopping_rounds)
  ')
  
  funcNames <- c(funcNames, funcName)
  assign(funcName, eval(parse(text=func)))
}

#=============================================================
# Create base learner vector 
#=============================================================
learner <- funcNames

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

