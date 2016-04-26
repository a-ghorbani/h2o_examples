#=============================================================
# Load required packages
#=============================================================
require(readr)
require(ggplot2)
require(h2o)
source("plot_h2o_grid.R")

#=============================================================
# Init H2O (connect to a running H2O cluster)
#=============================================================
h2o.init(port = 54324,
         username = "aghorbani", 
         password = Sys.getenv("h2oPass"),  
         startH2O = FALSE)

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
# Build the model
#=============================================================

y <- "Churn"
x <- setdiff(names(data_frame), 
             c(y,"CustID"))

dl.grid <- h2o.grid(
  algorithm             = "deeplearning",
  grid_id               = "dl.grid.search",
  x                     = x,
  y                     = y,
  training_frame        = train_frame,
  validation_frame      = valid_frame,
  hidden                = c(100,100),
  hidden_dropout_ratios = c(0.2,0.5),
  loss                  = c("CrossEntropy"),
  rate                  = 0.0001,
  l1                    = 0.0001,
  l2                    = 0.0001,
  input_dropout_ratio   = 0.1,
  rate_annealing        = 0.0001,
  classification_stop   = -1,
  stopping_rounds       = 1000,
  score_interval        = 0.0001,
  balance_classes       = TRUE,
  hyper_params          = list(
    activation            = c("RectifierWithDropout", "TanhWithDropout"),
    epochs                = c(500,1000),
    rho                   = c(0.999,0.99),
    epsilon               = c(1e-11,1e-7)
  ))

dl.grid

#=============================================================
# Plot
#=============================================================

plotGridHistory(grid_id         = "dl.grid.search",
                score           = "MSE",
                history_messure = "epochs", 
                fname           = "dl_grid_search.pdf",
                y_lim           = c(0,0.3),
                x_lim           = c(0,1000))
                