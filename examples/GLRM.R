
#=============================================================
# Load required packages
#=============================================================
require(h2o)
require(lattice)

h2o.init()
#port = 54324,
#         username = "aghorbani", 
#         password = Sys.getenv("h2oPass"),  
 #        startH2O = FALSE)

#h2o.removeAll()
#=============================================================
# Initiate problem parameters
#=============================================================
k  <- 10   # Low rank
n1 <- 40   # number of numerical columns
n2 <- 30   # number of ordinal columns
n3 <- 30   # number of binary columns
n <- n1+n2+n3 # number of columns
m <- 100  # number of rows

#=============================================================
# Initiate Low rank matrix
#=============================================================
X <- rnorm(m*k)
dim(X) <- c(m,k)

#=============================================================
# Initiate archetype matrix
#=============================================================
Y <- rnorm(k*n)
dim(Y) <- c(k,n)

#=============================================================
# High dimentional data (actual data)
#=============================================================
data   <- X %*% Y
data <- as.data.frame(data)
c.num  <- c(1:n1)           # numerical columns indices
c.ord  <- c((n1+1):(n1+n2)) # ordinal columns indices
c.bin  <- c((n1+n2+1):n)    # binary columns indices

#=============================================================
# Convert to Ordinal
# 1,2 ... 7
#=============================================================
tmp <- data[,c.ord]
tmp <- round((tmp - min(tmp)) / (max(tmp) - min(tmp)) * 6 + 1)
data[,c.ord] <- tmp
data[,c.ord] <- as.data.frame(lapply(data[,c.ord], as.factor))

#=============================================================
# Convert to Boolean
# 0, 1
#=============================================================
data[,c.bin] <- ( sign(data[,c.bin]) + 1 ) / 2  

#=============================================================
# Make part of data missing 
#=============================================================
r.na <- c(40:50)
c.na <- c((n1-3):(n-10))
data[r.na,c.na] <- NA

#=============================================================
# Upload data into H2O 
#=============================================================
data_df  <- as.h2o(data,  destination_frame = "data_df")

#=============================================================
# Fit GLRM model 
#=============================================================
glrm.fit <- h2o.glrm(
  training_frame    = data_df, 
  validation_frame  = data_df,
  k                 = k, 
  ignore_const_cols = FALSE,
  loss              = "Quadratic",
  multi_loss        = "Categorical", 
  loss_by_col       = c(rep("Hinge",n3)),
  loss_by_col_idx   = c(c.bin)-1,
  regularization_x  = "Quadratic",
  regularization_y  = "Quadratic",
  gamma_x           = 0.01,
  gamma_y           = 0.01
  )

#=============================================================
# h2o.predict will reconstructs data 
#=============================================================
glrm.reconst <- h2o.predict(glrm.fit, data_df)

#=============================================================
# Plot original vs Prediction 
#=============================================================
p1 <- levelplot(
        x           = t(data), 
        xlab        = "", 
        ylab        = "", 
        main        = "Original data", 
        colorkey    = list(at=seq(from=-10,to=10,length=11)),
        at          = seq(from=-10,to=10,length=11), 
        scales      = list(draw = FALSE),
        col.regions = rainbow(11))
p2 <- levelplot(
        x           = t(as.data.frame(h2o.getFrame(glrm.fit@model$representation_name))), 
        xlab        = "", 
        ylab        = "", 
        main        = "Low dim. rep.", 
        colorkey    = list(at=seq(from=-10,to=10,length=11)),
        at          = seq(from=-10,to=10,length=11), 
        scales      = list(draw = FALSE),
        col.regions = rainbow(11))
p3 <- levelplot(
        x           = t(as.data.frame(glrm.reconst)), 
        xlab        = "", 
        ylab        = "", 
        main        = "Reconst. (from Low-Ranked X.Y)", 
        colorkey    = list(at=seq(from=-10,to=10,length=11)),
        at          = seq(from=-10,to=10,length=11), 
        scales      = list(draw = FALSE),
        col.regions = rainbow(11))

print(p1, position = c(0,   0, 0.42, 1), more = TRUE)
print(p2, position = c(0.42, 0, 0.58, 1), more = TRUE)
print(p3, position = c(0.58, 0,   1, 1))

