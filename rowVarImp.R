
rowVarImp.h2o  <- 
  function(model, cols, data_frame, n){
    # Calculates top variable importance for each row.
    #
    # Args:
    #   model: A h2o model.
    #   cols: Name of columns to be considered for calculating ranks.
    #   data_frame: A H2O data frame.
    #   n: Returns the results based on "n"th most important variables.
    #   key_cols: key columns (the data frame should have at least one column
    #   as key value).
    #
    # Returns:
    #  A r data.frame that contains nth most important variable for each 
    #  variable it's importance, the value and it's name.
    
    # n cannot be bigger than length(cols)
    n <- min(length(cols), n)
    
    # prediction probability when all co-variables included
    pred.orig <- h2o.predict(model, data_frame)
    
    # Each time drop a column an calculate it's difference in prediction of probabilities.
    for(var in cols)  {
      tmp.df <- data_frame[, -which(names(data_frame) %in% c(var))]
      pred <- h2o.predict(model, tmp.df)
      if(var == cols[1]){
        pred.diff <- pred.orig[,3] - pred[,3]
        names(pred.diff) <- var
      }else{
        pred.diff[, var] <- pred.orig[,3] - pred[,3]
      }
    }

    # conver data.tables to matrices
    rankMat  <- as.matrix(pred.diff)
    valMat   <- as.matrix(data_frame[,cols])
    class(valMat) <- "character"
    
    # calculate
    l <- get_high_rank_values(rankMat, valMat, cols, n)
    
    # bind all results
    res <- cbind(l[[1]], l[[2]], l[[3]])
    
    # give appropriate name to each column
    colNameC <-  paste0("colName_VI", c(1:n))
    colNameR <-  paste0("imp_VI", c(1:n))
    colNameV <-  paste0("value_VI", c(1:n))
    colnames(res) <- c(colNameR, colNameV, colNameC)
    
    return(res)
  } 