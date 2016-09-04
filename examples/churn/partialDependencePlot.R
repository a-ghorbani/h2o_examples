
partialDependencePlot <- function(model, cols, data_frame, N=20, values = NULL){
  
  target_col <- model@parameters$y
  
  lvalues <- lapply(cols, function(col){
    type  <- h2o.getTypes(data_frame)[[which( names(data_frame) == col)]]
  
    if(is.null(values)){
      if(type == "enum"){
        values = as.data.frame(h2o.unique(data_frame[,col]))
      }else if(type == "string"){
        values = as.data.frame(h2o.unique(data_frame[,col]))
      }else if(type == "int"){
        minVal <- min(data_frame[,col], na.rm=TRUE)
        maxVal <- max(data_frame[,col], na.rm=TRUE)
        
        quant  <- h2o.quantile(data_frame[,col], probs = c(0.02, 0.98))
        minVal.q <- min(quant)
        maxVal.q <- max(quant)
        
        len <- (maxVal.q - minVal.q)
        diff.min = abs(minVal - minVal.q)
        diff.max = abs(maxVal - maxVal.q)
        if( diff.min / len > 0.05){
          cat("Min value is much smaller than 2% quantile, hence, we use only values from 2% quantile.\n")
          cat("Min: ", minVal, "\t 2% quantile: ", minVal.q, "\n")
          minVal = minVal.q
        }
        if( diff.max / len > 0.05){
          cat("Max value is much bigger than 98% quantile, hence, we use only values from 98% quantile.\n")
          cat("Max: ", maxVal, "\t 98% quantile: ", maxVal.q, "\n")
          maxVal = maxVal.q
        }
        
        values <- seq(minVal,maxVal,(maxVal-minVal)/N)
      }else if(type == "real"){
        minVal <- min(data_frame[,col], na.rm=TRUE)
        maxVal <- max(data_frame[,col], na.rm=TRUE)
        
        quant  <- h2o.quantile(data_frame[,col], probs = c(0.02, 0.98))
        minVal.q <- min(quant)
        maxVal.q <- max(quant)
        
        len <- (maxVal.q - minVal.q)
        diff.min = abs(minVal - minVal.q)
        diff.max = abs(maxVal - maxVal.q)
        if( diff.min / len > 0.05){
          cat("Min value is much smaller than 2% quantile, hence, we use only values from 2% quantile.\n")
          cat("Min: ", minVal, "\t 2% quantile: ", minVal.q, "\n")
          minVal = minVal.q
        }
        if( diff.max / len > 0.05){
          cat("Max value is much bigger than 98% quantile, hence, we use only values from 98% quantile.\n")
          cat("Max: ", maxVal, "\t 98% quantile: ", maxVal.q, "\n")
          maxVal = maxVal.q
        }
        
        values <- seq(minVal,maxVal,(maxVal-minVal)/N)
      }else{  
        stop(" unknown type for " + prdictor_col)
      }
    }
  })
  
  origpreds <- h2o.predict(model, data_frame)
  
  dim <- sapply(lvalues, length)
  deltas <- array(0, dim = dim)
  for (val in values) {
    tempframe <- data_frame
    tempframe[,predictor_col] <- val
    newpreds <- h2o.predict(model, tempframe)
    deltas <- c(deltas,mean(newpreds$p1-origpreds$p1))
  }
  
  p <- plot(values, 
            deltas,
            type='l', 
            main=paste0("Partial dependence plot for ", target_col, " as a function of ", predictor_col), 
            xlab=predictor_col, 
            ylab=paste0("delta.",target_col))
  
  return(p)
}
