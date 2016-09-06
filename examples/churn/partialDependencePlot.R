
partialDependencePlot <- function(model, cols, data_frame, N=20, values = NULL){
  
  require(plyr)
  
  target_col <- model@parameters$y
  
  ncol <- length(cols)
  if(ncol > 1 & !is.null(values) & class(values)!='list'){
      stop("If # of cols is bigger than 1 then values should be a list NULL.")
  }
  
  lvalues <- lapply(cols, function(col){
    type  <- h2o.getTypes(data_frame)[[which( names(data_frame) == col)]]
  
    vals <- values[[col]]
    
    if(is.null(vals)){
      if(type == "enum" | type == "string"){
        #vals = as.data.frame(h2o.unique(data_frame[,col]))
        tab <- as.data.frame(h2o.table(data_frame[,col]))
        vals <- tab[order(tab$Count, decreasing = T),][c(1:N),col]
      }else if(type == "int" | type == "real"){
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
        
        vals <- seq(minVal,maxVal,(maxVal-minVal)/(N-1))
        if(type == "int"){
          vals <- round(vals)
        }
      }else{  
        stop(" unknown type for " + prdictor_col)
      }
    }
    
    vals
  })
  
  origpreds <- h2o.predict(model, data_frame)
  
  dim_ <- sapply(lvalues, length)
  res <- array(0, dim = dim_, dimnames = lvalues)
  ff <- function(valsl, colsn, j, res_){
    if(j == ncol){
      tempframe <- data_frame
      for(i in c(1:ncol)){
        tempframe[,colsn[i]] <- valsl[i]
      }
      newpreds <- h2o.predict(model, tempframe)
      del <- mean(newpreds$p1-origpreds$p1)
      eval.parent(substitute(res_ <- del))
    }else{
      v <- lvalues[[j]]
      for(ii in c(1:length(v))){
        ff(valsl = c(valsl, v[ii]), colsn=c(colsn, cols[j]), j+1, res_[v[ii]])  
      }
    }
  }
  ff(c(), c(), 0, res[])
    ===> change ti this
  result1 <- matrix(result[,5], ncol = 10)
  result2 <- apply(a4d, 4, `[`, indices)
  identical(result1, result2)
  
  
  p <- plot(values, 
            deltas,
            type='l', 
            main=paste0("Partial dependence plot for ", target_col, " as a function of ", predictor_col), 
            xlab=predictor_col, 
            ylab=paste0("delta.",target_col))
  
  return(p)
}
