
#=============================================================
# Load required packages
#=============================================================
# Obviously h2o must be loaded and and initialized beforehand
require(ggplot2)
require(reshape2)
source("multiplot.R")

#=============================================================
# Plots grid models history score against history_messure.
# and saves into pdf file.
#
# Usage example:
#   plotGridHistory(grid_id         = "deeplearning_grid141", 
#                   history_messure = "epochs", 
#                   fname           = "dl_grid141.pdf")
#
#=============================================================
plotGridHistory = function(grid_id, score="logloss", history_messure,
                           cols=3, rows = 5, fname, x_lim = NULL, y_lim = NULL){
  grid      <- h2o.getGrid(grid_id)
  model_ids <- grid@model_ids
  models    <- lapply(model_ids, function(id) { h2o.getModel(id)})
  p <- list()
  for(i in 1:length(model_ids)){
    mod <- models[[i]]
    dat <- melt(mod@model$scoring_history[c(history_messure, 
                                            paste0("validation_", score), 
                                            paste0("training_", score))], 
                id=c(history_messure))
    p[[i]] <- ggplot(data = dat, aes_string(x = history_messure, 
                                            y="value", 
                                            color="variable")) + 
      geom_line() +
      ggtitle(toString(mod@allparameters[unlist(grid@hyper_names)])) +
      theme(plot.title = element_text(size=9))
    if(!is.null(x_lim)){
      p[[i]] <- p[[i]] +  xlim(x_lim) 
    }
    if(!is.null(y_lim)){
      p[[i]] <- p[[i]] + ylim(y_lim) 
    }
    if(i==-1){ 
      p[[i]] <- p[[i]] + theme(legend.justification=c(1,1), 
                               legend.position=c(1,1))
    }else{
      p[[i]] <- p[[i]]+guides(colour=FALSE)
    }
  }
  
  np  <- length(model_ids)
  ppp <- cols * rows
  npages = ceiling( np / ppp)
  pdf(fname)
  for(i in 1:npages){
    n1 <- ppp * (i-1) + 1
    n2 <- min(np,ppp * i)
    multiplot(plotlist=p[n1:n2], cols = cols, rows = rows)
  }
  dev.off()
}