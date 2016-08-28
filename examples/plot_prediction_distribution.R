
plot_prediction_distribution <- function(probs, trueVals, threshold) {
  # Plots the prediction distribution.
  #
  # Args:
  #   probs: The predicted probabilities.
  #   trueVals: The true values of the target variable (either 0 or 1).
  #   threshold: Threshold for prediction.
  #
  # Returns:
  #   A ggplot object.
  
  df <- data.frame(truth = truth, p1 = probs)
  
  v <- rep(NA, nrow(df))
  v <- ifelse(df$p1 >= threshold & df$truth == 1, "TP", v)
  v <- ifelse(df$p1 >= threshold & df$truth == 0, "FP", v)
  v <- ifelse(df$p1 < threshold & df$truth == 1, "FN", v)
  v <- ifelse(df$p1 < threshold & df$truth == 0, "TN", v)
  
  df$pred_type <- v
  
  p <- ggplot(data=df, aes(x=truth, y=p1)) + 
    geom_jitter(aes(color=pred_type, size=.2), alpha=0.6, size=1) +
    geom_violin(aes(x=truth, y=p1), fill=rgb(1,1,1,alpha=0.6), color=NA) + 
    geom_hline(yintercept=threshold, color="red", alpha=0.6) +
    scale_color_discrete(name = "type") +
    labs(title=sprintf("Threshold at %.2f", threshold)) + 
    xlab("Truth") + ylab("Probability") 
  
  return(p)
}
