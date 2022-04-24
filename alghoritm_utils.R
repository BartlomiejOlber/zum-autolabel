source("params.R")

stop_stabilization <- function(prev_pred, pred){
  pred_labels = round(pred)
  prev_pred_labels = round(prev_pred)
  similarity = sum(!xor(pred_labels, prev_pred_labels)) / length(pred_labels)
  return(similarity >= criterion_args$similarity_threshold)
}

stop_certainty_growth <- function(pred){
  mean_certainty = abs(1 - mean(abs(pred - round(pred))))
  if(mean_certainty < criterion_args$highest_certainty_sofar){
    return(TRUE)
  }else{
    criterion_args$highest_certainty_sofar <<- mean_certainty
    return(FALSE)
  }
}

stop_certainty_level <- function(pred){
  mean_certainty = abs(1 - mean(abs(pred - round(pred))))
  return(mean_certainty > criterion_args$mean_certainty_threshold)
}

check_stop_criterion <- function(criterion, prev_pred, pred){
  if(criterion == CRITERION_TYPES$stabilization)
    return(stop_stabilization(prev_pred, pred))

  if(criterion == CRITERION_TYPES$certainty_growth)
    return(stop_certainty_growth(pred))
  
  if(criterion == CRITERION_TYPES$certainty_threshold)
    return(stop_certainty_level(pred))
  
  stop("Invalid criterion: ", criterion)
}

choose_most_certain <- function(sample_rows, pred){
  percentile = quantile(abs(pred - round(pred)), autolabel_percent)
  most_certain = sample_rows[abs(pred - round(pred)) < percentile,]
  most_certain[, length(most_certain)] = round(pred[abs(pred - round(pred)) < percentile])
  return(most_certain)
}

compute_fmeasure <- function(pred, actual) {
  pred_labels = pred > 0.5
  precision <- sum(pred_labels & actual) / sum(pred_labels)
  recall <- sum(pred_labels & actual) / sum(actual)
  fmeasure <- 2 * precision * recall / (precision + recall)
  return (fmeasure)
}