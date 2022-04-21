stop_stabilization <- function(prev_pred, pred){
  pred_labels = round(pred)
  prev_pred_labels = round(prev_pred)
  similarity = sum(!(pred_labels ^ prev_pred_labels)) / length(pred_labels)
  return(similarity >= criterion_args$similarity_threshold)
}

stop_certainty_growth <- function(pred){
  mean_certainty = abs(1 - mean(abs(pred - round(pred))))
  if(mean_certainty < criterion_args$highest_certainty_sofar){
    return(TRUE)
  }else{
    criterion_args$highest_certainty_sofar <- mean_certainty
    return(FALSE)
  }
}

stop_certainty_level <- function(pred){
  mean_uncertainty = mean(abs(pred - round(pred)))
  return(abs(1 - mean_uncertainty) < criterion_args$mean_certainty_threshold)
}

stop_criterion <- function(prev_pred, pred){
  return(
    switch(
      criterion_type,
      stabilization={stop_stabilization(prev_pred, pred)},
      certainty_growth={stop_certainty_growth(pred)},
      certainty_level={stop_certainty_level(pred)}
    )
  )
}

choose_most_certain <- function(sample_rows, pred){
  percentile = quantile(pred, 1 - autolabel_percent)
  most_certain = sample_rows[pred > percentile,]
  most_certain[, n_cols] = round(pred[pred > percentile])
  return(most_certain)
}

compute_fmeasure <- function(pred, actual) {
  pred_labels = pred > 0.5
  precision <- sum(pred_labels & actual) / sum(pred_labels)
  recall <- sum(pred_labels & actual) / sum(actual)
  fmeasure <- 2 * precision * recall / (precision + recall)
  return (fmeasure)
}