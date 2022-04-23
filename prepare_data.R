source("params.R")

prepare_data <- function(data) {
  # z-score normalization
  data[1:(n_cols - 1)] <- scale(data[1:(n_cols - 1)])
  if (is.factor(data[, n_cols]))
    data[, n_cols] <- as.numeric(data[, n_cols])
  data[, n_cols] <- data[, n_cols] - min(data[, n_cols])
  
  # splitting
  train_n             = as.integer(n_rows * split_ratios[1])
  test_n              = as.integer(n_rows * split_ratios[2])
  train_labelled_n    = as.integer(train_n * split_train_ratios[1])
  train_unlabelled_n  = as.integer(train_n * split_train_ratios[2])
  train_decision_n    = as.integer(train_n * split_train_ratios[3])
  
  shuffled_data <-  data[sample(1:n_rows), ]
  test_set <-       shuffled_data[row.names(data) %in%
                                    1:test_n, ]
  labelled_set <-   shuffled_data[row.names(data) %in%
                                    (test_n + 1):(test_n + train_labelled_n), ]
  unlabelled_set <- shuffled_data[row.names(data) %in%
                                    (test_n + train_labelled_n + 1):(test_n + train_labelled_n + train_unlabelled_n), ]
  decision_set <-   shuffled_data[row.names(data) %in%
                                    (test_n + train_labelled_n + train_unlabelled_n + 1):n_rows, ]
  
  return(list(
    "test_set"=test_set,
    "labelled_set"=labelled_set,
    "unlabelled_set"=unlabelled_set,
    "decision_set"=decision_set
  ))
}