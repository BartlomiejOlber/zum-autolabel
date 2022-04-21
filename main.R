if (!require(xgboost)) install.packages('xgboost')
library(xgboost)
if (!require(e1071)) install.packages('e1071')
library(e1071)
if (!require(mltools)) install.packages('mltools')
library(mltools)
seed = 1
split_ratios = c(0.75, 0.25)
split_train_ratios = c(0.075, 0.9, 0.025)
set.seed(seed)
# use one
# data <- read.table("ring.dat", header=FALSE, skip=25, sep = ',')
# data <- read.table("magic.dat", header=FALSE, skip=16, sep=',')
data <- read.table("spambase.dat", header=FALSE, skip=63, sep = ',')
n_rows = nrow(data)
n_cols = ncol(data)

# z-score normalization
data[1:(n_cols-1)] <- scale(data[1:(n_cols-1)])

# get n_classes
# if (is.factor(data[,n_cols])){
#   n_classes = nlevels(data[,n_cols])  
#   data[,n_cols] <- as.numeric(data[,n_cols])
# }else{
#   data[,n_cols] <- data[,n_cols] - min(data[,n_cols])
#   n_classes = max(data[,n_cols]) + 1
# }
if (is.factor(data[,n_cols])){
  data[,n_cols] <- as.numeric(data[,n_cols])
}
data[,n_cols] <- data[,n_cols] - min(data[,n_cols])
n_classes = max(data[,n_cols]) + 1


# splitting 
train_n = as.integer(n_rows * split_ratios[1])
test_n = as.integer(n_rows * split_ratios[2])
train_labelled_n = as.integer(train_n * split_train_ratios[1])
train_unlabelled_n = as.integer(train_n * split_train_ratios[2])
train_decision_n = as.integer(train_n * split_train_ratios[3])
shuffled_data <- data[sample(1:n_rows), ]
test_set <- shuffled_data[row.names(data) %in% 1:test_n, ]
labelled_set <- shuffled_data[row.names(data) %in% 
                       (test_n+1):(test_n + train_labelled_n), ]
unlabelled_set <- shuffled_data[row.names(data) %in% (test_n + train_labelled_n + 1):
                         (test_n + train_labelled_n + train_unlabelled_n), ]

decision_set <- shuffled_data[row.names(data) %in%
                       (test_n + train_labelled_n + train_unlabelled_n + 1):n_rows, ]

# train xgboost
dtrain <- xgb.DMatrix(data = as.matrix(labelled_set[,1:(n_cols-1)]),
                      label = as.numeric(labelled_set[,n_cols]))
classifier <- xgboost(data = dtrain, max.depth = 20, eta = 0.8, nthread = 2, nrounds = 20, objective = "binary:logistic", verbose = 0)
# train svm
# classifier = svm(x=as.matrix(labelled_set[,1:(n_cols-1)]), y=as.numeric(labelled_set[,n_cols]), kernel = "radial", cost = 1, scale = FALSE, probability= TRUE)

pred <- predict(classifier, as.matrix(test_set[1:(n_cols-1)]))

auroc <- auc_roc(preds = pred, actuals = test_set[,n_cols])

compute_fmeasure <- function(pred, actual) {
  pred_labels = pred > 0.5
  precision <- sum(pred_labels & actual) / sum(pred_labels)
  recall <- sum(pred_labels & actual) / sum(actual)
  fmeasure <- 2 * precision * recall / (precision + recall)
  return (fmeasure)
}

fmeasure = compute_fmeasure(pred, test_set[,n_cols])

sprintf("auroc: %f", auroc)
sprintf("fmeasure: %f", fmeasure)



# autolabel algorithm

# train
# eval append to result list
# pred decision set
# stop if stabilized or certain on decision set; K patience iterations, P min percent predicted the same way as before, A mean certainty (abs(probability-label)) to be achieived or boolean if it stopped to grow
# pred on M unlabelled samples
# choose N the most certain 
# label them
# extend labelled, shrink unlabelled (only chosen or all considered)
criterion_types <- list("stabilization", "certainty_growth", "certainty_threshold")
criterion_args <- list("similarity_threshold" = 0.9) # stabilization
# criterion_args <- list("highest_certainty_sofar" = 0.0) # certainty_growth
# criterion_args <- list("mean_certainty_threshold" = 0.9) # certainty_threshold
curr_patience <- 5
patience <- 5
sample_size = 100
autolabel_percent = 0.1

pred <- predict(classifier, as.matrix(decision_set[1:(n_cols-1)]))
if(!is.null(prev_pred) || criterion_type == "certainty_level"){
  stop = stop_criterion(prev_pred, pred, curr_patience, criterion_type,
                                 criterion_args)
  if(stop){
    curr_patience <- curr_patience - 1
  }else{
    curr_patience <- patience
  }
  if(curr_patience == 0)
    break
}

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

prev_pred <- pred

sample_ids <- sample(nrows(unlabelled_set), sample_size)
sample_rows <- unlabelled_set[sample_ids,]
pred <- predict(classifier, as.matrix(sample_rows[1:(n_cols-1)]))

choose_most_certain <- function(sample_rows, pred){
  percentile = quantile(pred, 1 - autolabel_percent)
  most_certain = sample_rows[pred > percentile,]
  most_certain[, n_cols] = round(pred[pred > percentile])
  return(most_certain)
}

# remove_labelled <- function(sample_rows, pred){
#   percentile = quantile(pred, 1 - autolabel_percent)
#   most_certain = sample_rows[pred > percentile,]
#   most_certain[, n_cols] = round(pred[pred > percentile])
#   unlabelled_set <- unlabelled_set[row.names(unlabelled_set) %in% row.names(most_certain)]
#   labelled_set <- rbind(labelled_set, most_certain)
# }

most_certain <- choose_most_certain(sample_rows, pred)
unlabelled_set <- unlabelled_set[row.names(unlabelled_set) %in% row.names(most_certain)]
labelled_set <- rbind(labelled_set, most_certain)






