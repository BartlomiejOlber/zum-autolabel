if (!require(xgboost))
  install.packages('xgboost')
if (!require(e1071))
  install.packages('e1071')
if (!require(mltools))
  install.packages('mltools')

library(e1071)
library(xgboost)
library(mltools)

source("params.R")
source("alghoritm_utils.R")
source("prepare_data.R")
set.seed(seed)


# raw_data <- read.table("ring.dat", header=FALSE, skip=25, sep = ',')
# raw_data <- read.table("magic.dat", header=FALSE, skip=16, sep=',')
raw_data <- read.table("spambase.dat", header = FALSE, skip = 63, sep = ',')
n_rows = nrow(raw_data)
n_cols = ncol(raw_data)

data <- prepare_data(raw_data)
test_set        = data$test_set
labelled_set    = data$labelled_set
unlabelled_set  = data$unlabelled_set
decision_set    = data$decision_set


# Autolabel algorithm
# 1. train
# 2. count eval metrics on test set and append them to list
# 3. prediction on decision set
# 4. stop if stabilized or certain on decision set; 
#         K patience iterations, 
#         P min percent predicted the same way as before, 
#         A mean certainty (abs(probability-label)) to be achieived or boolean if it stopped to grow
# 5. prediction on M unlabelled samples
# 6. choose N the most certain
# 7. label them
# 8. extend labelled, shrink unlabelled (only chosen or all considered)
fmeasure_results = c()
auroc_results = c()
prev_pred <- NULL
n_incorrectly_labelled <- 0
curr_patience <- patience
loop_counter <- 0
while (loop_counter < max_iterations) {
  loop_counter = loop_counter + 1
  cat(sprintf("------------------------------------------------------------\n"))
  cat(sprintf("iteration no. %d \n", loop_counter))
  
  if (use_xgb) {
    dtrain <-
      xgb.DMatrix(data = as.matrix(labelled_set[, 1:(n_cols - 1)]),
                  label = as.numeric(labelled_set[, n_cols]))
    classifier <- xgboost(
      data = dtrain,
      max.depth = xgb_max_depth,
      eta = xgb_eta,
      nthread = xgb_nthread,
      nrounds = xgb_nrounds,
      objective = xgb_objective,
      verbose = xgb_verbose
    )
  }
  else{
    classifier <- svm(
      x = as.matrix(labelled_set[, 1:(n_cols - 1)]),
      y = as.numeric(labelled_set[, n_cols]),
      kernel = svm_kernel,
      cost = svm_cost,
      scale = svm_scale,
      probability = svm_probability
    )
  }
  
  if(eval_on_test){
    test_predictions <- predict(classifier, as.matrix(test_set[1:(n_cols - 1)]))
    auroc <- auc_roc(preds = test_predictions, actuals = test_set[, n_cols])
    fmeasure <- compute_fmeasure(test_predictions, test_set[, n_cols])
    
    cat(sprintf("auroc: %f \n", auroc))
    cat(sprintf("fmeasure: %f \n", fmeasure))
    
    auroc_results <- c(auroc_results, auroc)
    fmeasure_results <- c(fmeasure_results, fmeasure)
  }
  
  
  decision_set_predictions <- predict(classifier, as.matrix(decision_set[1:(n_cols - 1)]))
  # auroc <- auc_roc(preds = decision_set_predictions, actuals = decision_set[,n_cols])
  # fmeasure <- compute_fmeasure(decision_set_predictions, decision_set[,n_cols])
  # cat(sprintf("auroc: %f \n", auroc))
  # cat(sprintf("fmeasure: %f \n", fmeasure))
  
  if (!is.null(prev_pred) || used_criterion == criterion_types$certainty_threshold) {
    if (stop_criterion(used_criterion, prev_pred, decision_set_predictions)) 
      curr_patience <- curr_patience - 1
    else
      curr_patience <- patience
    
    if (curr_patience == 0)
      break
  }
  prev_pred <- decision_set_predictions
  
  sample_ids <- sample(nrow(unlabelled_set), sample_size)
  sample_rows <- unlabelled_set[sample_ids, ]
  sample_predictions <-
    predict(classifier, as.matrix(sample_rows[1:(n_cols - 1)]))
  
  # auroc <- auc_roc(preds = sample_predictions, actuals = sample_rows[,n_cols])
  # fmeasure <- compute_fmeasure(sample_predictions, sample_rows[,n_cols])
  # cat(sprintf("auroc: %f \n", auroc))
  # cat(sprintf("fmeasure: %f \n", fmeasure))
  
  
  most_certain <- choose_most_certain(sample_rows, sample_predictions)
  unlabelled_set <- unlabelled_set[!(row.names(unlabelled_set) %in% row.names(most_certain)), ]
  labelled_set <- rbind(labelled_set, most_certain)
  cat(sprintf("labelled set size: %d \n", nrow(labelled_set)))
  
  # curiosity check
  n_incorrectly_labelled = n_incorrectly_labelled + sum(sample_rows[row.names(sample_rows) %in% row.names(most_certain), n_cols] != most_certain[, n_cols])
  n_labelled = nrow(labelled_set) - train_labelled_n
  cat(sprintf("number of incorrectly labelled: %d out of %d labelled \n", n_incorrectly_labelled, n_labelled))
  
}
# do something with the results
print(auroc_results)
print(fmeasure_results)
