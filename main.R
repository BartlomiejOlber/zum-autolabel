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
source("experiments_utils.R")
set.seed(seed)

data <- get_data()
test_set        = data$test_set
labelled_set    = data$labelled_set
unlabelled_set  = data$unlabelled_set
decision_set    = data$decision_set

cat(sprintf("test set size: %d \n", nrow(test_set)))
cat(sprintf("labelled set size: %d \n", nrow(labelled_set)))
cat(sprintf("unlabelled set size: %d \n", nrow(unlabelled_set)))
cat(sprintf("decision set size: %d \n", nrow(decision_set)))

N_COLS = data$n_cols
LABELLED_INITIAL_SIZE = data$train_labelled_initial_size

# Autolabel algorithm
# 1. train
# 2. count eval metrics on test set and append them to list
# 3. prediction on decision set
# 4. stop if stabilized or certain on decision set;
#         K patience iterations,
#         P min percent predicted the same way as before, a mean certainty (abs(probability-label)) to be achieved or boolean if it stopped to grow
# 5. prediction on M unlabelled samples
# 6. choose N the most certain
# 7. label them
# 8. extend labelled, shrink unlabelled (only chosen or all considered)
fmeasure_results = c()
auroc_results = c()
prev_decision_set_predictions <- NULL
n_incorrectly_labelled <- 0
curr_patience <- patience
loop_counter <- 0
while (loop_counter < max_iterations) {
  loop_counter = loop_counter + 1
  cat(sprintf("------------------------------------------------------------\n"))
  cat(sprintf("iteration no. %d \n", loop_counter))
  
  if (use_xgb) {
    dtrain <-
      xgb.DMatrix(data = as.matrix(labelled_set[, 1:(N_COLS - 1)]),
                  label = as.numeric(labelled_set[, N_COLS]))
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
      x = as.matrix(labelled_set[, 1:(N_COLS - 1)]),
      y = as.numeric(labelled_set[, N_COLS]),
      kernel = svm_kernel,
      cost = svm_cost,
      scale = svm_scale,
      probability = svm_probability
    )
  }
  
  if (eval_on_test) {
    test_predictions  <- predict(classifier, as.matrix(test_set[1:(N_COLS - 1)]))
    auroc             <- auc_roc(preds = test_predictions, actuals = test_set[, N_COLS])
    fmeasure          <- compute_fmeasure(test_predictions, test_set[, N_COLS])
    
    auroc_results     <- c(auroc_results, auroc)
    fmeasure_results  <- c(fmeasure_results, fmeasure)
  }
  
  
  decision_set_predictions <- predict(classifier, as.matrix(decision_set[1:(N_COLS - 1)]))
  if (!is.null(prev_decision_set_predictions) || criterion_used == CRITERION_TYPES$certainty_threshold) {
    
    if (check_stop_criterion(criterion_used, prev_decision_set_predictions, decision_set_predictions))
      curr_patience <- curr_patience - 1
    else
      curr_patience <- patience
    
    if (curr_patience == 0)
      break
  }
  prev_decision_set_predictions <- decision_set_predictions
  
  sample_ids          <- sample(nrow(unlabelled_set), sample_size)
  sample_rows         <- unlabelled_set[sample_ids,]
  sample_predictions  <- predict(classifier, as.matrix(sample_rows[1:(N_COLS - 1)]))
  
  most_certain        <- choose_most_certain(sample_rows, sample_predictions)
  unlabelled_set      <- unlabelled_set[!(row.names(unlabelled_set) %in% row.names(most_certain)),]
  labelled_set        <- rbind(labelled_set, most_certain)
  cat(sprintf("labelled set size: %d \n", nrow(labelled_set)))
  
  # curiosity check
  n_incorrectly_labelled  = n_incorrectly_labelled + sum(sample_rows[row.names(sample_rows) %in% row.names(most_certain), N_COLS] != most_certain[, N_COLS])
  n_labelled              = nrow(labelled_set) - LABELLED_INITIAL_SIZE
  cat(sprintf("number of incorrectly labelled: %d out of %d labelled \n", n_incorrectly_labelled, n_labelled))
}
# do something with the results
print(auroc_results)
print(fmeasure_results)

# save experiment result
used_cl <- if (use_xgb) "xgb" else "svm"
experiment_summary <- c(
  used_cl,
  dataset,
  auroc_results[length(auroc_results)],
  fmeasure_results[length(fmeasure_results)],
  LABELLED_INITIAL_SIZE,
  n_labelled,
  n_incorrectly_labelled,
  loop_counter
)
update_experiments(experiment_summary)
