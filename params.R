use_xgb <- FALSE
eval_on_test <- TRUE
dataset <- "spambase" # "magic, "ring"


CRITERION_TYPES <- list(
  "stabilization"="stabilization", 
  "certainty_growth"="certainty_growth", 
  "certainty_threshold"="certainty_threshold")
criterion_used = CRITERION_TYPES$certainty_threshold
criterion_args <- list(
  "similarity_threshold" = 0.97, # stabilization
  "highest_certainty_sofar" = 0.0, # certainty_growth
  "mean_certainty_threshold" = 0.95 # certainty_threshold
  ) 
seed = 1
max_iterations <- 100
split_ratios = c(0.75, 0.25) # (train, test)
split_train_ratios = c(0.01, 0.98, 0.01) # (labelled, unlabelled, decision)
sample_size = 400
autolabel_percent = 0.1
patience <- 5

###### XGBOOST
xgb_max_depth <- 20
xgb_eta <- 0.8
xgb_nrounds <- 20
xgb_nthread <- 2
xgb_objective <- "binary:logistic"
xgb_verbose <- 0

###### SVM
svm_kernel <- "radial"
svm_cost <-1
svm_scale <- FALSE
svm_probability <- TRUE