use_xgb <- TRUE
eval_on_test <- TRUE


criterion_types <- list(
  "stabilization"="stabilization", 
  "certainty_growth"="certainty_growth", 
  "certainty_threshold"="certainty_threshold")
used_criterion = criterion_types$stabilization
criterion_args <- list(
  "similarity_threshold" = 0.97, # stabilization
  "highest_certainty_sofar" = 0.0, # certainty_growth
  "mean_certainty_threshold" = 0.95 # certainty_threshold
  ) 
seed = 1
max_iterations <- 40
split_ratios = c(0.75, 0.25)
split_train_ratios = c(0.01, 0.98, 0.01)
sample_size = 200
autolabel_percent = 0.05
patience <- 5

# XGBOOST
xgb_max_depth <- 20
xgb_eta <- 0.8
xgb_nthread <- 2
xgb_nrounds <- 20
xgb_objective <- "binary:logistic"
xgb_verbose <- 0

# SVM
svm_kernel <- "radial"
svm_cost <-1
svm_scale <- FALSE
svm_probability <- TRUE