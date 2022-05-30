# pretty ugly script just to see what result will give our classifiers m
# if they were trained on whole dataset - without usage of 'autolabel' algorith

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

################################################################################
# prepare data
################################################################################
dataset <- "spambase.dat"
data <- read.table(dataset, header = FALSE, skip = 63, sep = ',')
train_size                      = as.integer(n_rows * split_ratios[1])
test_size                       = as.integer(n_rows * split_ratios[2])

n_rows = nrow(data)
n_cols = ncol(data)

shuffled_data <-  data[sample(1:n_rows), ]
test_set  <-       shuffled_data[row.names(data) %in%
                                  1:test_size, ]
train_set <-       shuffled_data[row.names(data) %in%
                                   (test_size+1):n_rows, ]

################################################################################
# choose classifier and calculate metrics
################################################################################
if (use_xgb) {
  dtrain <-
    xgb.DMatrix(data = as.matrix(train_set[, 1:(n_cols - 1)]),
                label = as.numeric(train_set[, n_cols]))
  classifier <- xgboost(
    data = dtrain,
    max.depth = xgb_max_depth,
    eta = xgb_eta,
    nthread = xgb_nthread,
    nrounds = xgb_nrounds,
    objective = xgb_objective,
    verbose = xgb_verbose
  )
}else{
  classifier <- svm(
    x = as.matrix(train_set[, 1:(n_cols - 1)]),
    y = as.numeric(train_set[, n_cols]),
    kernel = svm_kernel,
    cost = svm_cost,
    scale = svm_scale,
    probability = svm_probability
  )
}

test_predictions  <- predict(classifier, as.matrix(test_set[1:(n_cols - 1)]))
auroc             <- auc_roc(preds = test_predictions, actuals = test_set[, n_cols])
fmeasure          <- compute_fmeasure(test_predictions, test_set[, n_cols])

################################################################################
# save results
################################################################################
csv <- 'experiments_results/whole_data_to_compare.csv'
column_names <- c('classifier', 'dataset', 'final AUROC', 'final F1')
used_cl <- if (use_xgb) "xgb" else "svm"
result <- c(
  used_cl,
  dataset,
  auroc,
  fmeasure
)

if (file.exists(csv)){
  df <- read.csv(csv, header = FALSE, sep = ",", skip = 1)
}else{
  df <- data.frame(matrix(ncol = length(column_names), nrow = 0))
}
colnames(df) <- column_names

new_row <- data.frame(t(result))
colnames(new_row) <- column_names
df <- rbind(df, new_row)

write.csv(df, csv, row.names = FALSE)

