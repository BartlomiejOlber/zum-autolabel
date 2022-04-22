if (!require(xgboost)) install.packages('xgboost')
library(xgboost)
if (!require(e1071)) install.packages('e1071')
library(e1071)
if (!require(mltools)) install.packages('mltools')
library(mltools)
source("utils.R")
#algorithm
seed = 1
split_ratios = c(0.75, 0.25)
split_train_ratios = c(0.01, 0.98, 0.01)
criterion_types <- list("stabilization", "certainty_growth", "certainty_threshold")
c_type = 3
# criterion_args <- list("similarity_threshold" = 0.97) # stabilization
# criterion_args <- list("highest_certainty_sofar" = 0.0) # certainty_growth
criterion_args <- list("mean_certainty_threshold" = 0.95) # certainty_threshold
curr_patience <- 5
patience <- 5
sample_size = 200
autolabel_percent = 0.05
auroc_results <-NULL
fmeasure_results <-NULL
prev_pred <-NULL
n_incorrectly_labelled <- 0
set.seed(seed)

# load data
# data <- read.table("ring.dat", header=FALSE, skip=25, sep = ',')
# data <- read.table("magic.dat", header=FALSE, skip=16, sep=',')
data <- read.table("spambase.dat", header=FALSE, skip=63, sep = ',')
n_rows = nrow(data)
n_cols = ncol(data)

# z-score normalization
data[1:(n_cols-1)] <- scale(data[1:(n_cols-1)])

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

# autolabel algorithm

# train
# eval append to result list
# pred decision set
# stop if stabilized or certain on decision set; K patience iterations, P min percent predicted the same way as before, A mean certainty (abs(probability-label)) to be achieived or boolean if it stopped to grow
# pred on M unlabelled samples
# choose N the most certain 
# label them
# extend labelled, shrink unlabelled (only chosen or all considered)

loop_counter <- 0
while(loop_counter < 40){
  loop_counter = loop_counter + 1
  cat(sprintf("iteration no. %d \n", loop_counter))
  # train xgboost
  dtrain <- xgb.DMatrix(data = as.matrix(labelled_set[,1:(n_cols-1)]),
                        label = as.numeric(labelled_set[,n_cols]))
  classifier <- xgboost(data = dtrain, max.depth = 20, eta = 0.8, nthread = 2, nrounds = 20, objective = "binary:logistic", verbose = 0)
  # train svm
  # classifier = svm(x=as.matrix(labelled_set[,1:(n_cols-1)]), y=as.numeric(labelled_set[,n_cols]), kernel = "radial", cost = 1, scale = FALSE, probability= TRUE)
  
  pred <- predict(classifier, as.matrix(test_set[1:(n_cols-1)]))
  
  auroc <- auc_roc(preds = pred, actuals = test_set[,n_cols])
  
  fmeasure <- compute_fmeasure(pred, test_set[,n_cols])
  
  cat(sprintf("auroc: %f \n", auroc))
  cat(sprintf("fmeasure: %f \n", fmeasure))
  if(!is.null(auroc_results)){
    auroc_results <- c(auroc_results, auroc)  
  }else{
    auroc_results <- auroc
  }
  
  if(!is.null(fmeasure_results)){
    fmeasure_results <- c(fmeasure_results, fmeasure)
  }else{
    fmeasure_results <- fmeasure
  }
  
  
  pred <- predict(classifier, as.matrix(decision_set[1:(n_cols-1)]))
  
  # auroc <- auc_roc(preds = pred, actuals = decision_set[,n_cols])
  # 
  # fmeasure <- compute_fmeasure(pred, decision_set[,n_cols])
  # 
  # cat(sprintf("auroc: %f \n", auroc))
  # cat(sprintf("fmeasure: %f \n", fmeasure))
  
  if(!is.null(prev_pred) || criterion_types[c_type] == "certainty_threshold"){
    stop <- stop_criterion(prev_pred, pred)
    if(stop){
      curr_patience <- curr_patience - 1
    }else{
      curr_patience <- patience
    }
    if(curr_patience == 0)
      break
  }
  
  prev_pred <- pred
  
  sample_ids <- sample(nrow(unlabelled_set), sample_size)
  sample_rows <- unlabelled_set[sample_ids,]
  
  
  pred <- predict(classifier, as.matrix(sample_rows[1:(n_cols-1)]))
  
  # auroc <- auc_roc(preds = pred, actuals = sample_rows[,n_cols])
  # 
  # fmeasure <- compute_fmeasure(pred, sample_rows[,n_cols])
  # 
  # cat(sprintf("auroc: %f \n", auroc))
  # cat(sprintf("fmeasure: %f \n", fmeasure))
  
  
  most_certain <- choose_most_certain(sample_rows, pred)
  
  unlabelled_set <- unlabelled_set[!(row.names(unlabelled_set) %in% row.names(most_certain)),]

  labelled_set <- rbind(labelled_set, most_certain)
  
  print(nrow(labelled_set))

  # curiosity check
  n_incorrectly_labelled = n_incorrectly_labelled + sum(sample_rows[row.names(sample_rows) %in% row.names(most_certain), n_cols] != most_certain[,n_cols])  
  n_labelled = nrow(labelled_set) - train_labelled_n
  cat(sprintf("number of incorrectly labelled: %d out of %d labelled \n", n_incorrectly_labelled, n_labelled))
  
}
# do something with the results
print(auroc_results)
print(fmeasure_results)
