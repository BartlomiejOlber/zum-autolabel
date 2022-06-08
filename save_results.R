
save_results <- function(new_row_vec){
  experiments_csv_name <- 'experiments/experiments_results/CURRENT.csv'
  column_names <- c(
    'classifier', 'dataset', 'final AUROC', 'final F1', 'percentage of train size labelled', 
    'initially labelled', 'labelled', 'incorrectly laballed', 'iterations', 'sample_size', 'autolabel_percent', 
    'criterion_used', 'mean_certainty_threshold', 'similarity_threshold'
    )
  
  if (file.exists(experiments_csv_name)){
    df <- read.csv(experiments_csv_name, header = FALSE, sep = ",", skip = 1)
  }else{
    df <- data.frame(matrix(ncol = length(column_names), nrow = 0))
  }
  colnames(df) <- column_names
  
  new_row <- data.frame(t(new_row_vec))
  colnames(new_row) <- column_names
  df <- rbind(df, new_row)

  write.csv(df, experiments_csv_name, row.names = FALSE)
}

save_certainties <- function(certainties){
  file_name <- 'experiments/experiments_results/CERTAINTIES.csv'
  column_names <- c('mean_certainty')
  df <- data.frame(certainties)
  colnames(df) <- column_names
  
  write.csv(df, file_name, row.names = FALSE)
}

save_metrics_over_algorithm <- function(aurocs, f1s){
  file_name <- 'experiments/experiments_results/METRICS_OVER_ALGORITHM.csv'
  column_names <- c('auroc', 'f1')
  df <- data.frame(aurocs, f1s)
  colnames(df) <- column_names

  write.csv(df, file_name, row.names = FALSE)
}

save_metrics_of_other <- function(final_auroc, final_f1, initial_auroc, initial_f1){
  file_name <- 'experiments/experiments_results/EVAL_ON_OTHER_METRICS.csv'
  
  column_names <- c('final_auroc', 'final_f1', 'initial_auroc', 'initial_f1', 'variant', 'dataset')
  if (file.exists(file_name)){
    df <- read.csv(file_name, header = FALSE, sep = ",", skip = 1)
  }else{
    df <- data.frame(matrix(ncol = length(column_names), nrow = 0))
  }
  if(use_xgb){
    variant <- 'XGB to SVM'
  }else{
    variant <- 'SVM to XGB'
  }
  colnames(df) <- column_names
  
  new_row <- data.frame(t(c(final_auroc, final_f1, initial_auroc, initial_f1, variant, dataset)))
  colnames(new_row) <- column_names
  df <- rbind(df, new_row)
  
  write.csv(df, file_name, row.names = FALSE)
}