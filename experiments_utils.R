

update_experiments <- function(new_row_vec){
  experiments_csv_name <- 'experiments.csv'
  column_names <- c('classifier', 'dataset', 'final AUROC', 'final F1', 'labelled', 'incorrectly laballed')
  
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


