if (!require(glue))
  install.packages('glue')
library(glue)

data_to_compare <- read.csv('experiments/experiments_results/whole_data_to_compare.csv', header = TRUE, sep = ",")
f1_col <- 'final.F1'
auroc_col <- 'final.AUROC'
y_lim <- c(0.5, 1)

xgb_result <- data_to_compare[data_to_compare$classifier=='xgb', ]
xgb_f1_compare    <- xgb_result[[f1_col]][1]
xgb_auroc_compare <- xgb_result[[auroc_col]][1]
svm_result <- data_to_compare[data_to_compare$classifier=='svm', ]
svm_f1_compare    <- svm_result[[f1_col]][1]
svm_auroc_compare <- svm_result[[auroc_col]][1]

common_plot <- function(data ,X_column, desc){
  xgb_data <- data[data$classifier=='xgb',]
  xgb_F1s <- xgb_data[[f1_col]]
  xgb_AUROCs <- xgb_data[[auroc_col]]
  
  svm_data <- data[data$classifier=='svm',]
  svm_F1s <- svm_data[[f1_col]]
  svm_AUROCs <- svm_data[[auroc_col]]
  
  Xs <- xgb_data[[X_column]]
  
  # plot xgb-f1
  plot(Xs, xgb_F1s, main = glue('XGB-F1: {desc}'), type = 'o', col = 'blue', ylim = y_lim, xlab = desc, ylab = 'F1')
  abline(h = xgb_f1_compare, col='red', lty=2)
  legend(x = 'bottomright', title = 'Training:', c('autolabel', 'whole data'), lty=c(1, 2), col=c('blue','red'), lwd = 2)
  
  # plot xgb-auroc
  plot(Xs, xgb_AUROCs, main = glue('XGB-AUROC: {desc}'),type = 'o', col = 'blue', ylim = y_lim, xlab = desc, ylab = 'AUROC')
  abline(h = xgb_auroc_compare, col='red', lty=2)
  legend(x = 'bottomright', title = 'Training:', c('autolabel', 'whole data'), lty=c(1, 2), col=c('blue','red'), lwd = 2)
  
  # plot svm-f1
  plot(Xs, svm_F1s, main = glue('SVM-F1: {desc}'),type = 'o', col = 'blue', ylim = y_lim, xlab = desc, ylab = 'F1')
  abline(h = svm_f1_compare, col='red', lty=2)
  legend(x = 'bottomright', title = 'Training:', c('autolabel', 'whole data'), lty=c(1, 2), col=c('blue','red'), lwd = 2)
  
  # plot svm-auroc
  plot(Xs, svm_AUROCs, main = glue('SVM-AUROC: {desc}'),type = 'o', col = 'blue', ylim = y_lim, xlab = desc, ylab = 'AUROC')
  abline(h = svm_auroc_compare, col='red', lty=2)
  legend(x = 'bottomright', title = 'Training:', c('autolabel', 'whole data'), lty=c(1, 2), col=c('blue','red'), lwd = 2)
  
}


plot_labelled_set_size_results <- function(){
  data <- read.csv('experiments/experiments_results/labelled_set_size-certainty_threshold.csv', header = TRUE, sep = ",")

  common_plot(data, 'percentage.of.train.size.labelled', 'perc. of labelled')
}

plot_sample_size_results <- function(){
  data <- read.csv('experiments/experiments_results/sample_size-certainty_threshold.csv', header = TRUE, sep = ",")
  
  common_plot(data, 'sample_size', 'sample size')
}

plot_autolabel_perc_results <- function(){
  data <- read.csv('experiments/experiments_results/autolabel_percent_certainty_threshold.csv', header = TRUE, sep = ",")
  
  common_plot(data, 'autolabel_percent', 'autolabel percent')
}

plot_mean_certainty_threshold <- function(){
  data <- read.csv('experiments/experiments_results/mean_certainty_threshold.csv', header = TRUE, sep = ",")
  
  common_plot(data, 'mean_certainty_threshold', 'mean cert. threshold')
}

plot_similarity_threshold <- function(){
  data <- read.csv('experiments/experiments_results/similiarity_threshold.csv', header = TRUE, sep = ",")
  
  common_plot(data, 'similarity_threshold', 'similarity threshold')
}

plot_certainties <- function(){
  data <- read.csv('experiments/experiments_results/svm_certainties.csv', header = TRUE, sep = ",")
  certainties <- data[['mean_certainty']]
  plot(certainties, main = glue('SVM: mean certainty over algorithm'), type = 'l', col = 'blue', ylim = c(0.5, 1), xlab = 'iteration', ylab = 'mean certainty')
}

plot_metrics <- function(){
  data <- read.csv('experiments/experiments_results/METRICS_OVER_ALGORITHM.csv', header = TRUE, sep = ",")
  if (use_xgb){
    desc = 'XGB'
    auroc_compare <- xgb_auroc_compare
    f1_compare <- xgb_f1_compare
  }else{
    desc = 'SVM'
    auroc_compare <- svm_auroc_compare
    f1_compare <- svm_f1_compare
  }
  aurocs <- data[['auroc']]
  f1s <- data[['f1']]
  
  plot(aurocs, main = desc, type = 'l', col = 'blue', ylim = c(0.5, 1), xlab = 'iteration', ylab = 'AUROC')
  abline(h = auroc_compare, col='red', lty=2)
  
  plot(f1s, main = desc, type = 'l', col = 'blue', ylim = c(0.5, 1), xlab = 'iteration', ylab = 'F1')
  abline(h = f1_compare, col='red', lty=2)
}

plot_eval_on_other <- function(){
  data <- read.csv('experiments/experiments_results/EVAL_ON_OTHER_METRICS.csv', header = TRUE, sep = ",")
  ylim = c(0, 1)
  
  svm_to_xgb_result <- data[data$variant=='SVM to XGB', ]
  svm_to_xgb_final_f1_compare      <- svm_to_xgb_result[['final_f1']][1]
  svm_to_xgb_final_auroc_compare   <- svm_to_xgb_result[['final_auroc']][1]
  svm_to_xgb_initial_f1_compare    <- svm_to_xgb_result[['initial_f1']][1]
  svm_to_xgb_initial_auroc_compare <- svm_to_xgb_result[['initial_auroc']][1]
  
  xgb_to_svm_result <- data[data$variant=='XGB to SVM', ]
  xgb_to_svm_final_f1_compare      <- xgb_to_svm_result[['final_f1']][1]
  xgb_to_svm_final_auroc_compare   <- xgb_to_svm_result[['initial_auroc']][1]
  xgb_to_svm_initial_f1_compare    <- xgb_to_svm_result[['final_f1']][1]
  xgb_to_svm_initial_auroc_compare <- xgb_to_svm_result[['initial_auroc']][1]
  
  # plot xgb f1
  barplot(c(svm_to_xgb_final_f1_compare, svm_to_xgb_initial_f1_compare, xgb_f1_compare), names.arg =c('S->X', 'XGB labl', 'XGB all'), ylim = ylim, main = 'XGB-F1')
  
  # plot svm f1
  barplot(c(xgb_to_svm_final_f1_compare, svm_to_xgb_initial_auroc_compare, svm_f1_compare), names.arg =c('X->S', 'SVM labl', 'SVM all'), ylim = ylim, main = 'SVM-F1')
  
  # plot xgb auroc
  barplot(c(svm_to_xgb_final_auroc_compare, svm_to_xgb_initial_auroc_compare, xgb_auroc_compare), names.arg =c('S->X', 'XGB labl', 'XGB all'), ylim = ylim, main = 'XGB-AUROC')
  
  # plot svm auroc
  barplot(c(xgb_to_svm_final_auroc_compare, xgb_to_svm_initial_auroc_compare, svm_auroc_compare), names.arg =c('X->S', 'SVM labl', 'SVM all'), ylim = ylim, main = 'SVM-AUROC') 
  
}



#plot_labelled_set_size_results()
#plot_sample_size_results()
#plot_autolabel_perc_results()
#plot_certainties()
#plot_mean_certainty_threshold()
#plot_similarity_threshold()
#plot_metrics()
plot_eval_on_other()