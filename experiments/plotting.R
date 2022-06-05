data_to_compare <- read.csv('experiments_results/whole_data_to_compare.csv', header = TRUE, sep = ",")
f1_col <- 'final.F1'
auroc_col <- 'final.AUROC'
y_lim <- c(0.5, 1)

xgb_result <- data_to_compare[data_to_compare$classifier=='xgb', ]
xgb_f1_compare    <- xgb_result[[f1_col]][1]
xgb_auroc_compare <- xgb_result[[auroc_col]][1]
svm_result <- data_to_compare[data_to_compare$classifier=='svm', ]
svm_f1_compare    <- svm_result[[f1_col]][1]
svm_auroc_compare <- svm_result[[auroc_col]][1]




plot_labelled_set_size_results <- function(){
  data <- read.csv('experiments_results/labelled_set_size-certainty_threshold.csv', header = TRUE, sep = ",")

  xgb_data <- data[data$classifier=='xgb',]
  xgb_F1s <- xgb_data[[f1_col]]
  xgb_AUROCs <- xgb_data[[auroc_col]]
  
  svm_data <- data[data$classifier=='svm',]
  svm_F1s <- svm_data[[f1_col]]
  svm_AUROCs <- svm_data[[auroc_col]]

  percentage_of_labelled <- xgb_data[['percentage.of.train.size.labelled']]

  
  # plot xgb-f1
  plot(percentage_of_labelled, xgb_F1s, main = 'XGB-F1: perc. of labelled', type = 'o', col = 'blue', ylim = y_lim)
  abline(h = xgb_f1_compare, col='red', lty=2)
  legend(x = 'bottomright', title = 'Training:', c('autolabel', 'whole data'), lty=c(1, 2), col=c('blue','red'), lwd = 2)
  
  # plot xgb-auroc
  plot(percentage_of_labelled, xgb_AUROCs, main = 'XGB-AUROC: perc. of labelled',type = 'o', col = 'blue', ylim = y_lim)
  abline(h = xgb_auroc_compare, col='red', lty=2)
  legend(x = 'bottomright', title = 'Training:', c('autolabel', 'whole data'), lty=c(1, 2), col=c('blue','red'), lwd = 2)
  
  # plot svm-f1
  plot(percentage_of_labelled, svm_F1s, main = 'SVM-F1: perc. of labelled',type = 'o', col = 'blue', ylim = y_lim)
  abline(h = svm_f1_compare, col='red', lty=2)
  legend(x = 'bottomright', title = 'Training:', c('autolabel', 'whole data'), lty=c(1, 2), col=c('blue','red'), lwd = 2)
  
  # plot svm-auroc
  plot(percentage_of_labelled, svm_AUROCs, main = 'SVM-AUROC: perc. of labelled',type = 'o', col = 'blue', ylim = y_lim)
  abline(h = svm_auroc_compare, col='red', lty=2)
  legend(x = 'bottomright', title = 'Training:', c('autolabel', 'whole data'), lty=c(1, 2), col=c('blue','red'), lwd = 2)
  
  # TODO?: 
  # maybe compare svm and xgb? but thats a lot of plots and i dont think it will be readable
  #    and i dont know if it will have any sense since its our subjective choice of params for these algs
}


plot_labelled_set_size_results()
