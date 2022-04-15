seed = 1
split_ratios = c(0.75, 0.25)
split_train_ratios = c(0.1, 0.85, 0.05)
# use one
# data <- read.table("ring.dat", header=FALSE, skip=25) #labels - V21
# data <- read.table("winequality-white.dat", header=FALSE, skip=16, sep=',') #labels - V12
data <- read.table("satimage.dat", header=FALSE, skip=41)
n = nrow(data)
train_n = as.integer(n * split_ratios[1])
test_n = as.integer(n * split_ratios[2])
train_labelled_n = as.integer(train_n * split_train_ratios[1])
train_unlabelled_n = as.integer(train_n * split_train_ratios[2])
train_decision_n = as.integer(train_n * split_train_ratios[3])
n_rows <- nrow(data)
set.seed(seed)
shuffled_data <- data[sample(1:nrow(data)), ]
test_set <- data[row.names(data) %in% 1:test_n, ]
labelled_set <- data[row.names(data) %in% 
                       (test_n+1):(test_n + train_labelled_n), ]
unlabelled_set <- data[row.names(data) %in% (test_n + train_labelled_n + 1):
                         (test_n + train_labelled_n + train_unlabelled_n), ]

decision_set <- data[row.names(data) %in%
                       (test_n + train_labelled_n + train_unlabelled_n + 1):n, ]
