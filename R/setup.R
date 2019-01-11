downloadDataset = function() {
  DATASET_URL = "https://archive.ics.uci.edu/ml/machine-learning-databases/00373/drug_consumption.data"
  download.file(DATASET_URL, "data/dataset_raw.csv")
}

setupPackages = function() {
  packages = c("readr", "ggplot2", "pROC", "neuralnet", "dplyr", "tidyr", "viridis", "here", "e1071", "rpart", "rattle", "randomForest", "caret", "reshape", "pROC")
  if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))
  }
}

setup = function() {
  setupPackages()
  #downloadDataset()
}
