source("R/config.R")
source("R/logging.R")
source("R/garfun.R")

set.seed(1)

library(readr)
library(tidyr)
library(dplyr)

library(neuralnet)
library(e1071)
library(rpart)
library(randomForest)

library(rattle)
library(ggplot2)

library(caret)

MODEL = "nn"

variables = c("Age", "Gender", "Education", "Country", "CountryPP", "EstimatedIncome", "NScore", "AScore", "OScore", "EScore", "CScore", "SensationSeeking", "Impulsivity", "UsedAnyOtherDrug")
#variables = c("Age", "Gender", "Education", "Country", "NScore", "AScore", "OScore", "EScore", "CScore", "SensationSeeking", "Impulsivity")
target = "Cocaine"

buildDataset = function() {
  if (!file.exists(DATASET_RAW)) {
    logging.fatal("Raw dataset not found. Aborting")
    stop("Can't build dataset. Raw dataset not found.")
  }

  dataset = readr::read_csv(DATASET_RAW, col_names = TRUE)
  logging.debug("Raw dataset loaded")

  dataset = dataset %>%
    separate(SubstanceUsage,
             c("Alcohol", "Amphet", "Amyl", "Benzos", "Caffeine", "Cannabis", "Chocolate", "Cocaine", "Crack", "Ecstasy", "Heroin", "Ketamine", "Legal", "LSD", "Meth", "Mushrooms", "Nicotine", "Semer", "VSA"),
             ";")
  logging.debug("Columns processed")

  readr::write_csv(dataset, path=DATASET, col_names = TRUE)
  logging.info("Clean Dataset written")
}

loadDataset = function() {
  if (!file.exists(DATASET)) {
    logging.info("Clean dataset not found, building from raw...")
    buildDataset()
    logging.info("Clean dataset built and saved")
  }

  dataset = readr::read_csv(DATASET, col_names = TRUE)
  logging.info("Dataset loaded")

  logging.debug("Factorizing columns")

  dataset$Gender <- as.factor(dataset$Gender)
  dataset$Age <- as.factor(dataset$Age)
  dataset$Ethnicity <- as.factor(dataset$Ethnicity)
  dataset$Education <- as.factor(dataset$Education)
  dataset$Country <- as.factor(dataset$Country)

  columns_to_factor = c("Alcohol","Amphet","Amyl","Benzos","Caffeine","Cannabis","Chocolate","Cocaine","Crack","Ecstasy","Heroin","Ketamine","Legal","LSD","Meth","Mushrooms","Nicotine","Semer","VSA")
  for (column in columns_to_factor) {
    dataset[[column]] = dataset[[column]] %>%
      dplyr::recode('CL0'=0, 'CL1'=0, .default=1)
  }
  logging.debug("Columns factorized")

  logging.info("Feature engineering")
  drugs = c("Amphet","Amyl","Benzos","Cannabis","Cocaine","Crack","Ecstasy","Heroin","Ketamine","LSD","Meth","Mushrooms","Semer","VSA")
  dataset$NOfDrugsUsed = rowSums(dataset[,drugs])
  dataset$NOfOtherDrugsUsed = rowSums(dataset[,drugs[drugs!=target]])
  dataset$UsedAnyDrug = ifelse(dataset$NOfDrugsUsed > 0, 1, 0)
  dataset$UsedAnyOtherDrug = ifelse(dataset$NOfOtherDrugsUsed > 0, 1, 0)
  logging.debug("Dataset engineered")

  return(dataset)
}

normalize = function(dataset) {
  # factor to numerics
  indx <- sapply(dataset, is.factor)
  dataset[indx] = lapply(dataset[indx], function(x) as.numeric(x))

  dataset[seq(1, ncol(dataset)-1)] = lapply(dataset[seq(1, ncol(dataset)-1)], scale)

  return(dataset)
}

stripDataset = function(dataset) {
  logging.debug("Selecting columns")
  dataset = dataset %>%
    dplyr::select(!!variables, !!target) %>%
    dplyr::rename(Target = !!target)
  logging.debug("Columns selected")

  return(dataset)
}

splitData = function(dataset, p = 0.7) {
  sample = sample.int(n = nrow(dataset), size = floor(p * nrow(dataset)), replace = F)
  train = dataset[sample, ]
  test = dataset[-sample, ]

  return(list(train=train, test=test))
}

learn = function(dataset) {
  logging.info(paste("Learning with method", MODEL))
  formula = reformulate(variables, response = 'Target')

  nFolds = 10
  totalAccuracy = 0
  for(i in 1:nFolds) {

    # dataset needs to be normalized for nn and svm
    if (MODEL == "nn" || MODEL == "svm") {
      dataset = normalize(dataset)
    }

    # factorize the target so these model can work
    if (MODEL == "rforest" || MODEL == "dtree" || MODEL == "nbayes") {
      dataset$Target <- as.factor(dataset$Target)
      # TODO: bin the data
    }

    splitted = splitData(dataset)
    train = splitted$train
    test = splitted$test
    target = test$Target
    test$Target <- NULL

    prediction = NULL
    if (MODEL == "nn") {
      nn <- neuralnet(
        formula,
        data=train,
        hidden = c(floor(length(variables)/3)+1,2,1),
        linear.output=F
      )
      prediction = compute(nn, test)
      prediction = prediction$net.result
      plot(gar.fun('y',nn))
    }

    if (MODEL == "svm") {
      model_svm = svm(formula, train)
      prediction = predict(model_svm, test)
    }

    if (MODEL == "dtree") {
      model_tree = rpart::rpart(formula, data=train, method="class")
      rattle::fancyRpartPlot(model_tree)
      plot(model_tree)
      text(model_tree)
      prediction = predict(model_tree, test, type="class")
    }

    if (MODEL == "rforest") {
      model_rforest = randomForest(formula, data = train, importance = TRUE)
      print(model_rforest)
      prediction = predict(model_rforest, test, type="class")
    }

    if (MODEL == "nbayes") {
      model_nbayes = naiveBayes(formula, data = train)
      prediction = predict(model_nbayes, test, type="class")
    }

    comparison = data.frame(target=target, prediction=prediction)
    if (MODEL != "dtree" && MODEL != "rforest" && MODEL != "nbayes") comparison = sapply(comparison, round, digits=0)
    comparison = data.frame(comparison)

    logging.info(paste("Fold:", i))
    confM = confusionMatrix(table(comparison$prediction, comparison$target))
    totalAccuracy = totalAccuracy + confM$overall['Accuracy']

    confusionDF = as.data.frame(table(comparison$prediction, comparison$target))
    # p = ggplot(confusionDF, aes(x=Var1, y=Var2)) +
    #   geom_tile(aes(fill=Freq)) +
    #   scale_x_discrete(name="Actual Class") +
    #   scale_y_discrete(name="Predicted Class") +
    #   geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
    #   #scale_fill_manual(values = c("red","green")) +
    #   #scale_fill_gradient(breaks=seq(from=-.5, to=4, by=.2)) +
    #   labs(fill="Frequency")
    # show(p)
  }
  print(paste("Average accuracy: ", totalAccuracy/nFolds))

}

main = function() {
  dataset <<- loadDataset()
  stripped_dataset = stripDataset(dataset)
  learn(stripped_dataset)
}
