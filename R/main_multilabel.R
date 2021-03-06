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
library(pROC)

variables = c("Age", "Gender", "Education", "Country", "CountryPP", "EstimatedIncome", "NScore", "AScore", "OScore", "EScore", "CScore", "SensationSeeking", "Impulsivity")
#variables = c("Age", "Gender", "Education", "Country", "NScore", "AScore", "OScore", "EScore", "CScore", "SensationSeeking", "Impulsivity")
#target = "Cocaine"

buildDataset = function() {
  if (!file.exists(DATASET_RAW)) {
    logging.fatal("Raw dataset not found. Aborting")
    stop("Can't build dataset. Raw dataset not found.")
  }

  dataset = readr::read_csv(DATASET_RAW, col_names = TRUE)
  logging.debug("Raw dataset loaded")

  # split the drug usage columns since they are grouped together
  dataset = dataset %>%
    separate(SubstanceUsage,
             c("Alcohol", "Amphet", "Amyl", "Benzos", "Caffeine", "Cannabis", "Chocolate", "Cocaine", "Crack", "Ecstasy", "Heroin", "Ketamine", "Legal", "LSD", "Meth", "Mushrooms", "Nicotine", "Semer", "VSA"),
             ";")
  logging.debug("Columns processed")

  readr::write_csv(dataset, path=DATASET, col_names = TRUE)
  logging.info("Clean Dataset written")
}

loadDataset = function(target) {
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
  #drugs = c("Amphet","Amyl","Benzos","Cannabis","Cocaine","Crack","Ecstasy","Heroin","Ketamine","LSD","Meth","Mushrooms","Semer","VSA")
  #dataset$NOfDrugsUsed = rowSums(dataset[,drugs])
  #dataset$NOfOtherDrugsUsed = rowSums(dataset[,drugs[drugs!=target]])
  #dataset$UsedAnyDrug = ifelse(dataset$NOfDrugsUsed > 0, 1, 0)
  #dataset$UsedAnyOtherDrug = ifelse(dataset$NOfOtherDrugsUsed > 0, 1, 0)
  logging.debug("Dataset engineered")

  return(dataset)
}

normalize = function(dataset, target) {
  # factor to numerics
  indx <- sapply(dataset, is.factor)
  dataset[indx] = lapply(dataset[indx], function(x) as.numeric(x))

  # normalize between -1 and 1 with average 0 e std 1
  dataset[, !names(dataset) %in% target] = lapply(dataset[, !names(dataset) %in% target], scale)

  return(dataset)
}

# only select the columns defined in the variables array
stripDataset = function(dataset, target) {
  logging.debug("Selecting columns")
  dataset = dataset %>%
    dplyr::select(!!variables, target) # %>%
    #dplyr::rename(Target = target)
  logging.debug("Columns selected")

  return(dataset)
}

# split dataset in two parts p% train and (1-p)% test
splitData = function(dataset, p = 0.7) {
  sample = sample.int(n = nrow(dataset), size = floor(p * nrow(dataset)), replace = F)
  train = dataset[sample, ]
  test = dataset[-sample, ]

  return(list(train=train, test=test))
}

learn = function(dataset, MODEL, balance, target_vars) {
  logging.info(paste("Learning with method", MODEL))

  # build formula as Target=v1+v2+v3
  formula = as.formula(paste(paste(target_vars, collapse = " + "), paste(variables, collapse=" + "), sep=" ~ "))

  # dataset needs to be normalized for nn and svm
  if (MODEL == "nn" || MODEL == "svm") {
    dataset = normalize(dataset, target_vars)
  }

  # factorize the target so these model can work
  if (MODEL == "rforest" || MODEL == "dtree" || MODEL == "nbayes") {
    dataset[,target_vars] = lapply(dataset[,target_vars], as.factor)
    # TODO: bin the data
  }

  nFolds = 1
  totalAccuracy = 0
  totalPrecision = 0
  totalRecall = 0
  totalF1 = 0

  for(i in 1:nFolds) {
    splitted <<- splitData(dataset)
    train = splitted$train
    test <<- splitted$test
    target <<- test[,target_vars]
    test = test[, !(names(test) %in% target_vars)]

    if (balance == TRUE) {
      print("Balancing: ")
      print(table(train$Target))
      #print(table(train$Target))
      #train = ovun.sample(formula, data=train, method = "both", p=0.5, seed = 1, N=nrow(train))$data
      train = upSample(x = train, y = as.factor(train$Target))
      print(table(train$Target))
      train_ <<- train
    }

    prediction = NULL
    if (MODEL == "nn") {
      print(formula)
      nn <- neuralnet(
        formula,
        data=train,
        hidden = c(ceiling(length(variables)/3)),
        act.fct = "logistic",
        linear.output=FALSE
      )
      prediction = compute(nn, test)
      prediction = prediction$net.result
      plot(nn)
      #plot(gar.fun('y',nn))
    }

    if (MODEL == "svm") {
      model_svm = svm(formula, train)
      prediction = predict(model_svm, test)
    }

    if (MODEL == "dtree") {
      model_tree = rpart::rpart(formula, data=train, method="class")
      #rattle::fancyRpartPlot(model_tree)
      #plot(model_tree)
      #text(model_tree)
      prediction = predict(model_tree, test, type="class")
    }

    if (MODEL == "rforest") {
      model_rforest = randomForest(formula, data = train, importance = TRUE)
      #print(model_rforest)
      prediction = predict(model_rforest, test, type="class")
    }

    if (MODEL == "nbayes") {
      model_nbayes = naiveBayes(formula, data = train)
      prediction = predict(model_nbayes, test, type="class")
    }

    if (MODEL == "base") {
      prediction = rep(0, nrow(test))
      prediction[1] = 1
      prediction = factor(prediction)
    }

    # plot the roc curve and print the area under curve
    #roc_obj <- roc(target, as.numeric(prediction))
    #print(auc(roc_obj))
    #plot(ggroc(roc_obj, legacy.axes=TRUE) + geom_abline(slope=1, intercept=0, color="red", linetype=3) + theme_bw())
    prediction = data.frame(prediction)
    if (MODEL != "dtree" && MODEL != "rforest" && MODEL != "nbayes" && MODEL != "base") prediction = sapply(prediction, round, digits=0)

    gpred <<- prediction
    print(mean(target == prediction))
    # build confusion matrix
    # comparison <<- data.frame(target=target, prediction=prediction)
    # if (MODEL != "dtree" && MODEL != "rforest" && MODEL != "nbayes" && MODEL != "base") comparison = sapply(comparison, round, digits=0)
    # comparison <<- data.frame(comparison)
    # u <- union(comparison$prediction[1], comparison$target[1])
    # confusionTable = table(factor(comparison$prediction, u), factor(comparison$target, u))
    # print(confusionTable)
    #
    # #logging.info(paste("Fold:", i))
    # computedConfMatrix = confusionMatrix(confusionTable, mode = "prec_recall")
    # totalAccuracy = totalAccuracy + computedConfMatrix$overall['Accuracy']
    # totalPrecision = totalPrecision + computedConfMatrix$byClass['Precision']
    # totalRecall = totalRecall + computedConfMatrix$byClass['Recall']
    # totalF1 = totalF1 + computedConfMatrix$byClass['F1']
    #
    # #print(computedConfMatrix)
    # confusionDF = as.data.frame(confusionTable)
    # #print(confusionDF)
    # # p = ggplot(confusionDF, aes(x=Var1, y=Var2)) +
    # #   geom_tile(aes(fill=Freq)) +
    # #   scale_x_discrete(name="Actual Class") +
    # #   scale_y_discrete(name="Predicted Class") +
    # #   geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
    # #   #scale_fill_manual(values = c("red","green")) +
    # #   #scale_fill_gradient(breaks=seq(from=-.5, to=4, by=.2)) +
    # #   labs(fill="Frequency")
    # # show(p)
  }
  print(paste("Average accuracy: ", totalAccuracy/nFolds))
  print(paste("Average Precision: ", totalPrecision/nFolds))
  print(paste("Average Recall: ", totalRecall/nFolds))
  print(paste("F-measure: ", totalF1/nFolds))
}

main = function() {
  methods = c("base", "nn", "nbayes", "svm", "rforest", "dtree")
  drugs = c("Amphet", "Heroin", "Cannabis", "Cocaine", "Crack", "Chocolate", "Nicotine", "Caffeine", "Alcohol")
  for (drug in drugs) {
    cat(paste("\nDrug: ", drug))
    dataset <<- loadDataset(drug)
    stripped_dataset = stripDataset(dataset, drug)
    for (method in methods){
      tryCatch({
        learn(stripped_dataset, method, FALSE, target)
      }, error = function(e) {
        print(e)
        logging.error(paste("Can't complete training with method", method))
      })
    }
  }
}

mainTwo = function(target, method, balance) {
  dataset <<- loadDataset(target)
  stripped_dataset <<- stripDataset(dataset, target)
  learn(stripped_dataset, method, balance, target)
}
