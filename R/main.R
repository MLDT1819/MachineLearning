source("R/config.R")
source("R/logging.R")

library(readr)
library(ggplot2)

buildDataset = function() {
  if (!file.exists(DATASET_RAW)) {
    logging.fatal("Raw dataset not found. Aborting")
    stop("Can't build dataset. Raw dataset not found.")
  }

  dataset = readr::read_csv(DATASET_RAW, col_names = FALSE)
  logging.debug("Raw dataset loaded")

  colnames(dataset) = c("ID", "Age", "Gender", "Education", "Country", "Ethnicity",
                        "NScore", "EScore", "OScore", "AScore", "CScore", "Impulsive", "Sensation",
                        "Alcohol", "Amphet", "Amyl", "Benzos", "Caffeine", "Cannabis", "Chocolate", "Cocaine", "Crack", "Ecstasy", "Heroin", "Ketamine", "Legal", "LSD", "Meth", "Mushrooms", "Nicotine", "Semer", "VSA")
  logging.debug("Columns renamed")


  readr::write_csv(dataset, path=DATASET, col_names = TRUE)
  logging.info("Clean Dataset written")
}

main = function() {
  if (!file.exists(DATASET)) {
    logging.info("Clean dataset not found, building from raw...")
    buildDataset()
    logging.info("Clean dataset built and saved")
  }

  dataset = readr::read_csv(DATASET, col_names = TRUE)
  logging.info("Dataset loaded")
  dataset$Alcohol = as.factor(dataset$Alcohol)
  dataset$Amphet = as.factor(dataset$Amphet)
  dataset$Amyl = as.factor(dataset$Amyl)
  dataset$Benzos = as.factor(dataset$Benzos)
  dataset$Caffeine = as.factor(dataset$Caffeine)
  dataset$Cannabis = as.factor(dataset$Cannabis)
  dataset$Chocolate = as.factor(dataset$Chocolate)
  dataset$Cocaine = as.factor(dataset$Cocaine)
  dataset$Crack = as.factor(dataset$Crack)
  dataset$Ecstasy = as.factor(dataset$Ecstasy)
  dataset$Heroin = as.factor(dataset$Heroin)
  dataset$Ketamine = as.factor(dataset$Ketamine)
  dataset$Legal = as.factor(dataset$Legal)
  dataset$LSD = as.factor(dataset$LSD)
  dataset$Meth = as.factor(dataset$Meth)
  dataset$Mushrooms = as.factor(dataset$Mushrooms)
  dataset$Nicotine = as.factor(dataset$Nicotine)
  dataset$Semer = as.factor(dataset$Semer)
  dataset$VSA = as.factor(dataset$VSA)

  drug_usage = table(dataset$Cannabis)
  ggplot(as.data.frame(drug_usage), aes(x="", y=as.vector(drug_usage), fill=names(drug_usage))) +
    geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0)
}
