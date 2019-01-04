source("R/config.R")

logging_levels = list()
logging_levels$DEBUG = 1
logging_levels$INFO = 2
logging_levels$WARN = 3
logging_levels$ERROR = 4
logging_levels$FATAL = 5


logging.log = function(text, level = "INFO") {
  arg_level_number = logging_levels[[ level ]]
  config_level_number = logging_levels[[ LOGGING_LEVEL ]]
  if (arg_level_number >= config_level_number) {
    print(paste(paste("[", level, "]", sep=""), text))
  }
}

logging.debug = function(text) {
  logging.log(text, "DEBUG")
}

logging.info = function(text) {
  logging.log(text, "INFO")
}

logging.warn = function(text) {
  logging.log(text, "WARN")
}

logging.error = function(text) {
  logging.log(text, "ERROR")
}

logging.fatal = function(text) {
  logging.log(text, "FATAL")
}
