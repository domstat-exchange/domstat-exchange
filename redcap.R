################################################################################
#
################################################################################

CleanDictionary <- function(dictionary) {
  dictionary <- dictionary[, c(1, 4, 8, 5, 6)]
  remove <- NA
  for (i in 1:dim(dictionary)[1]) {
    if (dictionary[i, 2] %in% c("dropdown", "radio", "truefalse", "yesno")) {
      if (dictionary[i, 2] == "truefalse") {
        dictionary[i, 5] <- "1, True | 0, False"
      } else if (dictionary[i, 2] == "yesno") {
        dictionary[i, 5] <- "1, Yes | 0, No"
      }
      dictionary[i, 2] <- "Categorical"
      dictionary[i, 3] <- "Nominal"
    } else if (dictionary[i, 2] == "checkbox") {
      if (length(unlist(strsplit(dictionary[i, 5], " \\| "))) == 1) {
        code <- unlist(strsplit(dictionary[i, 5], ","))[1]
        dictionary[i, 1] <- paste(dictionary[i, 1], "___", code, sep = "")
        dictionary[i, 2] <- "Categorical"
        dictionary[i, 3] <- "Nominal"
        dictionary[i, 5] <- paste(dictionary[i, 5], " | *, [REMOVE]", sep = "")
      } else {
        dictionary[i, 2] <- "Vector"
        dictionary[i, 3] <- ""
      }
    } else if (dictionary[i, 2] %in% c("calc", "slider") |
               dictionary[i, 3] %in% c("integer", "number")) {
      dictionary[i, 2] <- "Quantitative"
      dictionary[i, 3] <- "Ratio"
      dictionary[i, 5] <- ""
    } else if (dictionary[i, 2] == "descriptive") {
      remove <- append(remove, i)
    } else {
      dictionary[i, 2] <- "Text"
      dictionary[i, 3] <- ""
      dictionary[i, 5] <- ""
    }
  }
  remove <- remove[-1]
  if (length(remove) > 0) {
    dictionary <- dictionary[-remove, ]
  }
  colnames(dictionary) <- c("variable", "type", "subtype", "label", "values")
  rownames(dictionary) <- NULL
  return(dictionary)
}

options(stringsAsFactors = FALSE)
setwd("L:/Faculty & Staff/***")

dictionary <- read.csv("DataDictionary_2015-10-20.csv")
clean.dictionary <- CleanDictionary(dictionary)
write.csv(clean.dictionary, "clean_dictionary.csv", na = "", row.names = FALSE)
