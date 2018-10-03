FormatData <- function(data, dictionary) {
  for (i in 1:dim(dictionary)[1]) {
    variable <- dictionary[i, "variable"]
    type <- dictionary[i, "type"]
    label <- dictionary[i, "label"]
    values <- dictionary[i, "values"]
    if (type == "Text") {
      data[, variable] <- as.character(data[, variable])
      if ("" %in% data[, variable]) {
        data[which(data[, variable] == ""), variable] <- NA
      }
      colnames(data)[which(colnames(data) == variable)] <- label
    } else if (type == "Quantitative") {
      data[, variable] <- as.numeric(data[, variable])
      colnames(data)[which(colnames(data) == variable)] <- label
    } else if (type == "Categorical" | type == "Vector") {
      levels <- unlist(strsplit(values, " \\| "))
      pairs <- rep(NA, 2)
      for (j in 1:length(levels)) {
        level <- levels[j]
        first.comma <- regexpr(",", level)[1]
        code <- substr(level, 1, first.comma - 1)
        interpretation <- substr(level, first.comma + 2, nchar(level))
        pairs <- rbind(pairs, c(code, interpretation))
      }
      pairs <- pairs[-1, ]
      if (type == "Categorical") {
        replace <- factor(rep(NA, dim(data)[1]), levels = pairs[, 2],
                          exclude = NULL)
        for (j in 1:length(replace)) {
          if (data[j, variable] %in% pairs[, 1]) {
            index <- which(pairs[, 1] == data[j, variable])
            replace[j] <- pairs[index, 2]
          }
        }
        data[, variable] <- replace
        colnames(data)[which(colnames(data) == variable)] <- label
      } else if (type == "Vector") {
        columns <- which(substr(colnames(data), 1, nchar(variable)) == variable)
        for (j in 1:length(columns)) {
          replace <- factor(rep(NA, dim(data)[1]),
                            levels = c("CHECKED", "UNCHECKED"), exclude = NULL)
          for (k in 1:length(replace)) {
            if (data[k, columns[j]] == 1) {
              replace[k] <- "CHECKED"
            } else if (data[k, columns[j]] == 0) {
              replace[k] <- "UNCHECKED"
            }
          }
          data[, columns[j]] <- replace
          this.name <- colnames(data)[columns[j]]
          code <- substr(this.name, nchar(variable) + 4, nchar(this.name))
          index <- which(pairs[, 1] == code)
          colnames(data)[columns[j]] <- paste(label, " | ",
                                              pairs[index, 2], sep = "")
        }
      }
    }
  }
  rownames(data) <- NULL
  return(data)
}

Fixed <- function(num, digits = 1) {
  return(format(round(num, digits), trim = TRUE, nsmall = digits))
}


CategoricalTable <- function(variable) {
  missing <- sum(is.na(variable))
  counts <- table(variable)
  if (missing < length(variable)) {
    percentages <- Fixed(100 * prop.table(counts))
  } else {
    percentages <- rep(" ", length(counts))
  }
  frequencies <- paste(counts, " (", percentages, "%)", sep = "")
  levels <- names(counts)
  output <- data.frame(levels, frequencies)
  output <- rbind(output, c("Missing", missing))
  colnames(output) <- c("COL.2", "COL.3")
  rownames(output) <- NULL
  return(output)
}

QuantitativeTable <- function(variable) {
  missing <- sum(is.na(variable))
  variable <- variable[which(!is.na(variable))]
  if (length(variable) == 0) {
    output <- data.frame(c("Mean (SD)", "Median (Q1-Q3)", "Min-Max", "Missing"),
                         c(rep(" ", 3), missing))
  } else {
    mean <- Fixed(mean(variable))
    sd <- Fixed(sd(variable))
    if (length(variable) == 1) {
      sd <- " "
    }
    mean.sd <- paste(mean, " (", sd, ")", sep = "")
    median <- Fixed(median(variable))
    q1 <- Fixed(quantile(variable, 0.25))
    q3 <- Fixed(quantile(variable, 0.75))
    median.iqr <- paste(median, " (", q1, "-", q3, ")", sep = "")
    min <- Fixed(min(variable))
    max <- Fixed(max(variable))
    min.max <- paste(min, "-", max, sep = "")
    output <- data.frame(c("Mean (SD)", "Median (Q1-Q3)", "Min-Max", "Missing"),
                         c(mean.sd, median.iqr, min.max, missing))
  }
  colnames(output) <- c("COL.2", "COL.3")
  rownames(output) <- NULL
  return(output)
}

VectorTable <- function(variables) {
  n.records <- dim(variables)[1]
  n.options <- dim(variables)[2]
  output <- data.frame(rep("", n.options), rep("", n.options))
  for (i in 1:n.options) {
    output[i, 1] <- unlist(strsplit(colnames(variables)[i], " \\| "))[2]
    count <- sum(variables[, i] == "CHECKED")
    percentage <- Fixed(100 * (count / n.records))
    output[i, 2] <- paste(count, " (", percentage, "%)", sep = "")
  }
  colnames(output) <- c("COL.2", "COL.3")
  rownames(output) <- NULL
  return(output)
}

TextTable <- function(variable) {
  missing <- sum(is.na(variable))
  completed <- sum(!is.na(variable))
  n.records <- length(variable)
  completed <- paste(completed, " (",
                     Fixed(100 * (completed / n.records)), "%)",
                     sep = "")
  output <- data.frame(c("Completed", "Missing"), c(completed, missing))
  colnames(output) <- c("COL.2", "COL.3")
  rownames(output) <- NULL
  return(output)
}

UnivariateReport <- function(data, dictionary) {
  output <- data.frame("", "", paste("(N=", dim(data)[1], ")", sep = ""))
  colnames(output) <- c("COL.1", "COL.2", "COL.3")
  for (i in 1:dim(dictionary)[1]) {
    if (dictionary[i, "type"] == "Vector") {
      index <- which(substr(colnames(data), 1,
                     nchar(dictionary[i, "label"]) + 3) ==
                     paste(dictionary[i, "label"], "| "))
      tbl <- VectorTable(data[, index])
    } else if (dictionary[i, "type"] == "Text") {
      tbl <- TextTable(data[, dictionary[i, "label"]])
    } else if (dictionary[i, "type"] == "Categorical") {
      tbl <- CategoricalTable(data[, dictionary[i, "label"]])
      remove <- NA
      for (j in 1:dim(tbl)[1]) {
        if (tbl[j, "COL.2"] == "[REMOVE]") {
          remove <- append(remove, j)
        }
      }
      remove <- remove[-1]
      if (length(remove) > 0) {
        tbl <- tbl[-remove, ]
      }
    } else if (dictionary[i, "type"] == "Quantitative") {
      tbl <- QuantitativeTable(data[, dictionary[i, "label"]])
    }
    label <- c(dictionary[i, "label"], rep("", 2))
    tbl <- data.frame(COL.1 = rep("", dim(tbl)[1]), tbl)
    tbl <- rbind(label, tbl)
    output <- rbind(output, tbl)
  }
  rownames(output) <- NULL
  return(output)
}

TrimP <- function(p) {
  if (!is.nan(p) & !is.na(p)) {
    if (p < 0.001) p.value <- "<0.001"
    else if (p > 0.999) p.value <- "1"
    else p.value <- Fixed(p, 3)
  } else {
    p.value <- ""
  }
  return(p.value)
}

BivariateReport <- function(data, dictionary, variable) {
  stratifier <- data[, variable]
  reduced.dictionary <- dictionary[-which(dictionary[, "label"] == variable), ]
  split.data <- data[which(stratifier == levels(stratifier)[1]), ]
  output <- UnivariateReport(split.data, reduced.dictionary)
  for (i in 2:length(levels(stratifier))) {
    split.data <- data[which(stratifier == levels(stratifier)[i]), ]
    split.report <- UnivariateReport(split.data, reduced.dictionary)
    output <- data.frame(output, split.report[, "COL.3"])
  }
  p <- rep("", dim(output)[1])
  for (i in 1:dim(output)[1]) {
    p.value <- NA
    if (!is.na(output[i, "COL.1"]) & output[i, "COL.1"] != "") {
      type <- dictionary[which(dictionary[, "label"] == output[i, "COL.1"]),
                         "type"]
      if (type == "Quantitative") {
        main.variable <- data[, output[i, "COL.1"]]
        test <- tryCatch(anova(lm(main.variable ~ stratifier)),
                         error = function(error) "N/A",
                         warning = function(warning) "N/A")
        if (!("N/A" %in% test)) {
          p.value <- anova(lm(main.variable ~ stratifier))[1, "Pr(>F)"]
          p[i] <- TrimP(p.value)
        }
      } else if (type %in% c("Categorical", "Vector")) {
        columns <- which(substr(colnames(data), 1, nchar(output[i, "COL.1"])) ==
                         output[i, "COL.1"])
        for (j in 1:length(columns)) {
          main.variable <- data[, columns[j]]
          tbl <- table(main.variable, stratifier)
          expected <- (rowSums(tbl) %*% t(colSums(tbl))) / sum(tbl)
          if (sum(expected < 5, na.rm = TRUE) == 0) {
            test <- tryCatch(chisq.test(tbl),
                             error = function(error) "N/A",
                             warning = function(warning) "N/A")
            if (!("N/A" %in% test)) {
              p.value <- chisq.test(tbl)[["p.value"]]
            }
          } else {
            test <- tryCatch(fisher.test(tbl, simulate.p.value = TRUE,
                                         B = 100000),
                             error = function(error) "N/A",
                             warning = function(warning) "N/A")
            if (!("N/A" %in% test)) {
              p.value <- fisher.test(tbl, simulate.p.value = TRUE,
                                     B = 100000)[["p.value"]]
            }
          }
          if (type == "Categorical") {
            p[i] <- TrimP(p.value) 
          } else if (type == "Vector") {
            p[i + j] <- TrimP(p.value)
          }       
        }
      }
    }
  }
  output <- data.frame(output, p)
  output <- rbind(c(rep("", 2), levels(stratifier), "P-value"), output)
  output <- rbind(c(rep("", 2), variable,
                    rep("", length(levels(stratifier)))), output)
  colnames(output) <- paste("COL.", 1:(length(levels(stratifier)) + 3),
                            sep = "")
  rownames(output) <- NULL
  return(output)
}

options(stringsAsFactors = FALSE)

setwd("L:/Faculty & Staff/***")
data <- read.csv("data.csv")
dictionary <- read.csv("clean_dictionary.csv")

for (i in 1:dim(dictionary)[1]) {
  dictionary[i, "label"] <- paste("(", i, ") ", dictionary[i, "label"], sep = "")
}

formatted.data <- FormatData(data, dictionary)

uni.report <- UnivariateReport(formatted.data, dictionary)
write.table(uni.report, "uni_report.csv", sep = ",", na = "",
            row.names = FALSE, col.names = FALSE)

bi.report <- BivariateReport(formatted.data, dictionary, "***")
write.table(bi.report, "bi_report_.csv", sep = ",", na = "",
            row.names = FALSE, col.names = FALSE)
