# TODO: Add comment 
# Author: Brad 
# File: FA_Analysis.R 
# Version: 1.0 
# Date: 07.10.2013 
# Purpose: Analyze Fund Advertising

###############################################################################
cat("SECTION: INITIAL SETUP", "\n")
############################################################################### 

# Clear workspace
rm(list = ls(all = TRUE))

# Limit History to not exceed 50 lines
Sys.setenv(R_HISTSIZE = 500)

repo <- c("http://cran.us.r-project.org")
options(repos = structure(repo))
options(install.packages.check.source = FALSE)
# String as factors is False -- used for read.csv
options(StringsAsFactors = FALSE)

# Default maxprint option
options(max.print = 500)
# options(max.print=99999)

# Memory limit
memory.limit(size = 8183)

# Set location (1=HOME,2=WORK,3=CORALSEA FROM HOME,4=CORALSEA FROM WORK) Location <- 1
Location <- 1

if (Location == 1) {
  #setwd("")
  input_directory <- normalizePath("", winslash = "\\", mustWork = NA)
  output_directory <- normalizePath("", winslash = "\\", mustWork = NA)
  function_directory <- normalizePath("C:/Users/Brad/Dropbox/Research/R/", winslash = "\\", mustWork = NA)
  
} else if (Location == 2) {
  #setwd("H:/Research/Mutual_Fund_Advertising/R/")
  input_directory <- normalizePath("H:/Research/Mutual_Fund_Advertising/Data/", winslash = "\\", mustWork = NA)
  output_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research/Fund_Advertising/R/", winslash = "\\", mustWork = NA)
  function_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research/R/", winslash = "\\", mustWork = NA)
  
} else if (Location == 3) {
  #setwd("H:/Research/Mutual_Fund_Advertising/R/")
  input_directory <- normalizePath("H:/Research/Mutual_Fund_Advertising/Data/", winslash = "\\", mustWork = NA)
  output_directory <- normalizePath("//tsclient/C/Users/Brad/Dropbox/Research/Fund_Advertising/R/", winslash = "\\", mustWork = NA)
  function_directory <- normalizePath("//tsclient/C/Users/Brad/Dropbox/Research/R/", winslash = "\\", mustWork = NA)
  
} else if (Location == 4) {
  #setwd("H:/Research/Mutual_Fund_Advertising/R/")
  input_directory <- normalizePath("H:/Research/Mutual_Fund_Advertising/Data/", winslash = "\\", mustWork = NA)
  output_directory <- normalizePath("//tsclient/C/Users/bdaughdr/Dropbox/Research/Fund_Advertising/R/", winslash = "\\", mustWork = NA)
  function_directory <- normalizePath("//tsclient/C/Users/bdaughdr/Dropbox/Research/R/", winslash = "\\", mustWork = NA)
  
} else {
  cat("ERROR ASSIGNING DIRECTORIES", "\n")
  
}
rm(Location)


###############################################################################
cat("SECTION: FUNCTIONS", "\n")
############################################################################### 

source(file = paste(function_directory, "functions_db.R", sep = ""), echo = FALSE)
source(file = paste(function_directory, "functions_statistics.R", sep = ""), echo = FALSE)
source(file = paste(function_directory, "functions_text_analysis.R", sep = ""), echo = FALSE)
source(file = paste(function_directory, "functions_utilities.R", sep = ""), echo = FALSE)

describe2 <- function(x) {
  
  # x <- descrip_stats_fund2[,-match('yr',names(descrip_stats_fund2))]
  
  var <- colnames(x)
  var <- as.data.frame(var, stringsAsFactors = FALSE)
  
  get_stats <- function(column, data) {
    
    # column <- 'sentences_iois' data <- x group_var <- group
    
    text01 <- paste0("var='", column, "',")
    text02 <- paste0("n=sum(!is.na(", column, ")),")
    text03 <- paste0("mean=mean(", column, ",na.rm=TRUE),")
    text04 <- paste0("sd=sd(", column, ",na.rm=TRUE),")
    text05 <- paste0("mode=names(sort(-table(", column, ")))[1],")
    text06 <- paste0("mad=mad(", column, ",na.rm=TRUE),")
    text07 <- paste0("range=max(", column, ",na.rm=TRUE)-min(", column, ",na.rm=TRUE),")
    text08 <- paste0("skew=skew(", column, ", na.rm=TRUE,type=3),")
    text09 <- paste0("kurtosis=kurtosi(", column, ", na.rm=TRUE,type=3),")
    text10 <- paste0("se=(sd(", column, ",na.rm=TRUE)/sqrt(sum(!is.na(", column, ")))),")
    text11 <- paste0("min=min(", column, ",na.rm=TRUE),")
    text12 <- paste0("decile1=quantile(", column, ", probs=0.10,na.rm=TRUE),")
    text13 <- paste0("quintile1=quantile(", column, ", probs=0.20,na.rm=TRUE),")
    text14 <- paste0("quartile1=quantile(", column, ", probs=0.25,na.rm=TRUE),")
    text15 <- paste0("decile3=quantile(", column, ", probs=0.30,na.rm=TRUE),")
    text16 <- paste0("quintile2=quantile(", column, ", probs=0.40,na.rm=TRUE),")
    text17 <- paste0("median=quantile(", column, ", probs=0.50,na.rm=TRUE),")
    text18 <- paste0("quintile3=quantile(", column, ", probs=0.60,na.rm=TRUE),")
    text19 <- paste0("decile7=quantile(", column, ", probs=0.70,na.rm=TRUE),")
    text20 <- paste0("quartile3=quantile(", column, ", probs=0.75,na.rm=TRUE),")
    text21 <- paste0("quintile4=quantile(", column, ", probs=0.80,na.rm=TRUE),")
    text22 <- paste0("decile9=quantile(", column, ", probs=0.90,na.rm=TRUE),")
    text23 <- paste0("max=max(", column, ",na.rm=TRUE)")
    
    str <- paste0("list(", text01, text02, text03, text04, text05, text06, text07, text08, text09, text10, text11, text12, text13, text14, 
                  text15, text16, text17, text18, text19, text20, text21, text22, text23, ")")
    expr <- parse(text = str)
    
    a_dt <- data.table(data)
    b <- as.data.frame(a_dt[, eval(expr)], stringsAsFactors = FALSE)
    
    return(b)
    
  }
  
  cc <- apply(var, 1, get_stats, data = x)
  dd <- do.call("rbind", cc)
  
  return(dd)
  
}

describeBy2 <- function(x, group) {
  
  # x <- descrip_stats_iois2 group <- 'yr'
  
  var <- colnames(x[, -match(group, names(x))])
  var <- as.data.frame(var, stringsAsFactors = FALSE)
  
  get_stats_yr <- function(column, data, group_var) {
    
    # column <- 'sentences_iois' data <- x group_var <- group
    
    text01 <- paste0("var='", column, "',")
    text02 <- paste0("n=sum(!is.na(", column, ")),")
    text03 <- paste0("mean=mean(", column, ",na.rm=TRUE),")
    text04 <- paste0("sd=sd(", column, ",na.rm=TRUE),")
    text05 <- paste0("mode=names(sort(-table(", column, ")))[1],")
    text06 <- paste0("mad=mad(", column, ",na.rm=TRUE),")
    text07 <- paste0("range=max(", column, ",na.rm=TRUE)-min(", column, ",na.rm=TRUE),")
    text08 <- paste0("skew=skew(", column, ", na.rm=TRUE,type=3),")
    text09 <- paste0("kurtosis=kurtosi(", column, ", na.rm=TRUE,type=3),")
    text10 <- paste0("se=(sd(", column, ",na.rm=TRUE)/sqrt(sum(!is.na(", column, ")))),")
    text11 <- paste0("min=min(", column, ",na.rm=TRUE),")
    text12 <- paste0("decile1=quantile(", column, ", probs=0.10,na.rm=TRUE),")
    text13 <- paste0("quintile1=quantile(", column, ", probs=0.20,na.rm=TRUE),")
    text14 <- paste0("quartile1=quantile(", column, ", probs=0.25,na.rm=TRUE),")
    text15 <- paste0("decile3=quantile(", column, ", probs=0.30,na.rm=TRUE),")
    text16 <- paste0("quintile2=quantile(", column, ", probs=0.40,na.rm=TRUE),")
    text17 <- paste0("median=quantile(", column, ", probs=0.50,na.rm=TRUE),")
    text18 <- paste0("quintile3=quantile(", column, ", probs=0.60,na.rm=TRUE),")
    text19 <- paste0("decile7=quantile(", column, ", probs=0.70,na.rm=TRUE),")
    text20 <- paste0("quartile3=quantile(", column, ", probs=0.75,na.rm=TRUE),")
    text21 <- paste0("quintile4=quantile(", column, ", probs=0.80,na.rm=TRUE),")
    text22 <- paste0("decile9=quantile(", column, ", probs=0.90,na.rm=TRUE),")
    text23 <- paste0("max=max(", column, ",na.rm=TRUE)")
    
    str <- paste0("list(", text01, text02, text03, text04, text05, text06, text07, text08, text09, text10, text11, text12, text13, text14, 
                  text15, text16, text17, text18, text19, text20, text21, text22, text23, ")")
    expr <- parse(text = str)
    
    a_dt <- data.table(data, c(group_var))
    b <- as.data.frame(a_dt[, eval(expr), by = group_var], stringsAsFactors = FALSE)
    
    return(b)
    
  }
  
  cc <- apply(var, 1, get_stats_yr, data = x, group_var = group)
  dd <- do.call("rbind", cc)
  
  return(dd[order(dd[, group]), ])
  
}

quantile_dvs <- function(w, data, group_var, quantile_data, quantile_col_low, quantile_col_high) {
  # w <- quintile_vars_iois[1] data <- text_stats_iois group_var <- c('wficn','yr','month') quantile_data <- quintile_vars_data_iois
  # quantile_col_low <- 'quintile1' quantile_col_high <- 'quintile4'
  
  data_trim <- data[, append(group_var, w)]
  
  quintile_data_trim <- quantile_data[quantile_data[, "var"] == w, ]
  quintile_data_trim <- quintile_data_trim[, -match("var", names(quintile_data_trim))]
  quintile_data_trim <- data.frame(quintile_data_trim, temp_q_low = NA, temp_q_high = NA)
  
  data_full <- merge(data_trim, quintile_data_trim, by.x = c("yr"), by.y = c("yr"), all.x = TRUE, all.y = FALSE, sort = TRUE, suffixes = c(".x", 
                                                                                                                                           ".y"), incomparables = NA)
  
  data_full[, "temp_q_low"] <- ifelse((data_full[, w] < data_full[, quantile_col_low]), 1, 0)
  data_full[, "temp_q_low"] <- ifelse(is.na(data_full[, w]), NA, data_full[, "temp_q_low"])
  data_full[, "temp_q_high"] <- ifelse((data_full[, w] > data_full[, quantile_col_high]), 1, 0)
  data_full[, "temp_q_high"] <- ifelse(is.na(data_full[, w]), NA, data_full[, "temp_q_high"])
  
  colnames(data_full)[match("temp_q_low", names(data_full))] <- paste(w, "below", quantile_col_low, sep = "_")
  colnames(data_full)[match("temp_q_high", names(data_full))] <- paste(w, "above", quantile_col_high, sep = "_")
  
  return(data_full[, -match(c(w, quantile_col_low, quantile_col_high), names(data_full))])
  
}

quantile_cast_merge <- function(w, group, quantile_num) {
  # w <- quintiles_melt[quintiles_melt[,'yr']==1999,] group <- 'fyear' quantile_num <- 5
  
  merge_table <- unique(w[, c("temp_id", group, "variable")])
  quintile_u <- sort(unique(w[, "quantile"]))
  
  for (i in 1:length(quintile_u)) {
    # i <- 1
    
    v <- w[w[, "quantile"] == quintile_u[i], ]
    v <- v[!is.na(v[, "value"]), ]
    colnames(v)[match(c("value"), names(v))] <- quintile_u[i]
    v[, "temp_id"] <- seq(1, nrow(v))
    merge_table <- merge(merge_table, v[, -match(c("quantile"), names(v))], by.x = c("temp_id", group, "variable"), by.y = c("temp_id", 
                                                                                                                             group, "variable"), all.x = TRUE, all.y = FALSE, sort = TRUE, suffixes = c(".x", ".y"), incomparables = NA)
    
  }
  merge_table <- merge_table[!(rowSums(is.na(merge_table[, 4:ncol(merge_table)])) == quantile_num), ]
  
  return(merge_table)
}

quantile_cast_cuts <- function(z, split_var, quantile_num) {
  # z <- data_trim[data_trim[,'yr']==1999,] split_var <- x quantile_num <- 5
  eps <- .Machine$double.eps
  df <- data.frame(z, quantile = as.integer(with(z, cut(z[, split_var], breaks = quantile(z[, split_var], probs = (0:quantile_num)/quantile_num, 
                                                                                          na.rm = TRUE) + eps * (0:quantile_num), include.lowest = TRUE))), stringsAsFactors = FALSE)
  return(df)
}

quantile_yr_cast <- function(x, data, dep_var, group_var, quantile_count) {
  
  # x <- univariate_vars[21] x <- univariate_vars[27]
  
  # data <- data_all_univariate dep_var <- 'pct_flow' group_var <- 'yr' quantile_count <- 5
  
  data_trim <- data[, c(dep_var, group_var, x)]
  data_trim <- data_trim[!is.na(data_trim[, x]), ]
  
  quantiles <- ddply(data_trim, group_var, quantile_cast_cuts, split_var = x, quantile_num = quantile_count)
  
  quantiles_melt <- melt(quantiles, c(group_var, "quantile"), dep_var)
  quantiles_melt <- ddply(quantiles_melt, c("yr", "variable"), function(y) {
    data.frame(temp_id = seq(1, nrow(y)), y, stringsAsFactors = FALSE)
  })
  
  quantiles_melt_cast <- ddply(quantiles_melt, c("yr"), quantile_cast_merge, quantile_num = quantile_count)
  quantiles_melt_cast <- subset(quantiles_melt_cast, select = -temp_id)
  
  return(data.frame(cut_var = x, quantiles_melt_cast, stringsAsFactors = FALSE))
  
}

quantile_cast <- function(x, data, dep_var, group_var, quantile_count) {
  
  # x <- univariate_vars[21] x <- univariate_vars[27]
  
  # data <- data_all_univariate dep_var <- 'pct_flow' group_var <- 'yr' quantile_count <- 5
  
  data_trim <- data[, c(dep_var, group_var, x)]
  data_trim <- data_trim[!is.na(data_trim[, x]), ]
  
  quantiles <- quantile_cast_cuts(data_trim, split_var = x, quantile_num = quantile_count)
  
  quantiles_melt <- melt(quantiles, c(group_var, "quantile"), dep_var)
  quantiles_melt <- ddply(quantiles_melt, c(group_var, "variable"), function(y) {
    data.frame(temp_id = seq(1, nrow(y)), y, stringsAsFactors = FALSE)
  })
  
  quantiles_melt_cast <- ddply(quantiles_melt, c(group_var), quantile_cast_merge, group = group_var, quantile_num = quantile_count)
  quantiles_melt_cast <- subset(quantiles_melt_cast, select = -temp_id)
  
  return(data.frame(cut_var = x, quantiles_melt_cast, stringsAsFactors = FALSE))
  
}

diff_in_mean <- function(x, var_col, yr_col, quantile_first_col, quantile_last_col) {
  # x <- quintiles_pct_flow var_col <- 'cut_var' yr_col <- 'yr' quantile_first_col <- 'X1' quantile_last_col <- 'X5'
  
  averages_quantile_cast <- ddply(x, c(yr_col, var_col), function(z) {
    stats <- suppressWarnings(as.data.frame(describe(z[, -match(c(yr_col, var_col), names(z))], na.rm = TRUE, skew = FALSE, range = FALSE), 
                                            stringsAsFactors = FALSE))
    stats[, "var"] <- row.names(stats)
    return(stats)
  })
  
  colnames(averages_quantile_cast)[match(c("var"), names(averages_quantile_cast))] <- "quantile"
  averages_quantile_cast2 <- ddply(averages_quantile_cast, c(yr_col), function(z) {
    return(suppressMessages(dcast(z[c(yr_col, "quantile", var_col, "mean")], cut_var ~ quantile)))
  })
  
  averages_quantile_cast_ttest <- ddply(x, c(yr_col, var_col), function(z) {
    # z <- x[x[,'variable']=='pct_flow',]
    
    ftest_results <- var.test(z[, quantile_first_col], z[, quantile_last_col])
    ttest_results <- t.test(z[, quantile_first_col], z[, quantile_last_col])
    test_data <- data.frame(t_minus_b = (ttest_results$estimate[2] - ttest_results$estimate[1]), t_stat = ttest_results$statistic, t_p_val = ttest_results$p.value, 
                            f_stat = ftest_results$statistic, f_p_val = ftest_results$p.value)
    return(test_data)
  })
  
  combined_table <- merge(averages_quantile_cast2, averages_quantile_cast_ttest, by.x = c(yr_col, var_col), by.y = c(yr_col, var_col), all.x = TRUE, 
                          all.y = FALSE, sort = FALSE, suffixes = c(".x", ".y"), incomparables = NA)
  
  return(combined_table)
  
}

expand.dft <- function(x, var, na.strings = "NA", as.is = FALSE, dec = ".") {
  
  # x <- test var <- 'cshr' na.strings <-
  
  # Take each row in the source data frame table and replicate it # using the Freq value
  DF <- sapply(1:nrow(x), function(i) x[rep(i, each = x[i, c(var)]), ], simplify = FALSE)
  
  # Take the above list and rbind it to create a single DF Also subset the result to eliminate the Freq column
  DF <- do.call("rbind", DF)
  DF <- DF[, -match(var, names(DF))]
  
  # Now apply type.convert to the character coerced factor columns to facilitate data type selection for each column
  DF <- as.data.frame(lapply(DF, function(x) {
    type.convert(as.character(x), na.strings = na.strings, as.is = as.is, dec = dec)
  }))
  
  # Return data frame
  return(DF)
  
}

r2 <- function(x, adj = TRUE) {
  
  pmodel.response <- plm:::pmodel.response.plm
  
  ## fetch response and residuals
  y <- pmodel.response(x, model = "within")
  myres <- resid(x)
  n <- length(myres)
  
  if (adj) {
    adjssr <- x$df.residual
    adjtss <- n - 1
  } else {
    adjssr <- 1
    adjtss <- 1
  }
  
  ssr <- sum(myres^2)/adjssr
  tss <- sum(y^2)/adjtss
  return(1 - ssr/tss)
}

stata.clustering <- function(x, clu, res) {
  x <- as.matrix(x)
  clu <- as.vector(clu)
  res <- as.vector(res)
  fac <- unique(clu)
  num.fac <- length(fac)
  num.reg <- ncol(x)
  u <- matrix(NA, nrow = num.fac, ncol = num.reg)
  meat <- matrix(NA, nrow = num.reg, ncol = num.reg)
  
  # outer terms (X'X)^-1
  outer <- solve(t(x) %*% x)
  
  # inner term sum_j u_j'u_j where u_j = sum_i e_i * x_i
  for (i in seq(num.fac)) {
    index.loop <- clu == fac[i]
    res.loop <- res[index.loop]
    x.loop <- x[clu == fac[i], ]
    u[i, ] <- as.vector(colSums(res.loop * x.loop))
  }
  inner <- t(u) %*% u
  
  # 
  V <- outer %*% inner %*% outer
  return(V)
}

extract.plm2 <- function(model, vcov. = NULL, ...) {
  require("sandwich")
  if ("plm" %in% class(model) == FALSE) {
    stop("Internal error: Incorrect model type! Should be a plm object!")
  }
  tab <- summary(model)$coef[, -3]
  rs <- summary(model)$r.squared[1]
  adj <- summary(model)$r.squared[2]
  n.ind1 <- length(levels(as.factor(index(model)[, 1])))
  n.ind2 <- length(levels(as.factor(index(model)[, 2])))
  n <- n.ind1 + n.ind2
  gof <- matrix(c(rs, adj, n), ncol = 1)
  row.names(gof) <- c("R$^2$", "Adj. R$^2$", "Num. obs.")
  table.content <- list(tab, gof)
  res <- table.content
  if (!is.null(vcov.)) {
    FUN <- match.fun(vcov.)
    tes <- coeftest(model, vcov = FUN(model, ...))
    res[[1]][, "Std. Error"] <- tes[, "Std. Error"]
    res[[1]][, "Pr(>|t|)"] <- tes[, "Pr(>|t|)"]
  }
  return(res)
}

###############################################################################
cat("SECTION: LIBRARIES", "\n")
############################################################################### 

update.packages(ask = FALSE, checkBuilt = TRUE)

# Load External Packages
external_packages <- c("AER", "ca", "compare", "cwhmisc", "data.table", "descr", "fastmatch", "foreign", "formatR", "gdata", "gmodel", "gtools", 
                       "Hmisc", "installr", "knitr", "leaps", "lmtest", "markdown", "memisc", "mitools", "pander", "pbapply", "pcse", "PerformanceAnalytics", 
                       "plm", "plyr", "psych", "quantreg", "R.oo", "R2wd", "reporttools", "reshape2", "rJava", "rms", "RSQLite", "RWeka", "RWekajars", "sandwich", 
                       "Snowball", "sqldf", "stargazer", "stringr", "SWordInstaller", "tcltk", "texreg", "tm", "UsingR", "xtable")
invisible(unlist(sapply(external_packages, load_external_packages, repo_str = repo, simplify = FALSE, USE.NAMES = FALSE)))
installed_packages <- list_installed_packages(external_packages)

###############################################################################
cat("SECTION: PREALLOCATE DATA", "\n")
############################################################################### 

# Create base column table
temp_data_cols <- as.data.frame(matrix(NA, ncol = 7, nrow = 5), stringsAsFactors = FALSE)
colnames(temp_data_cols) <- c("order", "isnum", "ischar", "isdate", "isfactor", "colnames", "desc")
temp_data_cols[, 1] <- as.numeric(temp_data_cols[, 1])
temp_data_cols[, 2] <- as.numeric(temp_data_cols[, 2])
temp_data_cols[, 3] <- as.numeric(temp_data_cols[, 3])
temp_data_cols[, 4] <- as.numeric(temp_data_cols[, 4])
temp_data_cols[, 5] <- as.numeric(temp_data_cols[, 5])
temp_data_cols[, 6] <- as.character(temp_data_cols[, 6])
temp_data_cols[, 7] <- as.character(temp_data_cols[, 7])

# Files table
file_list <- c("ccm.csv")
files_cols_count <- 2
files_cols <- temp_data_cols[1:files_cols_count, ]
files_cols[1, ] <- data.frame(order = 1, isnum = 0, ischar = 1, isdate = 0, isfactor = 0, colnames = "filename", stringsAsFactors = FALSE)
files_cols[2, ] <- data.frame(order = 2, isnum = 0, ischar = 1, isdate = 0, isfactor = 0, colnames = "filepath", stringsAsFactors = FALSE)
files <- as.data.frame(matrix(NA, ncol = files_cols_count, nrow = length(file_list)), stringsAsFactors = FALSE)
colnames(files) <- files_cols[, 6]
files <- format_function(files, files_cols)


############################################################################### 
cat("SECTION: IMPORT DATA", "\n")
############################################################################### 

files[, 1] <- file_list
files[, 2] <- unlist(mapply(merge_cols_function, col_one = input_directory, col_two = files[, 1], separator = "", SIMPLIFY = FALSE, USE.NAMES = FALSE))

for (j in 1:nrow(files)) {
  # j <- 1 j <- 2
  
  sample_data <- read.csv(file = files[j, 2], header = TRUE, na.strings = "NA", stringsAsFactors = FALSE)
  for (i in which(sapply(sample_data, class) == "character")) {
    sample_data[[i]] <- trim(sample_data[[i]])
  }
  for (i in 1:ncol(sample_data)) {
    sample_data[, i] <- unknownToNA(sample_data[, i], unknown = c("", ".", "n/a", "na", "NA", NA, "null", "NULL", NULL, "nan", "NaN", NaN, 
                                                                  NA_integer_, "NA_integer_", NA_complex_, "NA_complex_", NA_character_, "NA_character_", NA_real_, "NA_real_"), force = TRUE)
    sample_data[, i] <- ifelse(is.na(sample_data[, i]), NA, sample_data[, i])
  }
  
  sample_data <- sample_data[rowSums(is.na(sample_data[, 1:ncol(sample_data)])) < ncol(sample_data), ]
  
  row.names(sample_data) <- seq(nrow(sample_data))
  
  # write.csv(sample_data, file=paste(output_directory,file=files[j,1],sep=''),row.names=FALSE)
  
  progress_function(outer_loop_count = j, outer_loop_start_val = 1, outer_loop_end_val = nrow(files), inner_loop_count = 1, inner_loop_start_val = 1, 
                    inner_loop_end_val = 1)
  
}


############################################################################### 
cat("SECTION: CLEAN DATA", "\n")
############################################################################### 

begindate <- as.Date("1993-01-01")
enddate <- as.Date("1998-12-31")

aggregatevar <- c("permno")

sample_data <- sample_data[order(sample_data[, aggregatevar], sample_data[, "fyear"]), ]
row.names(sample_data) <- seq(nrow(sample_data))

colnames(sample_data) <- tolower(colnames(sample_data))

# Create unique id for each permno
q_aggregatevar_lookup <- ""
q_aggregatevar_lookup <- paste(q_aggregatevar_lookup, "select distinct ", aggregatevar, " , count(permno) as count ", sep = "")
q_aggregatevar_lookup <- paste(q_aggregatevar_lookup, "from               sample_data                              ", sep = "")
q_aggregatevar_lookup <- paste(q_aggregatevar_lookup, "group by        ", aggregatevar, "                          ", sep = "")
q_aggregatevar_lookup <- paste(q_aggregatevar_lookup, "order by        ", aggregatevar, "                          ", sep = "")
q_aggregatevar_lookup <- gsub(" {2,}", " ", q_aggregatevar_lookup)

aggregatevar_lookup <- data.frame(temp_id = as.integer(NA), sqldf(q_aggregatevar_lookup), stringsAsFactors = FALSE)
aggregatevar_lookup[, 1] <- seq(nrow(aggregatevar_lookup))
aggregatevar_lookup_name <- paste(aggregatevar, "_id", sep = "")
colnames(aggregatevar_lookup)[1] <- aggregatevar_lookup_name

q_sample_data_clean <- ""
q_sample_data_clean <- paste(q_sample_data_clean, "select          b.", aggregatevar_lookup_name, ", a.*       ", sep = "")
q_sample_data_clean <- paste(q_sample_data_clean, "from            sample_data a                               ", sep = "")
q_sample_data_clean <- paste(q_sample_data_clean, "left join       aggregatevar_lookup b                       ", sep = "")
q_sample_data_clean <- paste(q_sample_data_clean, "on              a.", aggregatevar, "=b.", aggregatevar, "    ", sep = "")
q_sample_data_clean <- paste(q_sample_data_clean, "order by        a.", aggregatevar, ",a.fyear               ", sep = "")
q_sample_data_clean <- gsub(" {2,}", " ", q_sample_data_clean)
sample_data_clean <- sqldf(q_sample_data_clean)

# rm2(sample_data)

sample_data_num_to_pad_cols <- c("permno", "permco")
for (i in 1:length(sample_data_num_to_pad_cols)) {
  sample_data_clean[, sample_data_num_to_pad_cols[i]] <- paste("", formatC(as.integer(sample_data_clean[, sample_data_num_to_pad_cols[i]]), 
                                                                           width = 8, format = "d", flag = "0"), sep = "")
  sample_data_clean[, sample_data_num_to_pad_cols[i]] <- trim(sample_data_clean[, sample_data_num_to_pad_cols[i]])
}

# sample_data_num_to_char_cols <- c('tic','cusip9','cusip8') for (i in 1:length(sample_data_num_to_char_cols)) {
# sample_data_clean[,sample_data_num_to_char_cols[i]] <- as.character(as.integer(sample_data_clean[,sample_data_num_to_char_cols[i]]))
# sample_data_clean[,sample_data_num_to_char_cols[i]] <- ifelse(sample_data_clean[,sample_data_num_to_char_cols[i]]=='0',NA,
# sample_data_clean[,sample_data_num_to_char_cols[i]]) sample_data_clean[,sample_data_num_to_char_cols[i]] <-
# trim(sample_data_clean[,sample_data_num_to_char_cols[i]]) }

sample_data_char_to_date_cols <- c("date", "datadate", "linkdt", "linkenddt")
for (i in 1:length(sample_data_char_to_date_cols)) {
  # i <- 4
  sample_data_clean[, sample_data_char_to_date_cols[i]] <- as.character(sample_data_clean[, sample_data_char_to_date_cols[i]])
  sample_data_clean[, sample_data_char_to_date_cols[i]] <- ifelse(sample_data_clean[, sample_data_char_to_date_cols[i]] == "E", as.character(format(Sys.Date(), 
                                                                                                                                                    "%Y%m%d")), sample_data_clean[, sample_data_char_to_date_cols[i]])
  sample_data_clean[, sample_data_char_to_date_cols[i]] <- gsub("-", "", sample_data_clean[, sample_data_char_to_date_cols[i]])
  sample_data_clean[, sample_data_char_to_date_cols[i]] <- trim(sample_data_clean[, sample_data_char_to_date_cols[i]])
  sample_data_clean[, sample_data_char_to_date_cols[i]] <- as.Date(paste(substr(sample_data_clean[, sample_data_char_to_date_cols[i]], 1, 
                                                                                4), substr(sample_data_clean[, sample_data_char_to_date_cols[i]], 5, 6), substr(sample_data_clean[, sample_data_char_to_date_cols[i]], 
                                                                                                                                                                7, 8), sep = "-"))
  
}

# sample_data_num_to_date_cols <- c('date','datadate','linkdt','linkenddt') for (i in 1:length(sample_data_num_to_date_cols)) {
# sample_data_clean[,sample_data_num_to_date_cols[i]] <- ifelse(sample_data_clean[,sample_data_num_to_date_cols[i]]=='E',Sys.Date(),
# sample_data_clean[,sample_data_num_to_date_cols[i]]) sample_data_clean[,sample_data_num_to_date_cols[i]] <-
# as.character(as.Date(as.integer(sample_data_clean[,names(sample_data_clean)==sample_data_num_to_date_cols[i]]), origin='1960-01-01'))
# sample_data_clean[,sample_data_num_to_date_cols[i]] <- ifelse(sample_data_clean[,sample_data_num_to_date_cols[i]]=='0',NA,
# sample_data_clean[,sample_data_num_to_date_cols[i]]) sample_data_clean[,sample_data_num_to_date_cols[i]] <-
# trim(sample_data_clean[,sample_data_num_to_date_cols[i]]) }

sample_data_char_to_num_cols <- c("mv_c", "cshi_c_adj", "prcc_c_adj", "csho_c_adj", "tstk_c_adj", "tstkn_c_adj", "mv_f", "cshi_f_adj", "prcc_f_adj", 
                                  "csho_f_adj", "tstk_f_adj", "tstkn_f_adj", "cshtr_f_adj")
for (i in 1:length(sample_data_char_to_num_cols)) {
  # i <- 1
  sample_data_clean[, sample_data_char_to_num_cols[i]] <- gsub(",", "", sample_data_clean[, sample_data_char_to_num_cols[i]])
  sample_data_clean[, sample_data_char_to_num_cols[i]] <- gsub("$", "", sample_data_clean[, sample_data_char_to_num_cols[i]])
  sample_data_clean[, sample_data_char_to_num_cols[i]] <- as.numeric(sample_data_clean[, sample_data_char_to_num_cols[i]])
  # sample_data_clean[,sample_data_char_to_num_cols[i]] <- ifelse(sample_data_clean[,sample_data_char_to_num_cols[i]]=='0',NA,
  # sample_data_clean[,sample_data_char_to_num_cols[i]]) sample_data_clean[,sample_data_char_to_num_cols[i]] <-
  # trim(sample_data_clean[,sample_data_char_to_num_cols[i]])
}

for (i in which(sapply(sample_data_clean, class) == "character")) {
  sample_data_clean[[i]] <- trim(sample_data_clean[[i]])
}
for (i in 1:ncol(sample_data_clean)) {
  sample_data_clean[, i] <- unknownToNA(sample_data_clean[, i], unknown = c("", ".", "n/a", "na", "NA", NA, "null", "NULL", NULL, "nan", 
                                                                            "NaN", NaN, NA_integer_, "NA_integer_", NA_complex_, "NA_complex_", NA_character_, "NA_character_", NA_real_, "NA_real_"), force = TRUE)
  sample_data_clean[, i] <- ifelse(is.na(sample_data_clean[, i]), NA, sample_data_clean[, i])
}

sample_data_clean <- sample_data_clean[rowSums(is.na(sample_data_clean[, 1:ncol(sample_data_clean)])) < ncol(sample_data_clean), ]

sample_data_clean <- sample_data_clean[order(sample_data_clean[, "cusip9"], sample_data_clean[, "fyear"]), ]
row.names(sample_data_clean) <- seq(nrow(sample_data_clean))

sample_data_clean <- data.frame(sample_data_clean, cshr_log = NA, inst_inv_count_log = NA, xad_log = NA, at_log = NA, mv_c_log = NA, age_last_c_log = NA, 
                                prcc_c_reciprocal = NA, avg_ret_volc_log = NA, mv_f_log = NA, age_last_f_log = NA, prcc_f_reciprocal = NA, avg_ret_volf_log = NA, hexcd_dv_nyse = NA, 
                                hexcd_dv_amex = NA, hexcd_dv_nasdaq = NA)
# sample_data_clean[,'month'] <- month(sample_data_clean[,'cshr'])
sample_data_clean[, "cshr_log"] <- suppressWarnings(log(sample_data_clean[, "cshr"]))
sample_data_clean[, "inst_inv_count_log"] <- suppressWarnings(log(sample_data_clean[, "inst_inv_count"]))
sample_data_clean[, "xad_log"] <- suppressWarnings(log(sample_data_clean[, "xad"]))
sample_data_clean[, "at_log"] <- suppressWarnings(log(sample_data_clean[, "at"]))
sample_data_clean[, "mv_c_log"] <- suppressWarnings(log(sample_data_clean[, "mv_c"]))
sample_data_clean[, "age_last_c_log"] <- suppressWarnings(log(sample_data_clean[, "age_last_c"]))
sample_data_clean[, "prcc_c_reciprocal"] <- suppressWarnings(1/sample_data_clean[, "prcc_c"])
sample_data_clean[, "avg_ret_volc_log"] <- suppressWarnings(log(sample_data_clean[, "avg_ret_volc"]))
sample_data_clean[, "mv_f_log"] <- suppressWarnings(log(sample_data_clean[, "mv_f"]))
sample_data_clean[, "age_last_f_log"] <- suppressWarnings(log(sample_data_clean[, "age_last_f"]))
sample_data_clean[, "prcc_f_reciprocal"] <- suppressWarnings(1/sample_data_clean[, "prcc_f"])
sample_data_clean[, "avg_ret_volf_log"] <- suppressWarnings(log(sample_data_clean[, "avg_ret_volf"]))
sample_data_clean[, "hexcd_dv_nyse"] <- ifelse(sample_data_clean$hexcd == 1, 1, 0)
sample_data_clean[, "hexcd_dv_amex"] <- ifelse(sample_data_clean$hexcd == 2, 1, 0)
sample_data_clean[, "hexcd_dv_nasdaq"] <- ifelse(sample_data_clean$hexcd == 3, 1, 0)

# test1_cusip <- sqldf('select distinct *, count(cusip9) as count from sample_data_clean group by cusip9, fyear order by cusip9, fyear')
# test2_cusip <- sqldf('select cusip9, fyear, count from test1_cusip where count <> 1') test3_cusip <- sqldf('select * from
# sample_data_clean where cusip9='002535300'') test1_permno <- sqldf('select distinct *, count(permno) as count from sample_data_clean
# group by permno, fyear order by permno, fyear') test2_permno <- sqldf('select permno, fyear, count from test1_permno where count <> 1')
# test <- sample_data_clean[,c('hexcd','hexcd_dv_nyse','hexcd_dv_amex','hexcd_dv_nasdaq')]


ccm_trim_c <- sample_data_clean[(sample_data_clean[, c("year")] >= as.numeric(format(begindate, format = "%Y")) & sample_data_clean[, c("year")] <= 
                                   as.numeric(format(enddate, format = "%Y"))), ]
ccm_trim_f <- sample_data_clean[(sample_data_clean[, c("fyear")] >= as.numeric(format(begindate, format = "%Y")) & sample_data_clean[, c("fyear")] <= 
                                   as.numeric(format(enddate, format = "%Y"))), ]


ccmvars_comb <- c(aggregatevar, aggregatevar_lookup_name, "year_all", "year", "fyear", "tic", "cusip8", "cusip9", "ncusip8")
ccmvars_comp <- c("at", "at_log", "oibdp", "xad", "xad_log", "cshi", "csho", "cshr", "cshr_log", "roa", "tic", "tstk", "tstkn", "inst_inv_count", 
                  "inst_inv_count_log")
ccmvars_comp_c1 <- c("prcc_c", "prcc_c_reciprocal", "prcc_c_adj", "mv_c", "mv_c_log", "tstk_c_adj", "tstkn_c_adj", "cshi_c_adj", "csho_c_adj", 
                     "cshtr_c")
ccmvars_comp_f1 <- c("prcc_f", "prcc_f_reciprocal", "prcc_f_adj", "mv_f", "mv_f_log", "tstk_f_adj", "tstkn_f_adj", "cshi_f_adj", "csho_f_adj", 
                     "cshtr_f", "cshtr_f_adj")
ccmvars_crsp <- c("avg_annual_retd_vw", "avg_annual_retd_ew", "hexcd", "hexcd_dv_nyse", "hexcd_dv_amex", "hexcd_dv_nasdaq")
ccmvars_crsp_c1 <- c("age_last_c", "age_last_c_log", "avg_agec", "avg_marketcap_crspc", "avg_marketcap_crsp_adjc", "avg_prcc", "avg_prc_adjc", 
                     "avg_retc", "avg_retc2", "avg_retxc", "avg_retxc2", "avg_ret_volc", "avg_ret_volc_log")
ccmvars_crsp_c2 <- c("avg_qspreadc", "avg_rspreadc", "avg_qspreadc2", "avg_rspreadc2", "avg_qspread_adjc", "avg_rspread_adjc", "avg_qspread_adjc2", 
                     "avg_rspread_adjc2")
ccmvars_crsp_c3 <- c("avg_total_vol_unscaledc", "avg_total_vol_rescaledc", "avg_total_vol_adj_unscaledc", "avg_total_vol_adj_rescaledc")
ccmvars_crsp_c4 <- c("avg_shrout_unscaledc", "avg_shrout_rescaledc", "avg_shrout_adj_unscaledc", "avg_shrout_adj_rescaledc")
ccmvars_crsp_c5 <- c("share_turn_unscaled_crspc", "share_turn_rescaled_crspc", "share_turn_adj_unscaled_crspc", "share_turn_adj_rescaled_crspc")
ccmvars_crsp_c6 <- c("share_turn_unscaled_crspc", "share_turn_rescaled_crspc", "share_turn_adj_unscaled_crspc", "share_turn_adj_rescaled_crspc")
ccmvars_crsp_f1 <- c("age_last_f", "age_last_f_log", "avg_agef", "avg_marketcap_crspf", "avg_marketcap_crsp_adjf", "avg_prcf", "avg_prc_adjf", 
                     "avg_retf", "avg_retf2", "avg_retxf", "avg_retxf2", "avg_ret_volf", "avg_ret_volf_log")
ccmvars_crsp_f2 <- c("avg_qspreadf", "avg_rspreadf", "avg_qspreadf2", "avg_rspreadf2", "avg_qspread_adjf", "avg_rspread_adjf", "avg_qspread_adjf2", 
                     "avg_rspread_adjf2")
ccmvars_crsp_f3 <- c("avg_total_vol_unscaledf", "avg_total_vol_rescaledf", "avg_total_vol_adj_unscaledf", "avg_total_vol_adj_rescaledf")
ccmvars_crsp_f4 <- c("avg_shrout_unscaledf", "avg_shrout_rescaledf", "avg_shrout_adj_unscaledf", "avg_shrout_adj_rescaledf")
ccmvars_crsp_f5 <- c("share_turn_unscaled_crspf", "share_turn_rescaled_crspf", "share_turn_adj_unscaled_crspf", "share_turn_adj_rescaled_crspf")


ccm_trim_c2 <- ccm_trim_c[, c(ccmvars_comb, ccmvars_comp, ccmvars_comp_c1, ccmvars_crsp, ccmvars_crsp_c1, ccmvars_crsp_c2, ccmvars_crsp_c3, 
                              ccmvars_crsp_c4, ccmvars_crsp_c5)]

ccm_trim_f2 <- ccm_trim_f[, c(ccmvars_comb, ccmvars_comp, ccmvars_comp_f1, ccmvars_crsp, ccmvars_crsp_f1, ccmvars_crsp_f2, ccmvars_crsp_f3, 
                              ccmvars_crsp_f4, ccmvars_crsp_f5)]

### Check for blank observations
ccm_trim_c3_criteria_results <- which((is.na(ccm_trim_c2[, c(aggregatevar)]) | is.na(ccm_trim_c2[, c("cusip8")]) | is.na(ccm_trim_c2[, c("cusip9")]) | 
                                         is.na(ccm_trim_c2[, c("tic")]) | is.na(ccm_trim_c2[, c("cshr")]) | is.na(ccm_trim_c2[, c("oibdp")]) | is.na(ccm_trim_c2[, c("at")]) | is.na(ccm_trim_c2[, 
                                                                                                                                                                                                 c("roa")]) | is.na(ccm_trim_c2[, c("age_last_c")]) | is.na(ccm_trim_c2[, c("avg_agec")]) | is.na(ccm_trim_c2[, c("avg_marketcap_crspc")]) | 
                                         is.na(ccm_trim_c2[, c("avg_marketcap_crsp_adjc")]) | is.na(ccm_trim_c2[, c("avg_prcc")]) | is.na(ccm_trim_c2[, c("avg_prc_adjc")]) | is.na(ccm_trim_c2[, 
                                                                                                                                                                                                c("avg_ret_volc")]) | is.na(ccm_trim_c2[, c("avg_retc")]) | is.na(ccm_trim_c2[, c("avg_retxc")]) | is.na(ccm_trim_c2[, c("avg_retc2")]) | 
                                         is.na(ccm_trim_c2[, c("avg_retxc2")]) | is.na(ccm_trim_c2[, c("avg_qspreadc")]) | is.na(ccm_trim_c2[, c("avg_rspreadc")]) | is.na(ccm_trim_c2[, 
                                                                                                                                                                                       c("avg_qspreadc2")]) | is.na(ccm_trim_c2[, c("avg_rspreadc2")]) | is.na(ccm_trim_c2[, c("avg_qspread_adjc")]) | is.na(ccm_trim_c2[, c("avg_rspread_adjc")]) | 
                                         is.na(ccm_trim_c2[, c("avg_qspread_adjc2")]) | is.na(ccm_trim_c2[, c("avg_rspread_adjc2")]) | is.na(ccm_trim_c2[, c("avg_total_vol_unscaledc")]) | 
                                         is.na(ccm_trim_c2[, c("avg_total_vol_adj_unscaledc")]) | is.na(ccm_trim_c2[, c("avg_shrout_unscaledc")]) | is.na(ccm_trim_c2[, c("avg_shrout_adj_unscaledc")]) | 
                                         is.na(ccm_trim_c2[, c("share_turn_unscaled_crspc")]) | is.na(ccm_trim_c2[, c("share_turn_adj_unscaled_crspc")])))
if (length(ccm_trim_c3_criteria_results) != 0) {
  ccm_trim_c3 <- ccm_trim_c2[-ccm_trim_c3_criteria_results, ]
  ccm_trim_c3_remove <- ccm_trim_c2[ccm_trim_c3_criteria_results, ]
  
} else {
  ccm_trim_c3 <- ccm_trim_c2[1:nrow(ccm_trim_c2), ]
  ccm_trim_c3_remove <- ccm_trim_c2[0, ]
  
}


ccm_trim_f3_criteria_results <- which((is.na(ccm_trim_f2[, c(aggregatevar)]) | is.na(ccm_trim_f2[, c("cusip8")]) | is.na(ccm_trim_f2[, c("cusip9")]) | 
                                         is.na(ccm_trim_f2[, c("tic")]) | is.na(ccm_trim_f2[, c("cshr")]) | is.na(ccm_trim_f2[, c("oibdp")]) | is.na(ccm_trim_f2[, c("at")]) | is.na(ccm_trim_f2[, 
                                                                                                                                                                                                 c("roa")]) | is.na(ccm_trim_f2[, c("age_last_f")]) | is.na(ccm_trim_f2[, c("avg_agef")]) | is.na(ccm_trim_f2[, c("avg_marketcap_crspf")]) | 
                                         is.na(ccm_trim_f2[, c("avg_marketcap_crsp_adjf")]) | is.na(ccm_trim_f2[, c("avg_prcf")]) | is.na(ccm_trim_f2[, c("avg_prc_adjf")]) | is.na(ccm_trim_f2[, 
                                                                                                                                                                                                c("avg_ret_volf")]) | is.na(ccm_trim_f2[, c("avg_retf")]) | is.na(ccm_trim_f2[, c("avg_retxf")]) | is.na(ccm_trim_f2[, c("avg_retf2")]) | 
                                         is.na(ccm_trim_f2[, c("avg_retxf2")]) | is.na(ccm_trim_f2[, c("avg_qspreadf")]) | is.na(ccm_trim_f2[, c("avg_rspreadf")]) | is.na(ccm_trim_f2[, 
                                                                                                                                                                                       c("avg_qspreadf2")]) | is.na(ccm_trim_f2[, c("avg_rspreadf2")]) | is.na(ccm_trim_f2[, c("avg_qspread_adjf")]) | is.na(ccm_trim_f2[, c("avg_rspread_adjf")]) | 
                                         is.na(ccm_trim_f2[, c("avg_qspread_adjf2")]) | is.na(ccm_trim_f2[, c("avg_rspread_adjf2")]) | is.na(ccm_trim_f2[, c("avg_total_vol_unscaledf")]) | 
                                         is.na(ccm_trim_f2[, c("avg_total_vol_adj_unscaledf")]) | is.na(ccm_trim_f2[, c("avg_shrout_unscaledf")]) | is.na(ccm_trim_f2[, c("avg_shrout_adj_unscaledf")]) | 
                                         is.na(ccm_trim_f2[, c("share_turn_unscaled_crspf")]) | is.na(ccm_trim_f2[, c("share_turn_adj_unscaled_crspf")])))
if (length(ccm_trim_f3_criteria_results) != 0) {
  ccm_trim_f3 <- ccm_trim_f2[-ccm_trim_f3_criteria_results, ]
  ccm_trim_f3_remove <- ccm_trim_f2[ccm_trim_f3_criteria_results, ]
  
} else {
  ccm_trim_f3 <- ccm_trim_f2[1:nrow(ccm_trim_f2), ]
  ccm_trim_f3_remove <- ccm_trim_f2[0, ]
  
}


### Check if shareholders or shares is 0###
ccm_trim_c4_criteria_results <- which((ccm_trim_c3[, c("csho")] == 0 | ccm_trim_c3[, c("cshr")] == 0))
if (length(ccm_trim_c4_criteria_results) != 0) {
  ccm_trim_c4 <- ccm_trim_c3[-ccm_trim_c4_criteria_results, ]
  ccm_trim_c4_remove <- ccm_trim_c3[ccm_trim_c4_criteria_results, ]
  
} else {
  ccm_trim_c4 <- ccm_trim_c3[1:nrow(ccm_trim_c3), ]
  ccm_trim_c4_remove <- ccm_trim_c3[0, ]
  
}
ccm_trim_f4_criteria_results <- which((ccm_trim_f3[, c("csho")] == 0 | ccm_trim_f3[, c("cshr")] == 0))
if (length(ccm_trim_f4_criteria_results) != 0) {
  ccm_trim_f4 <- ccm_trim_f3[-ccm_trim_f4_criteria_results, ]
  ccm_trim_f4_remove <- ccm_trim_f3[ccm_trim_f4_criteria_results, ]
  
} else {
  ccm_trim_f4 <- ccm_trim_f3[1:nrow(ccm_trim_f3), ]
  ccm_trim_f4_remove <- ccm_trim_f3[0, ]
  
}


### Create shareturnover
ccm_trim_c5 <- data.frame(ccm_trim_c4, share_turn_rescaled_compc = NA, share_turn_adj_rescaled_compc = NA)
ccm_trim_c5[, c("share_turn_rescaled_compc")] <- (ccm_trim_c5[, c("avg_total_vol_rescaledc")]/ccm_trim_c5[, c("csho")]) * 100
ccm_trim_c5[, c("share_turn_adj_rescaled_compc")] <- (ccm_trim_c5[, c("avg_total_vol_adj_rescaledc")]/ccm_trim_c5[, c("csho_c_adj")]) * 100

ccm_trim_f5 <- data.frame(ccm_trim_f4, share_turn_rescaled_compf = NA, share_turn_adj_rescaled_compf = NA)
ccm_trim_f5[, c("share_turn_rescaled_compf")] <- (ccm_trim_f5[, c("avg_total_vol_rescaledf")]/ccm_trim_f5[, c("csho")]) * 100
ccm_trim_f5[, c("share_turn_adj_rescaled_compf")] <- (ccm_trim_f5[, c("avg_total_vol_adj_rescaledf")]/ccm_trim_f5[, c("csho_f_adj")]) * 100


### Check is shareturnover is blank or >500%###
ccm_trim_c6_criteria_results <- which((is.na(ccm_trim_c5[, c("share_turn_rescaled_compc")]) | ccm_trim_c5[, c("share_turn_rescaled_compc")] > 
                                         500 | is.na(ccm_trim_c5[, c("share_turn_adj_rescaled_compc")]) | ccm_trim_c5[, c("share_turn_adj_rescaled_compc")] > 500))
if (length(ccm_trim_c6_criteria_results) != 0) {
  ccm_trim_c6 <- ccm_trim_c5[-ccm_trim_c6_criteria_results, ]
  ccm_trim_c6_remove <- ccm_trim_c5[ccm_trim_c6_criteria_results, ]
  
} else {
  ccm_trim_c6 <- ccm_trim_c5[1:nrow(ccm_trim_c5), ]
  ccm_trim_c6_remove <- ccm_trim_c5[0, ]
  
}
ccm_trim_f6_criteria_results <- which((is.na(ccm_trim_f5[, c("share_turn_rescaled_compf")]) | ccm_trim_f5[, c("share_turn_rescaled_compf")] > 
                                         500 | is.na(ccm_trim_f5[, c("share_turn_adj_rescaled_compf")]) | ccm_trim_f5[, c("share_turn_adj_rescaled_compf")] > 500))
if (length(ccm_trim_f6_criteria_results) != 0) {
  ccm_trim_f6 <- ccm_trim_f5[-ccm_trim_f6_criteria_results, ]
  ccm_trim_f6_remove <- ccm_trim_f5[ccm_trim_f6_criteria_results, ]
  
} else {
  ccm_trim_f6 <- ccm_trim_f5[1:nrow(ccm_trim_f5), ]
  ccm_trim_f6_remove <- ccm_trim_f5[0, ]
  
}


### Check is exchange code is blank###
ccm_trim_c7_criteria_results <- which((is.na(ccm_trim_c6[, c("hexcd")]) | is.na(ccm_trim_c6[, c("hexcd_dv_nyse")]) | is.na(ccm_trim_c6[, c("hexcd_dv_amex")]) | 
                                         is.na(ccm_trim_c6[, c("hexcd_dv_nasdaq")])))
if (length(ccm_trim_c7_criteria_results) != 0) {
  ccm_trim_c7 <- ccm_trim_c6[-ccm_trim_c7_criteria_results, ]
  ccm_trim_c7_remove <- ccm_trim_c6[ccm_trim_c7_criteria_results, ]
  
} else {
  ccm_trim_c7 <- ccm_trim_c6[1:nrow(ccm_trim_c6), ]
  ccm_trim_c7_remove <- ccm_trim_c6[0, ]
  
}
ccm_trim_f7_criteria_results <- which((is.na(ccm_trim_f6[, c("hexcd")]) | is.na(ccm_trim_f6[, c("hexcd_dv_nyse")]) | is.na(ccm_trim_f6[, c("hexcd_dv_amex")]) | 
                                         is.na(ccm_trim_f6[, c("hexcd_dv_nasdaq")])))
if (length(ccm_trim_f7_criteria_results) != 0) {
  ccm_trim_f7 <- ccm_trim_f6[-ccm_trim_f7_criteria_results, ]
  ccm_trim_f7_remove <- ccm_trim_f6[ccm_trim_f7_criteria_results, ]
  
} else {
  ccm_trim_f7 <- ccm_trim_f6[1:nrow(ccm_trim_f6), ]
  ccm_trim_f7_remove <- ccm_trim_f6[0, ]
  
}


ccm_advertise_c_criteria_results <- which((is.na(ccm_trim_c7[, c("xad")]) | ccm_trim_c7[, c("xad")] <= 0))
ccm_advertise_c <- ccm_trim_c7[-ccm_advertise_c_criteria_results, ]
ccm_noadvertise_c <- ccm_trim_c7[ccm_advertise_c_criteria_results, ]

ccm_advertise_f_criteria_results <- which((is.na(ccm_trim_f7[, c("xad")]) | ccm_trim_f7[, c("xad")] <= 0))
ccm_advertise_f <- ccm_trim_f7[-ccm_advertise_f_criteria_results, ]
ccm_noadvertise_f <- ccm_trim_f7[ccm_advertise_f_criteria_results, ]

ccm_advertise_c <- ccm_advertise_c[order(ccm_advertise_c[, "permno"], ccm_advertise_c[, "year"]), ]
row.names(ccm_advertise_c) <- seq(nrow(ccm_advertise_c))

ccm_advertise_f <- ccm_advertise_f[order(ccm_advertise_f[, "permno"], ccm_advertise_f[, "fyear"]), ]
row.names(ccm_advertise_f) <- seq(nrow(ccm_advertise_f))

ccm_noadvertise_c <- ccm_noadvertise_c[order(ccm_noadvertise_c[, "permno"], ccm_noadvertise_c[, "year"]), ]
row.names(ccm_noadvertise_c) <- seq(nrow(ccm_noadvertise_c))

ccm_noadvertise_f <- ccm_noadvertise_f[order(ccm_noadvertise_f[, "permno"], ccm_noadvertise_f[, "fyear"]), ]
row.names(ccm_noadvertise_f) <- seq(nrow(ccm_noadvertise_f))

rm2(ccm_trim_c, ccm_trim_c2, ccm_trim_c3, ccm_trim_c3_remove)
rm2(ccm_trim_c4, ccm_trim_c4_remove, ccm_trim_c5, ccm_trim_c6, ccm_trim_c6_remove)
rm2(ccm_trim_f, ccm_trim_f2, ccm_trim_f3, ccm_trim_f3_remove)
rm2(ccm_trim_f4, ccm_trim_f4_remove, ccm_trim_f5, ccm_trim_f6, ccm_trim_f6_remove)
rm2(ccm_trim_c3_criteria_results, ccm_trim_c4_criteria_results, ccm_trim_c6_criteria_results, ccm_advertise_c_criteria_results)
rm2(ccm_trim_f3_criteria_results, ccm_trim_f4_criteria_results, ccm_trim_f6_criteria_results, ccm_advertise_f_criteria_results)

write.dta(ccm_advertise_f, file = paste(input_directory, "ccm_advertise_f", ".dta", sep = ""))

############################################################################### 
cat("SECTION: TABLE 1 - PANEL A & B (DESCRIPTIVE STATISTICS)", "\n")
############################################################################### 

sumstatvars_c <- c("xad", "age_last_c", "mv_c", "at", "roa", "avg_retc2", "prcc_c", "avg_ret_volc", "cshr", "inst_inv_count", "avg_qspreadc", 
                   "avg_rspreadc", "share_turn_rescaled_compc", "avg_total_vol_rescaledc")
sumstatvars_f <- c("xad", "age_last_f", "mv_f", "at", "roa", "avg_retf2", "prcc_f", "avg_ret_volf", "cshr", "inst_inv_count", "avg_qspreadf", 
                   "avg_rspreadf", "share_turn_rescaled_compf", "avg_total_vol_rescaledf")

ccm_advertise_trim_c <- ccm_advertise_c[, c(sumstatvars_c)]
row.names(ccm_advertise_trim_c) <- seq(nrow(ccm_advertise_trim_c))

ccm_noadvertise_trim_c <- ccm_noadvertise_c[, c(sumstatvars_c)]
row.names(ccm_noadvertise_trim_c) <- seq(nrow(ccm_noadvertise_trim_c))

ccm_advertise_trim_f <- ccm_advertise_f[, c(sumstatvars_f)]
row.names(ccm_advertise_trim_f) <- seq(nrow(ccm_advertise_trim_f))

ccm_noadvertise_trim_f <- ccm_noadvertise_f[, c(sumstatvars_f)]
row.names(ccm_noadvertise_trim_f) <- seq(nrow(ccm_noadvertise_trim_f))

descriptive_stats0_pa <- describe2(ccm_advertise_trim_f)
# descriptive_stats0_pa <- describe2(ccm_advertise_trim_f[,-(which(sapply(ccm_advertise_trim_f,class)=='character'))])
# descriptive_stats0_pa <-
# as.data.frame(describe(ccm_advertise_trim_f[,-(which(sapply(ccm_advertise_trim_f,class)=='character'))],na.rm=TRUE,skew=TRUE,range=TRUE),stringsAsFactors=FALSE)
# descriptive_stats0_pa[,'var'] <- row.names(descriptive_stats0_pa) row.names(descriptive_stats0_pa) <- seq(nrow(descriptive_stats0_pa))
# descriptive_stats0_pa <- descriptive_stats0_pa[c('var','n','mean','sd','median','max','min')]
descriptive_stats_pa <- descriptive_stats0_pa[descriptive_stats0_pa[, "var"] %in% sumstatvars_f, ]

write.csv(descriptive_stats_pa, file = paste(output_directory, "descriptive_stats_pa", ".csv", sep = ""), na = "", quote = TRUE, row.names = FALSE)


descriptive_stats0_pb <- describe2(ccm_noadvertise_trim_f)
# descriptive_stats0_pb <- describe2(ccm_noadvertise_trim_f[,-(which(sapply(ccm_noadvertise_trim_f,class)=='character'))])
# descriptive_stats0_pb <-
# as.data.frame(describe(ccm_noadvertise_trim_f[,-(which(sapply(ccm_noadvertise_trim_f,class)=='character'))],na.rm=TRUE,skew=TRUE,range=TRUE),stringsAsFactors=FALSE)
# descriptive_stats0_pb[,'var'] <- row.names(descriptive_stats0_pb) row.names(descriptive_stats0_pb) <- seq(nrow(descriptive_stats0_pb))
# descriptive_stats0_pb <- descriptive_stats0_pb[c('var','n','mean','sd','median','max','min')]
descriptive_stats_pb <- descriptive_stats0_pb[descriptive_stats0_pb[, "var"] %in% sumstatvars_f, ]

write.csv(descriptive_stats_pb, file = paste(output_directory, "descriptive_stats_pb", ".csv", sep = ""), na = "", quote = TRUE, row.names = FALSE)

############################################################################### 
cat("SECTION: TABLE 2 (UNIVARIATE ANALYSIS)", "\n")
############################################################################### 

univariate_stats <- c("mean", "median")
univariate_vars_avg <- c("cshr", "inst_inv_count")
univariate_vars_x <- "mv_f"
univariate_vars_y <- "xad"

quantile_nums <- c(5)

for (i in 1:length(quantile_nums)) {
  # i <- 1 i <- 2
  
  
  temp_quintile_1 <- within(ccm_advertise_f, quantile_x <- as.integer(cut(get(univariate_vars_x), quantile(get(univariate_vars_x), probs = 0:quantile_nums[i]/quantile_nums[i]), 
                                                                          include.lowest = TRUE)))
  temp_quintile_2 <- within(temp_quintile_1, quantile_y <- as.integer(cut(get(univariate_vars_y), quantile(get(univariate_vars_y), probs = 0:quantile_nums[i]/quantile_nums[i]), 
                                                                          include.lowest = TRUE)))
  
  
  for (j in 1:length(univariate_stats)) {
    # j <- 1 j <- 2 j <- 3
    
    col_header <- paste(as.vector(matrix("X", ncol = quantile_nums[i], nrow = 1)), seq(1, quantile_nums[i]), sep = "")
    row_header <- rep(c(col_header, "Difference"), length(univariate_vars_avg))
    
    df_temp <- as.data.frame(matrix(NA, ncol = (quantile_nums[i] + 4), nrow = (quantile_nums[i] + 1) * length(univariate_vars_avg) + 1), 
                             stringsAsFactors = FALSE)
    colnames(df_temp) <- c("stat", "stat_var", "y_var", "quantile", col_header)
    
    df_temp[1, 1:4] <- " "
    df_temp[1, 5:ncol(df_temp)] <- univariate_vars_x
    df_temp[2:nrow(df_temp), 1] <- univariate_stats[j]
    df_temp[2:nrow(df_temp), 3] <- row_header
    df_temp[2:nrow(df_temp), 4] <- univariate_vars_y
    
    for (k in 1:length(univariate_vars_avg)) {
      # k <- 1 k <- 2
      
      univariate_temp <- with(temp_quintile_2, tapply(get(univariate_vars_avg[k]), list(quantile_x, quantile_y), get(univariate_stats[j]), 
                                                      na.rm = TRUE))
      univariate_temp <- as.data.frame(unclass(univariate_temp), stringsAsFactors = FALSE)
      differences <- apply(univariate_temp, 2, function(x) x[length(x)] - x[1])
      results <- rbind(univariate_temp, differences)
      
      df_temp[(2 + (quantile_nums[i] + 1) * (k - 1)):(1 + (quantile_nums[i] + 1) * k), 5:ncol(df_temp)] <- results
      df_temp[(2 + (quantile_nums[i] + 1) * (k - 1)):(1 + (quantile_nums[i] + 1) * k), 2] <- univariate_vars_avg[k]
    }
    
    univariate_name <- paste("univariate", univariate_stats[j], quantile_nums[i], sep = "_")
    assign(univariate_name, df_temp, envir = .GlobalEnv)
    write.csv(df_temp, file = paste(output_directory, univariate_name, ".csv", sep = ""), na = "", quote = TRUE, row.names = FALSE)
    
    progress_function(outer_loop_count = i, outer_loop_start_val = 1, outer_loop_end_val = length(quantile_nums), inner_loop_count = j, 
                      inner_loop_start_val = 1, inner_loop_end_val = length(univariate_stats))
    
  }
  
}



############################################################################### 
cat("SECTION: TABLE 3 (REGRESSION ANALYSIS)", "\n")
############################################################################### 

model_type <- c("between", "within")

dep_var <- c("cshr_log", "inst_inv_count_log")
index_vars <- c("permno_id", "fyear")

control_vars <- c("xad_log", "age_last_f_log", "avg_retf2", "roa", "mv_f_log", "prcc_f_reciprocal", "at_log", "avg_ret_volf_log")
controls <- paste(control_vars, sep = "", collapse = " + ")

dv_vars <- c("hexcd_dv_nasdaq", "hexcd_dv_amex")
dvs <- paste(dv_vars, sep = "", collapse = " + ")

year_dv <- "factor(fyear)"
firm_dv <- "factor(permno_id)"

data <- ccm_advertise_f[, c(dep_var, index_vars, control_vars, dv_vars)]
data.pd <- pdata.frame(data, index = c("permno_id", "fyear"), drop.index = FALSE, row.names = TRUE)

model_lookup <- data.frame(num = integer(), data = character(), index_individual = character(), index_time = character(), type = character(), 
                           dep_var = character(), ind_var = character(), formula = character(), stringsAsFactors = FALSE)
model_lookup[1, ] <- c(1, "data.pd", "permno_id", "fyear", model_type[1], dep_var[1], paste(controls, dvs, year_dv, sep = " + "), NA)
model_lookup[2, ] <- c(2, "data.pd", "permno_id", "fyear", model_type[2], dep_var[1], paste(controls, year_dv, sep = " + "), NA)
# model_lookup[2,] <- c(2,'data.pd','permno_id','fyear',model_type[2],dep_var[1],controls,NA)
model_lookup[3, ] <- c(3, "data.pd", "permno_id", "fyear", model_type[1], dep_var[2], paste(controls, dvs, year_dv, sep = " + "), NA)
model_lookup[4, ] <- c(4, "data.pd", "permno_id", "fyear", model_type[2], dep_var[2], paste(controls, year_dv, sep = " + "), NA)
model_lookup[, c("formula")] <- paste(model_lookup[, c("dep_var")], model_lookup[, c("ind_var")], sep = " ~ ")


aa <- data[rowSums(is.na(data[, 1:14])) == 0, ]
bb <- sqldf("select distinct  permno_id, count(permno_id) as count\n            from             data\n            group by         permno_id\n            order by         permno_id")


rsquared_lookup <- data.frame(num = integer(), rsquared_within = numeric(), rsquared_adjusted = numeric(), rsquared_between = numeric(), rsquared_overall = numeric(), 
                              stringsAsFactors = FALSE)

## REGRESSION 1
reg1 <- plm(as.formula(model_lookup[1, c("formula")]), data = data.pd, model = model_lookup[1, c("type")], na.rm = FALSE)
reg1_rse <- cl.plm(data, reg1, data[, "permno_id"])
# reg1_rse <- mcl.plm(data, reg1, data[,'permno_id'], data[,'fyear']) reg1_rse <- coeftest(reg1, vcov=function(x) vcovHC(x,
# cluster='group', type='HC1'))
reg1_rse_df0 <- unclass(reg1_rse)
reg1_rse_df <- data.frame(var = row.names(reg1_rse_df0), estimate = reg1_rse_df0[, 1], std.error = reg1_rse_df0[, 2], t.value = reg1_rse_df0[, 
                                                                                                                                             3], pr.t = reg1_rse_df0[, 4], stringsAsFactors = FALSE)
rsquared_lookup[1, ] <- c(1, NA, NA, summary(reg1)$r.squared[[1]], summary(lm(as.formula(model_lookup[1, c("formula")]), data = data))$r.squared)



## REGRESSION 2 reg2 <- plm(as.formula(model_lookup[2,c('formula')]),data=data.pd,model=model_lookup[2,c('type')],effect='time',na.rm=FALSE)
## reg2 <- plm(as.formula(model_lookup[2,c('formula')]),data=data.pd,model=model_lookup[2,c('type')],effect='twoways',na.rm=FALSE)
## reg2_twoway <- plm(as.formula(paste(model_lookup[2,c('dep_var')],controls,sep=' ~
## ')),data=data.pd,model=model_lookup[2,c('type')],effect='individual',na.rm=FALSE) reg2_bw <-
## plm(as.formula(model_lookup[2,c('formula')]),data=data.pd,model=model_lookup[1,c('type')],na.rm=FALSE)
reg2_rse <- cl.plm(data, reg2, data[, "permno_id"])
# reg2_rse <- mcl.plm(data, reg2, data[,'permno_id'], data[,'fyear']) reg2_rse <- coeftest(reg2, vcov=function(x) vcovHC(x,
# cluster='group', type='HC1'))
reg2_rse_df0 <- unclass(reg2_rse)
reg2_rse_df <- data.frame(var = row.names(reg2_rse_df0), estimate = reg2_rse_df0[, 1], std.error = reg2_rse_df0[, 2], t.value = reg2_rse_df0[, 
                                                                                                                                             3], pr.t = reg2_rse_df0[, 4], stringsAsFactors = FALSE)
rsquared_lookup[2, ] <- c(2, summary(reg2)$r.squared[[1]], NA, NA, summary(lm(as.formula(model_lookup[2, c("formula")]), data = data))$r.squared)


# index=c(model_lookup[2,c('index_individual')],model_lookup[2,c('index_time')]),

# effect='time' effect='individual' effect='twoways'


# reg2_rse_df <- data.frame(reg2_rse[,1],reg2_rse[,2],reg2_rse[,3],reg2_rse[,4]) reg2_rse_df <- as.data.frame(as.matrix(reg2_rse))
# reg2_rse_df <- as.data.frame(do.call(rbind, reg2_rse),stringsAsFactors=FALSE)

# fixef(reg2, type = 'dmean') summary(fixef(reg2, type = 'dmean'))

# coeftest(reg2) coeftest(reg2, vcov=function(x) vcovHC(x, method='white1', type='HC1'))

# coeftest(reg2, vcov=function(x) vcovHC(x, cluster='time', type='HC1')) coeftest(reg2, vcov=function(x) vcovDC(x, type='HC1'))

# vcovHC(reg2,cluster='group') + vcovHC(reg2,cluster='time') - vcovHC(reg2,method='white1')

# a1 <- vcovHC(reg2,cluster='group') a2 <- vcovHC(reg2,cluster='time') a3 <- vcovHC(reg2,method='white1') a4 <- vcovDC(reg2, type='HC1')

# a5 <- pcse(reg2, groupN = aglUn$country,groupT = aglUn$year, pairwise = TRUE)






## REGRESSION 3
reg3 <- plm(as.formula(model_lookup[3, c("formula")]), data = data.pd, model = model_lookup[3, c("type")], na.rm = FALSE)
reg3_rse <- cl.plm(data, reg3, data[, "permno_id"])
# reg3_rse <- mcl.plm(data, reg3, data[,'permno_id'], data[,'fyear'])
reg3_rse_df0 <- unclass(reg3_rse)
reg3_rse_df <- data.frame(var = row.names(reg3_rse_df0), estimate = reg3_rse_df0[, 1], std.error = reg3_rse_df0[, 2], t.value = reg3_rse_df0[, 
                                                                                                                                             3], pr.t = reg3_rse_df0[, 4], stringsAsFactors = FALSE)
rsquared_lookup[3, ] <- c(1, NA, NA, summary(reg3)$r.squared[[1]], summary(lm(as.formula(model_lookup[3, c("formula")]), data = data))$r.squared)

## REGRESSION 4
reg4 <- plm(as.formula(model_lookup[4, c("formula")]), data = data.pd, model = model_lookup[4, c("type")], na.rm = FALSE)
reg4_rse <- cl.plm(data, reg4, data[, "permno_id"])
# reg4_rse <- mcl.plm(data, reg4, data[,'permno_id'], data[,'fyear']) reg4_rse <- coeftest(reg4, vcov=function(x) vcovHC(x,
# cluster='group', type='HC1'))
reg4_rse_df0 <- unclass(reg4_rse)
reg4_rse_df <- data.frame(var = row.names(reg4_rse_df0), estimate = reg4_rse_df0[, 1], std.error = reg4_rse_df0[, 2], t.value = reg4_rse_df0[, 
                                                                                                                                             3], pr.t = reg4_rse_df0[, 4], stringsAsFactors = FALSE)
# rsquared_lookup[4,] <- c(4,summary(reg4)$r.squared[[1]],summary(reg4)$r.squared[[2]],NA,
# summary(lm(as.formula(model_lookup[4,c('formula')]),data=data))$r.squared )
rsquared_lookup[4, ] <- c(4, summary(reg4)$r.squared[[1]], NA, NA, summary(lm(as.formula(model_lookup[4, c("formula")]), data = data))$r.squared)

## Output Table 4
reg1_extract <- extract.plm(reg1)
reg1_output <- createTexreg(coef.names = reg1_extract@coef.names, coef = reg1_extract@coef, se = reg1_rse_df[, c("std.error")], pvalues = reg1_rse_df[, 
                                                                                                                                                      c("pr.t")], gof.names = c("Between/Within R$^2$", "Overall R$^2$", "Num. obs.", "Num. groups"), gof = c(rsquared_lookup[1, c("rsquared_between")], 
                                                                                                                                                                                                                                                              rsquared_lookup[1, c("rsquared_overall")], nrow(reg1$model), reg1_extract@gof[3]), gof.decimal = as.logical(c("TRUE", "TRUE", "FALSE", 
                                                                                                                                                                                                                                                                                                                                                                            "FALSE")))

reg2_extract <- extract.plm(reg2)
reg2_output <- createTexreg(coef.names = reg2_extract@coef.names, coef = reg2_extract@coef, se = reg2_rse_df[, c("std.error")], pvalues = reg2_rse_df[, 
                                                                                                                                                      c("pr.t")], gof.names = c("Between/Within R$^2$", "Overall R$^2$", "Num. obs.", "Num. groups"), gof = c(rsquared_lookup[2, c("rsquared_within")], 
                                                                                                                                                                                                                                                              rsquared_lookup[2, c("rsquared_overall")], nrow(reg2$model), reg2_extract@gof[3]), gof.decimal = as.logical(c("TRUE", "TRUE", "FALSE", 
                                                                                                                                                                                                                                                                                                                                                                            "FALSE")))

reg3_extract <- extract.plm(reg3)
reg3_output <- createTexreg(coef.names = reg3_extract@coef.names, coef = reg3_extract@coef, se = reg3_rse_df[, c("std.error")], pvalues = reg3_rse_df[, 
                                                                                                                                                      c("pr.t")], gof.names = c("Between/Within R$^2$", "Overall R$^2$", "Num. obs.", "Num. groups"), gof = c(rsquared_lookup[3, c("rsquared_between")], 
                                                                                                                                                                                                                                                              rsquared_lookup[3, c("rsquared_overall")], nrow(reg3$model), reg3_extract@gof[3]), gof.decimal = as.logical(c("TRUE", "TRUE", "FALSE", 
                                                                                                                                                                                                                                                                                                                                                                            "FALSE")))

reg4_extract <- extract.plm(reg4)
reg4_output <- createTexreg(coef.names = reg4_extract@coef.names, coef = reg4_extract@coef, se = reg4_rse_df[, c("std.error")], pvalues = reg4_rse_df[, 
                                                                                                                                                      c("pr.t")], gof.names = c("Between/Within R$^2$", "Overall R$^2$", "Num. obs.", "Num. groups"), gof = c(rsquared_lookup[4, c("rsquared_within")], 
                                                                                                                                                                                                                                                              rsquared_lookup[4, c("rsquared_overall")], nrow(reg4$model), reg4_extract@gof[3]), gof.decimal = as.logical(c("TRUE", "TRUE", "FALSE", 
                                                                                                                                                                                                                                                                                                                                                                            "FALSE")))


screenreg(list(reg1_output, reg2_output, reg3_output, reg4_output), caption = "The effect of advertising on the breadth of ownership", digits = 3, 
          model.names = c("(1)", "(2)", "(3)", "(4)"), stars = c(0.01, 0.05, 0.1))

htmlreg(list(reg1_output, reg2_output, reg3_output, reg4_output), caption = "The effect of advertising on the breadth of ownership", digits = 3, 
        model.names = c("(1)", "(2)", "(3)", "(4)"), stars = c(0.01, 0.05, 0.1), file = paste(output_directory, "table4_regressions", ".doc", sep = ""))

rm2(reg1_rse, reg2_rse, reg3_rse, reg4_rse)
rm2(reg1_rse_df0, reg2_rse_df0, reg3_rse_df0, reg4_rse_df0) 
