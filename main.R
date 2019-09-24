############################
## Imports
############################
imports <- c('tidyverse','rio','bit64')
invisible(lapply(imports, require, character.only = TRUE))
source("Helpers.R", chdir = TRUE)
############################
### Read in Data
### Use - readr package to avoid errors
############################
args <- commandArgs(trailingOnly = TRUE)
DailyReport <- readr::read_csv(args[1])
DailyReport <- DailyReport[!is.na(DailyReport$Date),]
############################
## Testing
############################
T1 <- filterDate(DailyReport, method = 'lag', n_lag = 6)
T2 <- filterDate(DailyReport, method = 'lag', n_lag = 0)
test <- doTest(T1,T2)
############################
## Export
############################
date_time <- toString(sprintf('%s - %s', args[1], toString(Sys.Date())))
dir.create(date_time)
export(test$io, file = sprintf("%s/io.csv", date_time))
export(test$li, file = sprintf("%s/li.csv", date_time))
export(test$pn, file = sprintf("%s/p_naive.csv", date_time))
export(test$ps, file = sprintf("%s/p_sophisticated.csv", date_time))
