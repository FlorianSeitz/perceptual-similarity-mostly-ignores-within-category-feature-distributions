# ==========================================================================
# Analyses Experiment: 1. Preprocesses data
# ==========================================================================

# ==========================================================================
rm(list = ls(all = TRUE))
study <- 1 # specify the study (1 or 2)
main <- TRUE # TRUE = included subjects; FALSE = excluded subjects
# ==========================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change wd to where this script lies (RStudio needed!)
files <- list.files(path = sub("X", study, "../../data/raw/studyX/"), pattern = ".csv", full.names = TRUE)
dt <- rbindlist(lapply(files, fread), fill = TRUE)
stim <- fread(sub("X", study, "../../misc/stimuli/studyX.csv"), colClasses = list("character" = "id"))

# ==========================================================================
# Prepares randomization columns and deletes unnecessary rows
# ==========================================================================
dt[, f1_is := substring(unique(randomization.feature)[1], 5), by = participant]
dt[, e_is := as.integer(substring(unique(randomization.key)[1], 4)), by = participant]
dt <- dt[phase %in% c("train", "test")]

# ==========================================================================
# Deletes subjects that did not reach accuracy criterion
# ==========================================================================
subj_to_keep <- dt[phase == "train", mean(tail(trainResp.corr, 100)) > .9, by = participant][V1 == TRUE, participant]
dt <- dt[(participant %in% subj_to_keep) == main]

# ==========================================================================
# Prepares columns, deletes unnecessary rows and columns
# ==========================================================================
ids <- dt[, .(participant = sort(unique(participant)))][, .(participant, subj = paste0(ifelse(main == TRUE, "s", "x"), study, str_sub(paste0("0", order(participant)), -2, -1)))]
dt <- merge(dt, ids)
dt[, trial := 1:.N, by = subj] # counts through rows
dt[, id := str_extract(stim, "\\d+")] # extracs stimulus id
dt[, stim := NULL]
dt[, resp := paste0(trainResp.keys, testResp.keys, fillerResp.keys)] # combines response columns
dt[, resp := ifelse(resp == "e", unique(e_is), 1 - unique(e_is)), by = subj] # letter to numbers
dt[, corr := trainResp.corr]
dt[phase == "test" & !is.na(c), corr := ifelse(resp == c, 1, 0)] # checks if response to old stimuli in test is correct
dt[, rt := rowSums(cbind(trainResp.rt, testResp.rt, fillerResp.rt), na.rm = TRUE)] # combines response time columns

dt <- merge(dt, stim[, .(id, f1, f2, stim, stim_type)], by = "id", sort = FALSE) # adds feature values
dt[, phase := ifelse(phase == "train", "learn", ifelse(substr(stim, 1, 1) == "F", "filler", "transfer"))]

dt <- dt[, list(subj, phase, trial, stim, stim_type, f1, f2, c, resp = as.integer(resp), corr, rt, f1_is = as.factor(f1_is), e_is = as.factor(e_is))]

fwrite(dt, sub("X", study, paste0("../../data/processed/", if(main == FALSE) {"excluded_"}, "studyX.csv")))
