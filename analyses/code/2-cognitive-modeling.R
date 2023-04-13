# ==========================================================================
# Analyses Experiment: 2. Fits cognitive models
# ==========================================================================

# ==========================================================================
rm(list = ls(all = TRUE))
study <- 1 # specify the study
# ==========================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, modelr, purrr, doRNG)
devtools::install_github("FlorianSeitz/cognitivemodels@mahalanobis")
library(cognitivemodels)
parallel <- TRUE # fit on a parallel machine (Unix) or single core
if (parallel == TRUE) pacman::p_load(doFuture)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
d <- fread(sub("X", study, "../../data/processed/studyX.csv"))[phase == "learn"]
discount <- ifelse(study == 1, 32, 20) # discount first block (= 32 trials in Study 1, 20 trials in Study 2)

# ==========================================================================
# Specifies to-be-fitted models
# ==========================================================================
source("setup_models.R")
model_list <- list(
  gcm = GCM,                    # gcm (Minkowski similarity and softmax choicerule)
  gcm_maha = GCM_maha           # gcm (Mahalanobis similarity and softmax choicerule)
)

# ==========================================================================
# Fits models
# ==========================================================================
if (parallel == TRUE) {
  registerDoFuture()
  plan(multisession, workers = 4L)  ## on MS Windows, specify according to your number of cores
  setkey(d, "subj")  
  fits <- foreach(x = unique(d$subj),
                  .combine = "rbind",
                  .inorder = FALSE, 
                  .packages = c("data.table", "cognitivemodels", "modelr", "pacman", "doRNG"),
                  .export = c("model_list", "GCM", "GCM_maha", "discount")) %dorng% {
                    d[.(x), .(
                      model = names(model_list),
                      fit = map(model_list, exec, dt = .SD, discount = discount)), by = subj]
                  }   
} else {
  fits <- d[, .(
    model = names(model_list),
    fit = map(model_list, exec, dt = .SD, discount = discount)),
    by = subj]
}

saveRDS(fits, sub("X", study, "../other/studyX_cognitive_models.rds"))
