# ==========================================================================
# Analyses Experiment: 3. Analyses the results of the cognitive model fiting
# ==========================================================================

# ==========================================================================
# Parameters that need to be specified
study <- 2 # specify the study

models <- c("gcm", "gcm_maha") # specify the models to analyze
crit <- 2/3 # specify the criterion to assign a participant to a model
# ==========================================================================

# ==========================================================================
# Prepares packages and data
# ==========================================================================
pacman::p_load(data.table, cognitiveutils, RVAideMemoire, pwr, esc, splitstackshape)
devtools::install_github("FlorianSeitz/cognitivemodels@mahalanobis")
library(cognitivemodels)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change wd to where this script lies (RStudio needed!)
source("setup_models.R")
d <- fread(sub("X", study, "../../data/processed/studyX.csv"), key = "subj")[stim_type == "transfer"]
fits <- readRDS(sub("X", study,"../other/studyX_cognitive_models.rds"))

# ==========================================================================
# Computes summary of parameter estimates
# ==========================================================================
pars <- fits[model %in% models, fit[[1]]$par, by = list(subj, model)]
pars[, tau := 1 / tau] # compute inverse of tau for manuscript
pars <- merge(unique(d[, .(subj, f1_is)]), pars, by = "subj")
pars <- melt(pars, id.vars = c("subj", "f1_is", "model"), variable.name = "par")
pars[grepl("^eval", par), par := ifelse(grepl("feature1", par), f1_is, c("size", "angle")[!c("size", "angle") %in% f1_is]), by = .(subj, model)]
pars[par %in% c("size", "angle", "lambda", "tau"), .(m = mean(value), md = median(value), sd = sd(value)), by = .(model, par)][, round(.SD, 2), by = .(model, par)][order(model)]

# ==========================================================================
# Makes model predictions
# ==========================================================================
preds <- fits[model %in% models, predict(fit[[1]], newdata = d[subj]), by = .(subj, model)]
preds[, trial := d[subj, "trial"], by = .(subj, model)]
preds <- as.data.table(tidyr::pivot_wider(preds, names_from = "model", values_from = "V1"))
preds[, random := 0.5] # prediction of random model
d <- merge(d, preds, by = c("subj", "trial"))

# ==========================================================================
# Calculates the models' goodness-of-fit (gof, i.e., log likelihood)
# ==========================================================================
models <- c(models[models != "random"], "random") # include random model in analyses
gofs <- d[, lapply(.SD, function(x) {
  gof(obs = resp, pred = x, type = "loglik", response = "disc", na.rm = TRUE)
}), .SDcols = models, by = subj]

# ==========================================================================
# Calculates evidence strengths (weights) and makes model comparisons
# ==========================================================================
weights <- gofs[, exp(.SD - max(.SD)), .SDcols = models, by = .(subj)]
weights[, (models) := .SD/ rowSums(.SD), .SDcols = models] # equal to AIC weights (due to out-of-sample predictions)

# Aggregate level comparisons
agg <- unlist(sort(weights[, lapply(.SD, mean), .SDcols = models], decreasing = T))
round(agg, 2) # evidence for the models
agg[1] / agg[2:length(agg)] # evidence ratio of best model to other models
gofs[, round(colMeans(.SD), 2), .SDcols = !"subj"] # log likelihood

# Individual level comparisons
weights[, best_model := ifelse(max(.SD) > crit, names(which.max(.SD)), "none"), by = subj] # defines best model per subject
weights[, ratio := sort(.SD, decreasing = T)[, 1] / sort(.SD, decreasing = T)[, 2], by = .(subj, best_model)] # computes evidence ratio of best to second-best model per subject
weights[, ratio := cut(ratio, c(1, 3, 10, 30, 100, Inf), labels = c("weak", "substantial", "strong", "very strong", "decisive"))] # divides evidence ratio into intervals (similar to Bayes factors)
weights[, table(best_model, ratio)] # shows number of described participants per model + evidence ratio

model_distr[best_model != "random", ES.h(N[1] / nrow(weights), N[2] / nrow(weights))] # effect size
model_distr <- weights[best_model != "none", .N, by = best_model]
model_distr[, multinomial.test(N)] # exact multinomial test
model_distr[, multinomial.multcomp(N, p.method = "holm")] # pairwise comparisons

# ==========================================================================
# Additional fit indices
# ==========================================================================
d_long <- melt(d, measure.vars = list(models), variable.name = "model", value.name = "pred")
add_gofs <- d_long[, .(mape = MAPE(obs = resp, pred = pred, response = 'disc', discount = 0),
                       arg = mean(choicerule(x = pred, type = "argmax")[, 1] == resp),
                       mse = mean((resp - pred)^2, na.rm = T),
                       ll = gof(obs = resp, pred = pred, type = "loglik", response = 'disc', na.rm = TRUE)), by = .(model, subj)]
add_gofs <- melt(add_gofs, measure.vars = c("mape", "arg", "mse", "ll"), variable.name = "gof")
add_gofs[, .(m = mean(value), md = median(value), sd = sd(value)), by = .(model, gof)][, round(.SD, 2), by = .(model, gof)][order(model)]

# ==========================================================================
# Tests if more people are fit by a model than expected by chance
# ==========================================================================
computes_evidence_strength <- function(x, models, true_model = c("gcm", "gcm_maha"), i) {
  set.seed(i)
  if(true_model == "gcm") {
    x[, resp_sampled := rbinom(n = nrow(.SD), size = 1, prob = gcm)]
  } else {
    x[, resp_sampled := rbinom(n = nrow(.SD), size = 1, prob = gcm_maha)]
  }
  x <- x[, lapply(.SD, function(p) {
    gof(obs = resp_sampled, pred = p, type = "loglik", response = "disc", na.rm = TRUE)
  }), .SDcols = models, by = subj]

  x <- x[, exp(.SD - max(.SD)), .SDcols = models, by = .(subj)]
  x[, (models) := .SD/ rowSums(.SD), .SDcols = models] # equal to AIC weights (due to out-of-sample predictions)
  
  x[, best_model := ifelse(max(.SD) > crit, names(which.max(.SD)), "other"), by = subj]
  x[best_model != "other", ratio := sort(.SD, decreasing = T)[, 1] / sort(.SD, decreasing = T)[, 2], by = .(subj, best_model)]
  x[, ratio := cut(ratio, c(1, 3, 10, 30, 100, Inf), labels = c("weak", "substantial", "strong", "very strong", "decisive"))][]
  return(x)
}

res <- rbindlist(lapply(1:1000, function(i) {
  data.table(gcm = computes_evidence_strength(d, models = models, true_model = "gcm", i = i)[, sum(best_model == "gcm_maha")],
             gcm_maha = computes_evidence_strength(d, models = models, true_model = "gcm_maha", i = i)[, sum(best_model == "gcm")])
}))

melt(res, measure.vars = c("gcm", "gcm_maha"))[, .(mean(value), median(value), sd(value)), by = variable][, round(.SD, 2), by = variable]
binom.test(weights[, sum(best_model == "gcm_maha")], nrow(weights), res[, mean(gcm)/nrow(weights)])
binom.test(weights[, sum(best_model == "gcm")], nrow(weights), res[, mean(gcm_maha)/nrow(weights)])

# ==========================================================================
# Makes individual analyses grouped by the subjects described by each model
# ==========================================================================
d <- merge(d, weights[, .(subj, best_model)])
if(study == 2) { # makes the predictions of all transfer stimuli go into the same direction
  d[!stim %in% paste0("T", 1:4), c("resp", "gcm", "gcm_maha") := .(1 - resp, 1 - gcm, 1 - gcm_maha)]
}

# Mean responses grouped by the subjects best described by each model
d[, mean(resp), by = .(subj, best_model)][, .(m = mean(V1), md = median(V1), sd = sd(V1)), by = .(best_model)][, round(.SD, 2), by = .(best_model)]
d[best_model == "gcm", prop.test(sum(resp), nrow(.SD), p = .5, alternative = "greater")]
d[best_model == "gcm_maha", prop.test(sum(resp), nrow(.SD), p = .5, alternative = "less")]

# Correspondence predictions and responses grouped by the subjects best described by each model
d[best_model == "gcm", round(mean(resp == round(gcm)), 2)]
d[best_model == "gcm", prop.test(sum(resp == round(gcm)), nrow(.SD), .5, alternative = "greater")]
d[best_model == "gcm_maha", round(mean(resp == round(gcm_maha)), 2)]
d[best_model == "gcm_maha", prop.test(sum(resp == round(gcm_maha)), nrow(.SD), .5, alternative = "greater")]
