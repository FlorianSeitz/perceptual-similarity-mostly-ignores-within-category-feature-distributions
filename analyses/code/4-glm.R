# ==========================================================================
# Analyses Experiment: 4. Generalized linear model (glm) with logit link
# ==========================================================================

# ==========================================================================
# Parameters that need to be specified
study <- 1 # specify the study
# ==========================================================================

pacman::p_load(data.table, lme4, emmeans)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio

# ==========================================================================
# Prepares transfer data
# ==========================================================================
d <- fread(sub("X", study, "../../data/processed/studyX.csv"))[stim_type == "transfer"]
d[, ":=" (subj = as.factor(subj), stim = as.factor(stim))] 
contrasts(d$stim) <- contr.sum

# ==========================================================================
# Computes glmer with randomization conditions (m1) and without (m2)
# ==========================================================================
m1 <- glmer(resp ~ stim + f1_is + e_is + (1|subj), data = d, family = binomial) 
m2 <- glmer(resp ~ stim + (1|subj), data = d, family = binomial) 
anova(m1, m2)
round(summary(m2)$coefficients, 2)

betas_transfer <- emmeans(m2, ~stim)
pairs(betas_transfer, adjust = "holm")
res_transfer <- as.data.table(betas_transfer)[, .(stim, emmean, pred = round(exp(emmean)/(1+exp(emmean)), 2))] # calculates mean predictions
(merge(res_transfer, d[, .(resp = mean(resp)), by = stim])) # calculates mean responses
