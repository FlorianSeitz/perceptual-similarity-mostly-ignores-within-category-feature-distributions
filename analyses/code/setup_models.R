# ==========================================================================
# Setup: Cognitive models
# Author: Florian I. Seitz
# ==========================================================================


## Set up the models -------------------------------------------------------
# GCM: standard generalized context model (with Minkowski similarity and softmax choicerule)
# GCM_maha: generalized context model with Mahalanobis similarity and softmax choicerule

# GCM with fixed bias (b0 = b1), metric exponent (r), and similarity exponent (q); first block discounted (dc)
GCM <- function(dt, discount, sim = "minkowski", feature1 = "f1", feature2 = "f2", fix = list(r = 2, q = "r", b0 = .5), cr = "softmax") {
  gcm(formula = resp ~ eval(as.name(feature1)) + eval(as.name(feature2)), 
      class = ~ c, 
      data = dt, 
      discount = discount, 
      similarity = sim, 
      fix = fix, 
      choicerule = cr
  )
}

# GCM with Mahalanobis similarity
GCM_maha <- function(dt, discount) {
  GCM(dt = dt, sim = "mahalanobis", discount = discount)
}