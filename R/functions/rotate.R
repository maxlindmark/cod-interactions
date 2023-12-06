# Plot ordination with ggplot?
# https://github.com/JenniNiku/gllvm/discussions/116

rotate <- function(object, alpha = 0.5, type = NULL, which.lvs = 1:2) {
  p <- ncol(object$y)
  num.lv <- object$num.lv
  num.lv.c <- object$num.lv.c
  num.RR <- object$num.RR
  quadratic <- object$quadratic
  
  if ((num.lv.c + num.RR) == 0 & is.null(type)) {
    type <- "residual"
  } else if (is.null(type)) {
    if (num.lv.c == 0 & num.RR > 0) {
      type <- "marginal"
    } else if (num.lv.c > 0) {
      type <- "conditional"
    }
  }
  
  if (type == "residual" | num.lv > 0) {
    # First create an index for the columns with unconstrained LVs
    sigma.lv <- NULL
    if (num.lv.c > 0 & type == "residual") {
      # to have similar scaling to unconstrained LVs
      sigma.lv <- object$params$sigma.lv[1:num.lv.c]
    } else if (num.lv.c > 0) {
      #here the LVs get scaled with sigma.lv not theta
      sigma.lv <- c(sigma.lv, rep(1, num.lv.c))
    }
    # constrained LVs never have a scale parameter so always get 1
    sigma.lv <- c(sigma.lv, rep(1, num.RR))
    
    # unconstrained LVs always get their species parameters scaled, never LVs
    if (num.lv > 0) {
      sigma.lv <-
        c(sigma.lv, object$params$sigma.lv[(num.lv.c + 1):(num.lv.c + num.lv)])
    }
    # scaling for quadratic coefficients
    if (quadratic != FALSE) {
      sigma.lv <- c(sigma.lv, sigma.lv ^ 2)
    }
    
    # Do the scaling
    sigma.lv <- diag(sigma.lv, length(sigma.lv))
    object$params$theta <- object$params$theta %*% sigma.lv
    
    #remove unconstrained LVs species loadings if type=="marginal"
    if (type == "marginal" & num.lv > 0) {
      if (quadratic != FALSE)
        object$params$theta <- object$params$theta[, -c((num.lv.c + num.RR + 1):(num.lv.c + num.RR + num.lv) +  num.RR + num.lv.c + num.lv), drop = F]
      object$params$theta <- object$params$theta[, -c((num.lv.c + num.RR + 1):(num.lv.c + num.RR + num.lv)), drop = F]
    }
    #remove constrined LVs species loadings if type=="residual"
    if (type == "residual" & num.RR > 0) {
      if (quadratic != FALSE)
        object$params$theta <-
          object$params$theta[, -c((num.lv.c + 1):(num.lv.c + num.RR) + num.RR + num.lv.c + num.lv), drop = F]
      object$params$theta <-
        object$params$theta[, -c((num.lv.c + 1):(num.lv.c + num.RR)), drop = F]
    }
  }
  
  lv <- getLV(object, type = type)
  
  do_svd <- svd(lv)
  
  if (type != "residual") {
    do_svd$v <- svd(getLV(object, type = type))$v
  }
  
  # do_svd <- svd(lv)
  # do_svd <- svd(object$lvs)
  svd_rotmat_sites <- do_svd$v
  svd_rotmat_species <- do_svd$v
  
  choose.lvs <- lv
  
  if (quadratic == FALSE) {
    choose.lv.coefs <-  object$params$theta
  } else{
    choose.lv.coefs <- optima(object, sd.errors = F)
  }
  
  bothnorms <- vector("numeric", ncol(choose.lv.coefs))
  for (i in 1:ncol(choose.lv.coefs)) {
    bothnorms[i] <- sqrt(sum(choose.lvs[, i] ^ 2)) * sqrt(sum(choose.lv.coefs[, i] ^ 2))
  }
  
  scaled_cw_sites <-  t(t(choose.lvs) / sqrt(colSums(choose.lvs ^ 2)) * (bothnorms ^ alpha))
  # scaled_cw_species <- t(t(choose.lv.coefs) / sqrt(colSums(choose.lv.coefs^2)) * (bothnorms^(1-alpha)))
  scaled_cw_species <- choose.lv.coefs
  for (i in 1:ncol(scaled_cw_species)) {
    scaled_cw_species[, i] <-
      choose.lv.coefs[, i] / sqrt(sum(choose.lv.coefs[, i] ^ 2)) * (bothnorms[i] ^ (1 - alpha))
  }
  
  choose.lvs <- scaled_cw_sites %*% svd_rotmat_sites
  choose.lv.coefs <- scaled_cw_species %*% svd_rotmat_species
  
  LVcoef  = NULL
  # Continue for env preds
  if (num.lv == 0 & (num.lv.c + num.RR) > 0 & type != "residual" | (num.lv.c + num.RR) > 0 &num.lv > 0 & type == "marginal") {
    LVcoef <- (object$params$LvXcoef %*% svd_rotmat_sites)[, which.lvs]
    LVcoef <- LVcoef / apply(object$lv.X, 2, sd)
  }
  
  result <- list(sites = choose.lvs, species = choose.lv.coefs, env = LVcoef)
  
  return(result)
}
