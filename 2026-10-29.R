#when fitting goes wrong - how to notice a bad fit and how to fix it
#four fixes: centering, rescaling, keeping parameters legal, and good starting values

#setup
library(biometrydata)

occ <- bobwhite_occupancy
counts <- bobwhite_counts
reef <- reef_surveys

# bird traits: keep the columns we need, and drop species missing any of them
birds <- avonet[, c("Migration", "Range.Size", "Mass")]
birds <- na.omit(birds)
birds$migrant <- as.integer(birds$Migration == 3)    # 1 = long-distance migrant

#fix 1: center covariates
year_nll <- function(p, x){
  prob <- plogis(p[1] + p[2] * x)
  -sum(dbinom(occ$n_occupied, size = occ$n_routes,
              prob = prob, log = TRUE))
}

# the calendar year, as it comes in the data
fit_raw <- optim(c(0, 0), year_nll, x = occ$year,
                 method = "BFGS")
fit_raw$value
fit_raw$convergence
#fix
# centered: subtract the mean year
occ$c_year <- occ$year - mean(occ$year)

fit_cen <- optim(c(0, 0), year_nll, x = occ$c_year,
                 method = "BFGS")
fit_cen$value
fit_cen$convergence
fit_cen$par

#fix 2: rescale
#change units
mig_nll <- function(p, range, mass){
  prob <- plogis(p[1] + p[2] * range + p[3] * mass)
  -sum(dbinom(birds$migrant, size = 1, prob = prob, log = TRUE))
}

fit_raw <- optim(c(0, 0, 0), mig_nll, range = birds$Range.Size, mass = birds$Mass)
fit_raw$par; fit_raw$value

fit_sc <- optim(c(0, 0, 0), mig_nll, range = birds$Range.Size / 1e6,
                mass = birds$Mass / 1000, method = "BFGS")
fit_sc$par; fit_sc$value

#fix 3: keep parameters legal
#three ways - transform, bound, penalize
# p[1] peak richness, p[2] depth of the peak, p[3] width, p[4] negative binomial size
hump_nat <- function(p){
  mu <- p[1] * exp(-(reef$depth - p[2])^2 / (2 * p[3]^2))
  -sum(dnbinom(reef$richness, mu = mu, size = p[4], log = TRUE))
}

start <- c(25, 10, 20, 0.2)    # 25 species at 10 m, 20 m wide: a sensible guess
fit_nat <- optim(start, hump_nat, method = "BFGS")
fit_nat$value
fit_nat$par

#fix - transform
hump_log <- function(p){
  mu <- exp(p[1]) * exp(-(reef$depth - p[2])^2 / (2 * exp(p[3])^2))
  -sum(dnbinom(reef$richness, mu = mu, size = exp(p[4]), log = TRUE))
}
# the same start, on the log scale
fit_log <- optim(c(log(25), 10, log(20), log(0.2)), hump_log, method = "BFGS")
fit_log$value
# back to the natural scale: peak, depth, width, size
c(exp(fit_log$par[1]), fit_log$par[2], exp(fit_log$par[3]), exp(fit_log$par[4]))

#fix - bound
fit_bnd <- optim(start, hump_nat, method = "L-BFGS-B",
                 lower = c(peak  = 0.01,   # species at the peak: must be above 0
                           depth = 0,      # metres below the surface: not above it
                           width = 0.1,    # metres: must be above 0
                           size  = 0.01))  # clumping: must be above 0
fit_bnd$value
fit_bnd$par

#fix - penalize
hump_pen <- function(p){
  if (any(p[c(1, 3, 4)] <= 0)) {     # peak, width or size not positive
    return(1e10)                     # a terrible score: go somewhere else
  }
  hump_nat(p)
}

fit_pen_bfgs <- optim(start, hump_pen, method = "BFGS")
fit_pen_nm   <- optim(start, hump_pen)                   # Nelder-Mead
fit_pen_bfgs$value; fit_pen_nm$value

#keep one failed fit from affecting a whole loop
#example of fail causing impact
lin_nll <- function(p){
  prob <- p[1] + p[2] * occ$elapsed
  -sum(dbinom(occ$n_occupied, size = occ$n_routes, prob = prob, log = TRUE))
}

set.seed(20)
results <- rep(NA, 10)
for (i in 1:10) {                      # this loop fails on purpose
  start <- c(runif(1, 0.5, 1), runif(1, -0.03, 0.01))
  results[i] <- optim(start, lin_nll, method = "BFGS")$value
}
results
#try catch()
set.seed(20)
results <- rep(NA, 10)
for (i in 1:10) {
  start <- c(runif(1, 0.5, 1), runif(1, -0.03, 0.01))
  results[i] <- tryCatch(
    optim(start, lin_nll, method = "BFGS")$value,
    error = function(e){ NA }               # if it fails, record NA
  )
}
results

#fix 4: good starting values
#build up from the simpler model
plain_nll <- function(p){            # counts fall from the first development
  mu <- exp(p[1] + p[2] * counts$frac_built)
  -sum(dnbinom(counts$count, mu = mu, size = exp(p[3]), log = TRUE))
}
threshold_nll <- function(p){        # counts flat until a fraction p[3] is built
  mu <- exp(p[1] + p[2] * pmax(0, counts$frac_built - p[3]))
  -sum(dnbinom(counts$count, mu = mu, size = exp(p[4]), log = TRUE))
}
fit_plain <- optim(c(2, -1, 0), plain_nll)

# start exactly at the plain model: its estimates, with a threshold of 0
fit_built <- optim(c(fit_plain$par[1:2], 0, fit_plain$par[3]), threshold_nll)

fit_built$par[3]              # the threshold: fraction of the route developed
exp(fit_built$par[1])         # expected bobwhite on a route below the threshold
fit_built$value





