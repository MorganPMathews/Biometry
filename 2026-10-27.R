library(bbmle)
library(biometrydata)

occ <- bobwhite_occupancy

#the occupancy model, with both parameters in one vector, as optim() wants
occ_nll <- function(p){
  prob <- plogis(p[1] + p[2] * occ$elapsed)
  -sum(dbinom(occ$n_occupied, size = occ$n_routes, prob = prob, log = TRUE))
}

occ_nll(c(2, -0.05)) #one guess: how good is it?

#brute force
a_vec <- seq(1, 3, by = 0.01)
b_vec <- seq(-0.15, 0.05, by = 0.001)
grid <- expand.grid(a = a_vec, b = b_vec) #every combination
nrow(grid)

grid_time <- system.time({
  grid$nll <- NA
  for(i in 1:nrow(grid)){
    grid$nll[i] <- occ_nll(c(grid$a[i], grid$b[i]))
  }
})
grid_time

grid[which.min(grid$nll),] #the best grid point

#two methods
fit_nm <- optim(c(0,0), occ_nll) #Nelder-Mead, the default
fit_bfgs <- optim(c(0,0), occ_nll, method = "BFGS")

fit_nm$par; fit_nm$value; fit_nm$counts
fit_bfgs$par; fit_bfgs$value; fit_bfgs$counts

#simulated annealing - using two different seeds
set.seed(1)
fit_sa1 <- optim(c(0,0), occ_nll, method = "SANN")

set.seed(2026)
fit_sa2 <- optim(c(0,0), occ_nll, method = "SANN")

fit_sa1$par; fit_sa1$value
fit_sa2$par; fit_sa2$value

#common practice: anneal first, then polish with BFGS, starting from where the annealing stopped
#"L-BFGS-B" is same as BFGS but with bounds. optimize() is one parameter only

#note that a fit can fail without an error - no error but convergence is 1, not 0
fit_cg <- optim(c(0,0), occ_nll, method = "CG") #another method optim() offers

fit_cg$par
fit_cg$value
fit_cg$convergence

#different starting values
reef <- reef_surveys

# p[1] log of the peak richness, p[2] depth of the peak (m),
# p[3] log of the width (m), p[4] log of the negative binomial size
reef_nll <- function(p){
  mu <- exp(p[1]) * exp(-(reef$depth - p[2])^2 / (2 * exp(p[3])^2))
  -sum(dnbinom(reef$richness, mu = mu, size = exp(p[4]), log = TRUE))
}

set.seed(19)
results <- NULL
for (i in 1:10) {
  start <- c(log(runif(1, 5, 50)), runif(1, 0, 30), log(runif(1, 1, 50)), log(runif(1, 1, 20)))
  fit <- optim(start, reef_nll)
  results <- rbind(results, c(peak = exp(fit$par[1]), depth = fit$par[2],
                              width = exp(fit$par[3]), nll = fit$value,
                              convergence = fit$convergence))
}
round(results, 1)

#richness averaged in 2m depth bins: 8,560 surveys are too many to read
bins <- cut(reef$depth, breaks = seq(0, 32, by = 2))
plot(tapply(reef$depth, bins, mean), tapply(reef$richness, bins, mean),
     pch = 16, xlab = "depth (m)", ylab = "mean species richness")

hump <- function(d, row){
  results[row, "peak"] * exp(-(d - results[row, "depth"])^2 / (2*results[row, "width"]^2))
}
best <- which.min(results[, "nll"])
worst <- which.max(results[, "nll"])
curve(hump(x, best), add = TRUE, col = "red", lwd = 3)
curve(hump(x, worst), add = TRUE, col = "grey50", lwd = 3, lty = 2)

#simulate from the fit
fit_occ <- optim(c(2, -0.05), occ_nll, method = "BFGS")
p_fit <- plogis(fit_occ$par[1] + fit_occ$par[2] * occ$elapsed)
#1,000 imaginary sets of surveys, from the fitted model
fake <- replicate(1000, rbinom(nrow(occ), size = occ$n_routes, prob = p_fit) / occ$n_routes)
lower <- apply(fake, 1, quantile, 0.025) #the middle 95% of the fakes
upper <- apply(fake, 1, quantile, 0.975) #year by year

plot(occ$year, occ$n_occupied / occ$n_routes, pch = 16, ylim = c(0.5, 1),
     xlab = "year", ylab = "fraction of routes occupied")
lines(occ$year, lower, lty = 2)
lines(occ$year, upper, lty = 2)

#checking fit:
#convergence = 0
#check that several starts agree
#check that the fitted curve goes through the data
#check that the data simulated from the fit look like the real data
#check that the bigger model never fits worse than a smaller one nested in it

#does development hurt bobwhite only past a threshold
counts <- bobwhite_counts
#p[1] log mu0, p[2] slope b, p[3] threshold t (fraction developed), p[4] log k
threshold_nll <- function(p){
  mu <- exp(p[1] + p[2] * pmax(0, counts$frac_built - p[3]))
  -sum(dnbinom(counts$count, mu = mu, size = exp(p[4]), log = TRUE))
}

start <- c(2, -1, 0.3, 0)
first_try <- optim(start, threshold_nll)
first_try$value

#alternatives
threshold_nll <- function(p){
  mu <- exp(p[1] + p[2] * pmax(0, counts$frac_built - p[3]))
  -sum(dnbinom(counts$count, mu = mu, size = exp(p[4]), log = TRUE))
}

start <- c(2, -1, 0.3, 0)
first_try <- optim(start, threshold_nll, method = "L-BFGS-B")
first_try$value










