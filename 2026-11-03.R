#confidence limits on what the fitted model predicts
#draw, compute, take the middle 95%

library(bbmle)
library(biometrydata)

occ <- bobwhite_occupancy
counts <- bobwhite_counts
#occupancy against years since 1997, as in Lecture 17
m_occ <- mle2(n_occupied ~ dbinom(size = n_routes, prob = plogis(a + b * elapsed)),
              start = list(a = 2, b = -0.05), data = occ)
coef(m_occ)
confint(m_occ)

#where plausible values come from
vcov(m_occ)                 #the variance-covariance matrix
sqrt(diag(vcov(m_occ)))     #standard errors of a and b
cov2cor(vcov(m_occ))        #how strongly a and b move together

#step 1: draw
set.seed(21)
draws <- MASS::mvrnorm(1000, mu = coef(m_occ), Sigma = vcov(m_occ))
head(draws)
cor(draws)
#step 2 and 3: compute and summarize
#occupancy in 2019 (22 years after 1997), once for each draw
occ19 <- plogis(draws[, "a"] + 22 * draws[, "b"])

hist(occ19)
quantile(occ19, c(0.025, 0.975))     #the middle 95%

#adding a band around a curve
#step 1
years <- occ$elapsed                 #the surveyed years, 0 to 22
#one row per draw, one column per year
curves <- matrix(NA, nrow = 1000, ncol = length(years))
for (i in 1:1000) {
  curves[i, ] <- plogis(draws[i, "a"] + draws[i, "b"] * years)
}
#the middle 95% of each column (the 2 means columns)
lower <- apply(curves, 2, quantile, 0.025)
upper <- apply(curves, 2, quantile, 0.975)
#step 2
obs <- occ$n_occupied / occ$n_routes
plot(occ$year, obs, pch = 16, ylim = c(0.5, 1),
     xlab = "year", ylab = "fraction of routes occupied")

lines(occ$year, plogis(coef(m_occ)["a"] + coef(m_occ)["b"] * years), lwd = 2)
lines(occ$year, lower, lty = 2)
lines(occ$year, upper, lty = 2)

#prediction = the parameters plus the scatter of real data
#prediction interval
preds <- matrix(NA, nrow = 1000, ncol = length(years))
for (i in 1:1000) {
  p_i <- plogis(draws[i, "a"] + draws[i, "b"] * years)
  # one simulated survey per year, with that year's number of routes
  preds[i, ] <- rbinom(length(years), size = occ$n_routes, prob = p_i) / occ$n_routes
}
pred_lower <- apply(preds, 2, quantile, 0.025)
pred_upper <- apply(preds, 2, quantile, 0.975)

lines(occ$year, pred_lower, lty = 3, col = "red")
lines(occ$year, pred_upper, lty = 3, col = "red")
sum(obs >= pred_lower & obs <= pred_upper)

#bobwhite and development
#bobwhite counted on a route, against the fraction of the route developed
m_nb <- mle2(count ~ dnbinom(mu = exp(log_mu0 + b * frac_built), size = exp(log_k)),
             start = list(log_mu0 = 2.5, b = -3, log_k = 0), data = counts)
coef(m_nb)

set.seed(22)
draws_nb <- MASS::mvrnorm(1000, mu = coef(m_nb), Sigma = vcov(m_nb))
#transform the ends
log(2) / -coef(m_nb)["b"]                  #the halving point
log(2) / -confint(m_nb)["b", ]        #its interval: transform the ends
#the band for development
built <- seq(0, 0.5, by = 0.01)     # 0% to 50% developed

counts_curves <- matrix(NA, nrow = 1000, ncol = length(built))
for (i in 1:1000) {
#the expected count at every value of built, for draw i, goes here
}

#the lower and upper ends of the band go here
plot(counts$frac_built, counts$count, col = "grey", ylim = c(0, 40),
     xlab = "fraction developed", ylab = "bobwhite counted")
# the fitted curve and the two ends of the band go here






