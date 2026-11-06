#likelihood examples
#the workflow:
#1. Ask the question - a sentence and a prediction, before the data
#2. Look at the data - table(), tapply(), plots: sizes, spread, zeros
#3. Choose the distribution - AIC across distributions (Lecture 18)
#4. Choose the deterministic part - parameters =, centering, rescaling (Lecture 20)
#5. Check the fit worked - convergence, a second start (Lecture 19)
#6. Compare models - AICtab(), anova() (Lecture 18)
#7. Check against the data - simulate from the fit (Lecture 19)
#8. Intervals, in biology terms - the recipe, confint(), a band (Lecture 21); ggplot()

#sponge example - substrate and depth
library(bbmle)
library(emdbook) #for the zero-inflated negative binomial
library(biometrydata)
library(ggplot2) #for the figure

#step 1: rename column names
rov <- data.frame(dive = tosca_rov$Dive.no,
                  depth = tosca_rov$Depth.water..m.,
                  substrate = tosca_rov$Substrate,
                  fov = tosca_rov$FOV.camera..m.,  #camera width, m
                  sponges = tosca_rov$Porifera....,
                  corals = tosca_rov$Cnidaria....) #all cnidarians, mostly corals

rov <- rov[!is.na(rov$fov), ] #sections with no camera width
rov$area <- 50 * rov$fov #seafloor seen: 50 m long, fov wide

#step 2: visualize the data
table(rov$substrate)
mean(rov$sponges == 0) #fraction with none
c(mean = mean(rov$sponges), variance = var(rov$sponges))
rov$density <- 100 * rov$sponges / rov$area #per 100 m2
tapply(rov$density, rov$substrate, median)

#build model
#step 3: distribution
#one density everywhere, three ways the counts could scatter around it. Test Poisson, negative binomial, zero inflated binomial
m_pois <- mle2(sponges ~ dpois(lambda = exp(log_d) * area / 100),
               start = list(log_d = 0), data = rov)
m_nb   <- mle2(sponges ~ dnbinom(mu = exp(log_d) * area / 100, size = exp(log_k)),
               start = list(log_d = 0, log_k = 0), data = rov)
m_zinb <- mle2(sponges ~ dzinbinom(mu = exp(log_d) * area / 100, size = exp(log_k),
                                   zprob = plogis(logit_z)),
               start = list(log_d = 0, log_k = 0, logit_z = -2), data = rov)
AICtab(m_pois, m_nb, m_zinb) #negative binomial beats the Poisson, zero inflated adds nothing as it's a bit worse

#step 4: fit the parameters
rov$depth_km <- (rov$depth - 1500) / 1000

m_sub <- mle2(sponges ~ dnbinom(mu = exp(log_d) * area / 100, size = exp(log_k)),
              start = list(log_d = 0, log_k = 0),
              parameters = list(log_d ~ substrate - 1), data = rov)

m_depth <- mle2(sponges ~ dnbinom(mu = exp(log_d + b * depth_km) * area / 100, 
                                  size = exp(log_k)),
                start = list(log_d = 0, b = 0, log_k = 0), data = rov)

m_both <- mle2(sponges ~ dnbinom(mu = exp(log_d + b * depth_km) * area / 100,
                                 size = exp(log_k)),
              start = list(log_d = 0, b = 0, log_k = 0),
              parameters = list(log_d ~ substrate - 1), data = rov)

#step 5: determining if the fit worked
m_both@details$convergence #0 is good
#a second start, far from the first: does it find the same bottom?
m_both_2 <- update(m_both, start = list(log_d = 3, b = -1, log_k = 1))
c(m_both@min, m_both_2@min) #same answer which is good

#step 6: compare models
AICtab(m_nb, m_sub, m_depth, m_both, weights = TRUE)
exp(coef(m_both))
#models with both depth and substrate is best
#biogenic gravels have high densities of sponges while gravel and sand have the lowest

#step 7: compare model with observed data
set.seed(7)
mu_hat <- predict(m_both) #the fitted mean, every section
k_hat <- exp(coef(m_both)["log_k"])

fake_zeros <- replicate(1000, {
  fake <- rnbinom(nrow(rov), mu = mu_hat, size = k_hat)
  tapply(fake == 0, rov$substrate, mean)
})

cbind(observed = tapply(rov$sponges. == 0, rov$substate, mean),
      lower = apply(fake_zeros, 1, quantile, 0.025),
      upper = apply(fake_zeros, 1, quantile, 0.975))
#mismatch: 20% sand observed by 9%-20% simulated
#let sediment be empty
rov$sediment <- as.numeric(rov$substrate %in% c("sand", "gravel", "boulders"))
#the on/off switch: sediment is 0 or 1, so sediment * plogis(logit_z) is 0
#zero inflation (off) on rock, and plogis(logit_z) (on) on sediment
#start near m_both's estimates
m_zero <- mle2(sponges ~ dzinbinom(mu = exp(log_d + b * depth_km) * area / 100,
                                   size = exp(log_k), zprob = sediment * plogis(logit_z)),
               start = list(log_d = 3, b = 0.5, log_k = -0.7, logit_z = -2),
               parameters = list(log_d ~ substrate - 1), data = rov)
#compare previous best model(m_both) with this model
AICtab(m_both, m_zero)
plogis(coef(m_zero)["logit_z"])
#m_zero is better than m_both

#step 8: intervals (use recipe)
set.seed(22)
draws <- MASS::mvrnorm(1000, mu = coef(m_zero), Sigma = vcov(m_zero))
colnames(draws)
#Q1: how many times denser on bedrock than on sand? (where sponges can settle)
quantile(exp(draws[, "log_d.substratebedrock"] - draws[, "log_d.substratesand"]),
         c(0.025, 0.975))
#Q2: how many times denser for every km deeper?
quantile(exp(draws[, "b"]), c(0.025, 0.975))
#Q3: the fraction of sediment sections that are truly empty
quantile(plogis(draws[, "logit_z"]), c(0.025, 0.975))
#results: Q1 - 4.4 to 7.6, Q2 - 1.22 to 1.90, Q3 - 9% to 18%

#draw a band for the figure
grid <- expand.grid(depth = seq(900, 2900, by = 50), substrate = c("bedrock", "sand"))
grid$depth_km <- (grid$depth - 1500) / 1000
col_d <- paste0("log_d.substrate", grid$substrate) 
#one row per draw, one column per grid point
curves <- matrix(NA, nrow = 1000, ncol = nrow(grid))
for (i in 1:1000) {
  curves[i, ] <- exp(draws[i, col_d] + draws[i, "b"] * grid$depth_km)
}
grid$lower <- apply(curves, 2, quantile, 0.025)        # the middle 95%
grid$upper <- apply(curves, 2, quantile, 0.975)
grid$density <- exp(coef(m_zero)[col_d] + coef(m_zero)["b"] * grid$depth_km)

#draw the line
ggplot(grid, aes(x = depth, y = density, color = substrate, fill = substrate)) +
  geom_point(data = rov[rov$substrate %in% c("bedrock", "sand"), ], alpha = 0.3) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = NA) +
  geom_line(linewidth = 1) +
  scale_y_continuous(trans = "log1p", breaks = c(0, 1, 10, 100, 1000)) +
  labs(x = "depth (m)", y = "sponges per 100 m²")

#coral example-----------------------------------------------------------------------------
c_nb <- mle2(corals ~ dnbinom(mu = exp(log_d) * area / 100, size = exp(log_k)),
             start = list(log_d = 0, log_k = 0), data = rov)

c_sub <- mle2(corals ~ dnbinom(mu = exp(log_d) * area / 100, size = exp(log_k)),
              start = list(log_d = 0, log_k = 0),
              parameters = list(log_d ~ substrate - 1), data = rov)

c_depth <- mle2(corals ~ dnbinom(mu = exp(log_d + b * depth_km) * area / 100, 
                                  size = exp(log_k)),
                start = list(log_d = 0, b = 0, log_k = 0), data = rov)

c_both <- mle2(corals ~ dnbinom(mu = exp(log_d + b * depth_km) * area / 100,
                                 size = exp(log_k)),
               start = list(log_d = 0, b = 0, log_k = 0),
               parameters = list(log_d ~ substrate - 1), data = rov)
AICtab(c_nb, c_sub, c_depth, c_both, weights = TRUE)

#the model that includes both substrate and depth is the best, again
set.seed(23)
coral_draws <- MASS::mvrnorm(1000, mu = coef(c_both), Sigma = vcov(c_both))
colnames(draws)
#how many times denser for every km deeper?
quantile(exp(coral_draws[, "b"]), c(0.025, 0.975))
#again, more dense on hard substrate BUT there are fewer corals at greater depths as opposed to sponges becoming denser with depth  
  