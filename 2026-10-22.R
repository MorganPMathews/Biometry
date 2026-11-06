install.packages("remotes")

options(timeout = 600)
remotes::install_github("bmaitner/biometry_course",
                        subdir = "biometrydata",
                        upgrade = "never")
library(biometrydata)
?bobwhite_occupancy

library(bbmle)
library(biometrydata)x
occ <- bobwhite_occupancy
counts <- bobwhite_counts

#occupancy model from last class: the share of routes with bobwhite, falling with year
occ_nll2 <- function(params, n_occupied, n_routes, elapsed){
  p <- plogis(params[1] + params[2] * elapsed)
  -sum(dbinom(n_occupied, size = n_routes, prob = p, log = TRUE))
}
parnames(occ_nll2) <- c("a","b")
occ_data <- list(n_occupied = occ$n_occupied, n_routes = occ$n_routes,
                 elapsed = occ$elapsed)
m_occ <- mle2(minuslogl = occ_nll2, start = c(a = 2, b = -0.05), data = occ_data)
m_occ@min

#nonsense predictor
junk <- runif(nrow(occ))

occ_junk <- function(params, n_occupied, n_routes, elapsed, junk){
  p <- plogis(params[1] + params[2] * elapsed + params[3] * junk)
  -sum(dbinom(n_occupied, size = n_routes, prob = p, log = TRUE))
}
parnames(occ_junk) <- c("a", "b", "c")

m_junk <- mle2(minuslogl = occ_junk,
               start = c(a = 2, b = -0.05, c = 0),
               data = c(occ_data, list(junk = junk)))

m_occ@min - m_junk@min #improves the fit

#likelihood ratio test (LRT)
const_nll <- function(params, n_occupied, n_routes){
  p <- plogis(params[1])
  -sum(dbinom(n_occupied, size = n_routes, prob = p, log = TRUE))
}
parnames(const_nll) <- "a"
m_const <- mle2(minuslogl = const_nll, start = c(a = 1),
                data = list(n_occupied = occ$n_occupied, n_routes = occ$n_routes))

deviance_occ <- 2 * (m_const@min - m_occ@min)
deviance_occ
pchisq(deviance_occ, df = 1, lower.tail = FALSE)

anova(m_const, m_occ)

#is the p-value above or below 0.05? which model to keep?
anova(m_occ, m_junk) #adding nonsense doesn't make it significant, keep simpler model

#AIC - when models are not nested (can't use LRT), used for model comparison and selection, badness of fit metric
#write the model as a formula - read as count is negative binomial, with this mean and this size
mle2(count ~ dnbinom(
  mu = mu0 * exp(b * frac_built),
  size = k),
  start = list(mu0 = 12, b = -3, 
               k = 1.4),
  data = counts)

#three models for the counts. m_flat is nested in m_year and in m_urban
m_flat <- mle2(count ~ dnbinom(mu = mu0, size = k),
               start = list(mu0 = 10, k = 1.4), data = counts)

m_year <- mle2(count ~ dnbinom(mu = mu0 * exp(r * elapsed), size = k),
               start = list(mu0 = 15, r = -0.03, k = 1.4), data = counts)

m_urban <- mle2(count ~ dnbinom(mu = mu0 * exp(b * frac_built, size = k),
               start = list(mu0 = 12, b = -3, k = 1.4), data = counts))

AIC(m_flat)
2 * m_flat@min + 2 * 2

#year and development
m_both <- mle2(count ~ dnbinom(mu = mu0 * exp(b * frac_built + r * elapsed),
                               size = k),
               start = list(mu0 = 15, b = -3, r = -0.03, k = 1.4),
               data = counts)

#compare models - AIC table can be good to include in publications to show why you chose a certain model
AICtab(m_flat, m_year, m_urban, m_both, base = TRUE)
#best model has dAIC 0 and lowest AIC value - note that AIC ranks only the models you give it and only on fit, so you have to determine if it makes biological sense

birds <- bbs_two_species
head(birds)
table(birds$species)
#make cardinal the first level, so it is the baseline in the models
birds$species <- factor(birds$species, levels = c("cardinal", "bobwhite"))

#letting a parameter differ
form <- count ~ dnbinom(mu = exp(log_mu0 + b * frac_built), size = exp(log_k))
start <- list(log_mu0 = 2, b = 0, log_k = 0)
m_same <- mle2(form, start = start, data = birds) #one model for both species
m_mu <- mle2(form, start = start, data = birds,
             parameters = list(log_mu0 ~ species)) #a separate mu0 for each
coef(m_mu)

#other ways to differ
m_b <- mle2(form, start = start, data = birds,
            parameters = list(b ~ species))
m_mub <- mle2(form, start = start, data = birds,
            parameters = list(log_mu0 ~ species, b ~ species))
m_muk <- mle2(form, start = start, data = birds,
              parameters = list(log_mu0 ~ species, log_k ~ species))
m_all <- mle2(form, start = start, data = birds,
              parameters = list(b ~ species, log_mu0 ~ species, log_k ~ species))
AICtab(m_same, m_mu, m_b, m_mub, m_muk, m_all, base = TRUE)

#keep in mind: you can use AIC to compare different types of discrete distributions or different types of continuous
  
               






