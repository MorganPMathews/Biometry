#how do parameters vary with covariates

bbs <- read.csv("https://github.com/bmaitner/biometry_course/raw/refs/heads/main/data/BBS/FL_BBS.csv")

#how many routes were surveyed each year
n_routes_by_year <- tapply(bbs$Route, bbs$Year, function(x){length(unique(x))})

#how many of them had bobwhite
bob_all <- bbs[bbs$Scientific_Name == "Colinus virginianus", ]
n_occ_by_year <- tapply(bob_all$Route, bob_all$Year, function(x){length(unique(x))})

occ <- data.frame(year = as.numeric(names(n_routes_by_year)),
                  n_routes = as.vector(n_routes_by_year),
                  n_occupied = as.vector(n_occ_by_year))

occ <- occ[occ$n_routes > 10, ] #1996 had a single route: drop it
occ$elapsed <- occ$year - min(occ$year) #0, 1, 2,...22

head(occ)

#nll
const_null <- function(p){
  -sum(dbinom(occ$n_occupied, size = occ$n_routes, prob = p, log = TRUE))
}
const_fit <- optimize(const_null, interval = c(0,1))
c(p = const_fit$minimum, nll = const_fit$objective)

lin_nll <- function(a,b){
  p <- a + b * occ$elapsed
  -sum(dbinom(occ$n_occupied, size = occ$n_routes, prob = p, log = TRUE))
}
#deliberately bad starting guess
optim(par = c(a = 0.5, b = 0.05),
      fn = function(p){
        lin_nll(p["a"], p["b"])
        },
        method = "BFGS")

#plogis
plogis(c(-3, -1, 0, 1, 3))
qlogis(c(0.1, 0.5, 0.9))
plogis(500)

#fit the decline
occ_nll <- function(a, b){
  p <- plogis(a + b * occ$elapsed)
  -sum(dbinom(occ$n_occupied, size = occ$n_routes, prob = p, log = TRUE))
}
fit_occ <- optim(par = c(a = 2, b = -0.05),
                 fn = function(p){
                 occ_nll(p["a"], p["b"])
                 },
                 method = "BFGS")
fit_occ$par
fit_occ$value

#plot
#straight line
plot(occ$elapsed, occ$n_occupied / occ$n_routes, pch = 16,
     ylim = c(0,1), xlab = "years since 1997", ylab = "proportion occupied")
elapsed_seq <- 0:22
lines(elapsed_seq,
      plogis(fit_occ$par["a"] + fit_occ$par["b"] * elapsed_seq),
      col = "blue", lwd = 3)

#mle
library(bbmle)

occ_nll2 <- function(params, n_occupied, n_routes, elapsed){
  p <- plogis(params[1] + params[2] * elapsed)
  -sum(dbinom(n_occupied, size = n_routes, prob = p, log = TRUE))
}

parnames(occ_nll2) <- c("a", "b") #mle insists on names

m_occ <- mle2(minuslogl = occ_nll2,
              start = c(a = 2, b = -0.05),
              data = list(n_occupied = occ$n_occupied,
                          n_routes = occ$n_routes,
                          elapsed = occ$elapsed))
coef(m_occ)

#bobwhites
y_bob <- bob_all$total_sightings
u_bob <- bob_all$frac_built

length(y_bob)

plot(u_bob, y_bob, pch = 16, col = rgb(0, 0, 0, 0.25),
     xlab = "fraction developed", ylab = "bobwhite counted")

#choosing the distribution
mean(y_bob)
var(y_bob)
var(y_bob)/mean(y_bob)

#fit the habitat effect
urban_nll <- function(m0, b, size){
  mu <- mu0 * exp(b * u_bob)
  -sum(dnbinom(y_bob, mu0 = mu, size = size, prob = p, log = TRUE))
  
}






  
