#power = rejecting your null hypothesis when it's actually wrong, probability of detecting an effect that is actually there
#power in broader sense = How does the amount of data change my answer? How does the quality of the data change my answer? How I trade cost against what I can learn?

#We need deterministic, distribution, parameters

#dataframe
bbs <- read.csv("https://github.com/bmaitner/biometry_course/raw/refs/heads/main/data/BBS/FL_BBS.csv")
bobwhite <- bbs[bbs$Scientific_Name == "Colinus virginianus",]
nrow(bobwhite) #1360
head(bobwhite[, c("Year", "Route", "total_sightings")])

#test Poisson distribution
counts_real <- bobwhite$total_sightings
mean(counts_real) #10
var(counts_real) #111 #mean is different from variance
mean(counts_real >= 40)
ppois(39, lambda = mean(counts_real), lower.tail = FALSE)

#test negative binomial distribution
mu0 <- mean(bobwhite$total_sightings)
v <- var(bobwhite$total_sightings)
size <- mu0^2 / (v-mu0)
round(c(mean = mu0, variance = v, size = size), 2)
mu0 + mu0^2 /size
pnbinom(39, mu = mu0, size = size, lower.tail = FALSE) #variance same and probability same as actual

#model
#step 1 - expected counts
mu0 <- 10.8
k <- 1.15
d <- 0.03

n_routes <- 40
n_years <- 10

year <- rep(0:(n_years - 1), each = n_routes)
mu <- mu0 * (1-d)^year
plot(year, mu, ylim = c(0,12))

#step 2 - add the noise
counts <- rnbinom(n = length(year), mu = mu, size = k)
plot(jitter(year), counts)
points(year, mu, col = "red", pch = 16)

#step 3 - analyze it (like it is real data)
m <- lm(log(counts + 1) ~ year)
summary(m)$coefficients["year",]

#step 4 - calculate power of one design
year <- rep(0:9, each = 40) #40 routes, 10 years - change this for mean to get close to 1 (below)
mu <- 10.8 * (1-0.03)^year

found <- numeric(500)

for(i in 1:500) {
  counts <- rnbinom(n = length(year), mu = mu, size = 1.15)
  m <- lm(log(counts + 1) ~ year)
  found[i] <- summary(m)$coefficients["year", "Pr(>|t|)"] < 0.05
}
mean(found)
#0.8 power is the usual target
#time beats routes
#another question we could ask is how many years of monitoring are necessary for adequate power

#is it easier to detect a slow decline or a fast one?
year <- rep(0:9, each = 40) 

decline <- 0.1

found <- numeric(500)
for(i in 1:500) {
  counts <- rnbinom(n = length(year), mu = 10.8*(1-decline)^year, size = 1.15)
  m <- lm(log(counts + 1) ~ year)
  found[i] <- summary(m)$coefficients["year", "Pr(>|t|)"] < 0.05
}
mean(found)

#keep in mind:
#power is not a p-value, calculating power after sampling is not helpful, when power is low the effects that reach significance are overestimates

#reef fish
reef <- read.csv("https://github.com/bmaitner/biometry_course/raw/refs/heads/main/data/Reef_fish/NCRMP_reef_fish_surveys.csv")
reef$protected <- ifelse(reef$PROT == 0, "no", "yes")

unprot <- reef$total_abundance[reef$protected == "no"]
prot <- reef$total_abundance[reef$protected == "yes"]

hist(unprot) #raw scale
hist(log(unprot)) #log scale: which looks symmetric?

hist(prot) #raw scale
hist(log(prot)) #log scale: which looks symmetric?

mean(unprot > 500)
plnorm(500, meanlog = log10(mean(unprot)), sdlog = log10(sd(unprot)), lower.tail = FALSE) #lognormal

mean_log_unprot <- mean(log(unprot+1))
sdlog_unprot <- sd(log(unprot+1))

mean_log_prot <- mean(log(prot))
sdlog_prot <- sd(log(prot))

n_per_group <- 60 #120 surveys, split evenly
effect <- 1.5 #the ratio you want to be able to detect

sim_unprot <- plnorm(n_per_group, meanlog = mean_log_unprot, sdlog = sdlog_unprot)
sim_prot <- plnorm(n_per_group, meanlog = mean_log_prot + log(effect), sdlog = sdlog_prot)
