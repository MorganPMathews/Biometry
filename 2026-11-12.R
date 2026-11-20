#Using glm()

library(bbmle)
library(biometrydata)
occ <- bobwhite_occupancy

#names: m_ for mle2() fits, g for glm() fits
#lecture 16's model, by hand
m_occ <- mle2(n_occupied ~ dbinom(size = n_routes, prob = plogis(a + b * elapsed)),
              start = list(a = 2, b = -0.05), data = occ)

#the same model: cbind(successes, failures) on the left, the predictors on the right
#cbind is (successes, failures)
g_occ <- glm(cbind(n_occupied, n_routes - n_occupied) ~ elapsed,
             family = binomial, data = occ)

coef(m_occ)
coef(g_occ)
c(logLik(m_occ), logLik(g_occ))

#confidence intervals
#2019 is 22 years after 1997
p19 <- predict(g_occ, newdata = data.frame(elapsed = 22),
               type = "link", se.fit = TRUE)
p19$fit #the estimate for 2019, on the logit scale
p19$se.fit #its standard error, also on the logit scale
#the lower end, estimate, upper end: all still logits
ends <- p19$fit + c(-1.96, 0, 1.96) * p19$se.fit
plogis(ends) #undo the link: back to probabilities

#easier way
library(marginaleffects)
predictions(g_occ, newdata = data.frame(elapsed = 22))

library(emmeans)
emmeans(g_occ, ~ elapsed, at = list(elapsed = 22), type = "response")

#binomial data (yes/no)
library(ggplot2)
songbirds <- avonet[avonet$Order1 == "Passeriformes", ]
birds <- data.frame(species = songbirds$Species1,
                    family = songbirds$Family1,
                    hwi = songbirds$Hand.Wing.Index,
                    lat = abs(songbirds$Centroid.Latitude), #degrees from the equator
                    migratory = as.numeric(songbirds$Migration == 3), #1 yes, 0 no
                    mass = songbirds$Mass, #g
                    wing = songbirds$Wing.Length) #mm
birds <- na.omit(birds)
mean(birds$migratory)
#fit it 
#for 0/1 data the response is just the 0/1 column
g_lat <- glm(migratory ~ lat, family = binomial, data = birds)
g_add <- glm(migratory ~ lat + hwi, family = binomial, data = birds)

AICtab(g_lat, g_add)
coef(g_add)
#draw it
#two kinds of songbird: rounded wings (HWI 15) and pointed wings (HWI 30)
grid <- expand.grid(lat = 0:70, hwi = c(15,30))
p <- predict(g_add, newdata = grid, type = "link", se.fit = TRUE)
grid$prob <- plogis(p$fit)
grid$lwr <- plogis(p$fit - 1.96 * p$se.fit)
grid$upr <- plogis(p$fit + 1.96 * p$se.fit)

ggplot(grid, aes(x = lat, y = prob, fill = factor(hwi))) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3) +
  geom_line(aes(color = factor(hwi))) +
  labs(x = "degrees from the equator", y = "probability of migrating",
       fill = "HWI", color = "HWI")

#same figure, easy way
plot_predictions(g_add, condition = list("lat", hwi = c(15, 30))) +
  labs(x = "degrees from the equator", y = "probability of migrating")

#poisson
reef <- reef_surveys
g_pois <- glm(richness ~ depth + I(depth^2), family = poisson, data = reef)
coef(g_pois)
#if the scatter really is poisson, about 95% of surveys fall in this range
mu <- fitted(g_pois)
lo <- qpois(0.025, mu)
hi <- qpois(0.975, mu)
mean(reef$richness >= lo & reef$richness <= hi)
#the fix: negative binomial
g_nb <- MASS::glm.nb(richness ~ depth + I(depth^2), data = reef)
AICtab(g_pois, g_nb) #as in lecture 18: poisson against negative binomial
summary(g_nb)

#curves with normal scatter: nls()
start_ll <- coef(lm(log(wing) ~ log(mass), data = birds))
n_wing <- nls(wing ~ a * mass^b, data = birds,
              start = list(a = exp(start_ll[[1]]), b = start_ll[[2]]))
coef(n_wing)
confint.default(n_wing)

#new example
corals <- coral_cover[coral_cover$REGION == "Tortugas", ]
corals$station <- paste(corals$PRIMARY_SAMPLE_UNIT, corals$STATION_NR, corals$YEAR)
#one row per station visit
stations <- unique(corals[, c("station", "YEAR")])
#1 if any highly susceptible coral was recorded there, 0 if not
susceptible <- "Colpophyllia|Dendrogyra|Dichocoenia|Diploria|Eusmilia|Meandrina"
found_at <- corals$station[grepl(susceptible, corals$COVER_CAT_NAME)]
stations$found <- as.numeric(stations$station %in% found_at)
stations$year <- factor(stations$YEAR)
tapply(stations$found, stations$year, mean)

# g_null: found ~ 1, a binomial glm with no predictors
g_null <- glm(found ~ 1, family = binomial, data = stations)
# g_year: found ~ year
g_year <- glm(found ~ year, family = binomial, data = stations)
# compare them with AICtab()
AICtab(g_null, g_year)
# the probability for each year, with a 95% interval:
p22 <- predict(g_year, newdata = data.frame(year = 2021), #not correct!
               type = "link", se.fit = TRUE)
p22$fit
p22$se.fit
#the lower end, estimate, upper end: all still logits
ends <- p22$fit + c(-1.96, 0, 1.96) * p22$se.fit
plogis(ends) #undo the link: back to probabilities



















