#mixed-effects models using lmer() and glmer()

library(lme4)
library(bbmle)
library(biometrydata)

songbirds <- avonet[avonet$Order1 == "Passeriformes", ]
birds <- data.frame(species = songbirds$Species1,
                    family = songbirds$Family1,
                    hwi = songbirds$Hand.Wing.Index,
                    lat = abs(songbirds$Centroid.Latitude),
                    migratory = as.numeric(songbirds$Migration == 3))
birds <- na.omit(birds)

nrow(birds) #species
length(unique(birds$family)) #families

#fit it
#names: lm_ for lm() fits, mm_ for mixed models
lm_lat <- lm(hwi ~ lat, data = birds) #every species independent
mm_lat <- lmer(hwi ~ lat + (1 | family), data = birds) #each family its own intercept

coef(lm_lat)
summary(mm_lat)
#(1 | family): "an intercept (1) that varies by (|) family
#square the standard deviations to get variances, in this case 41.1/(41.1 + 18.2) = 69%

#additional notes
(1 | family) #each family has its own intercept
(1 + lat | family) #each family has its own intercept and its own slope for latitude
(1 | site/plot) #plots nested within sites
(1 | site) + (1 | year) #crossed - every site was visited in every year

#compare and get intervals
mm_lat_ml <- lmer(hwi ~ lat + (1 | family), data = birds, REML = FALSE)
mm_null_ml <- lmer(hwi ~ 1 + (1 | family), data = birds, REML = FALSE)
AICtab(lm_lat, mm_null_ml, mm_lat_ml)
confint(mm_lat_ml) #profile intervals

#glmer()
bw <- bobwhite_counts
bw$route <- factor(bw$route) #route numbers are names, not amounts
bw$decade <- bw$elapsed / 10 #decades since 1996: glmer() prefers numbers near 1
nrow(bw)
length(unique(bw$route))
#fit it
#negative binomial, every count independent
g_nb <- MASS:: glm.nb(count ~ decade, data = bw)
#the same decline, with a random intercept for route
mm_pois <- glmer(count ~ decade + (1 | route), family = poisson, data = bw)
mm_nb <- glmer.nb(count ~ decade + (1 | route), data = bw)

AICtab(g_nb, mm_pois, mm_nb)
#undo the link
b <- fixef(mm_nb)["decade"] #fixef(): the fixed effects, on the log scale, per decade
exp(b) #birds multiplied by this per decade
exp(b/10) #and per year

ci <- confint(mm_nb, parm = "beta_", method = "Wald")
exp(ci["decade",]/10) #best year, with a 95% confidence interval
exp(coef(g_nb)["decade"]/10) #lecture 15's answer, for comparison

#practice
g_add <- glm(migratory ~ lat + hwi, family = binomial, data = birds)
mm_mig <- glmer(migratory ~ lat + hwi + (1 | family), family = binomial, data = birds)
exp(coef(g_add))
exp(fixef(mm_mig))
summary(g_add)$coefficients
summary(mm_mig)$coefficients
AICtab(g_add, mm_mig)
