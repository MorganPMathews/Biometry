#General Linear Models
#use lm()

#example using hand-wing index (HWI)
library(bbmle)
library(biometrydata)

songbirds <- avonet[avonet$Order1 == "Passeriformes", ]
birds <- data.frame(species = songbirds$Species1,
                    family = songbirds$Family1,
                    hwi = songbirds$Hand.Wing.Index,
                    lat = abs(songbirds$Centroid.Latitude), #degrees from the equator
                    migration_code = songbirds$Migration, #1, 2, or 3
                    habitat_code = songbirds$Habitat.Density) #1, 2, or 3
birds <- na.omit(birds)

#mle() way for reference
m_lat <- mle2(hwi ~ dnorm(mean = a + b * lat, sd = s),
              start = list(a = 15, b = 0, s = 5), data = birds)
#linear regression
lm_lat <- lm(hwi ~ lat, data = birds)

coef(m_lat)
coef(lm_lat)
c(logLik(m_lat), logLik(lm_lat))

#examine the fit
summary(lm_lat) #estimates, standard errors, R-squared
confint(lm_lat) #95% confidence intervals
sigma(lm_lat) #the fitted sd of the scatter

#ANOVA: a categorical predictor
lm_num <- lm(hwi ~ migration_code, data = birds) #codes 1, 2, 3 as numbers
coef(lm_num)
#tell R the codes are categories
birds$migration <- factor(birds$migration_code, levels = 1:3,
                          labels = c("sedentary", "partial", "migratory"))
lm_mig <- lm(hwi ~ migration, data = birds)
coef(lm_mig)
AIC(lm_num, lm_mig) #lm_mig is better

#three means
lm(hwi ~ migration - 1)

#ANCOVA: continuous and categorical predictors
lm_add <- lm(hwi ~ lat + migration, data = birds) #one slope, three intercepts
lm_int <- lm(hwi ~ lat * migration, data = birds) #each group its own slope

AICtab(lm_lat, lm_mig, lm_add, lm_int) #lm_add is the best
coef(lm_add)

#check if assumptions are met by plotting
plot(lm_add, which = 2) #normal Q-Q: are the residuals normal? The closer the observed values are to the model line, the better the fit
plot(lm_add, which = 1) #residuals against fitted: is the scatter even?

#confidence intervals
#a migratory songbird, 45 degrees from the equator
new_bird <- data.frame(lat = 45, migration = factor("migratory", levels = levels(birds$migration)))

predict(lm_add, newdata = new_bird, interval = "confidence") #the average bird
predict(lm_add, newdata = new_bird, interval = "prediction") #the one new species

#drawing predictions
library(ggplot2)
#the birds to predict for: every latitude from 0 to 70, in each migration group
grid <- expand.grid(lat = 0:70, migration = levels(birds$migration))
conf <- predict(lm_add, newdata = grid, interval = "confidence")
pred <- predict(lm_add, newdata = grid, interval = "prediction")
grid <- data.frame(grid, conf, pred_lwr = pred[, "lwr"], pred_upr = pred[, "upr"])

ggplot(grid, aes(x = lat)) +
  geom_point(data = birds, aes(y = hwi), size = 0.3, alpha = 0.2) +
  geom_ribbon(aes(ymin = pred_lwr, ymax = pred_upr), fill = "#2a7ab0", alpha = 0.2) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "#2a7ab0", alpha = 0.6) +
  geom_line(aes(y = fit)) +
  facet_wrap(~ migration) +
  labs(x = "degrees from the equator", y = "hand-wing index")

#fix for long upper tail - log(hwi)
lm_log <- lm(log(hwi) ~ lat + migration, data = birds)
exp(confint(lm_log)) #back on the HWI scale: each effect is now "times"
exp(predict(lm_log, newdata = new_bird, interval = "prediction"))
#prediction isn't exactly the same but the effects otherwise hold up

#habitat example
lm_hab_num <- lm(hwi ~ lat + migration + habitat_code, data = birds)
coef(lm_hab_num)
#tell R the codes are categories
birds$habitat <- factor(birds$habitat_code, levels = 1:3,
                          labels = c("dense", "semi-open", "open"))
lm_hab_fac <- lm(hwi ~ lat + migration + habitat, data = birds) #one slope, three intercepts
AICtab(lm_add, lm_hab_num, lm_hab_fac) #best one is lm_hab_fac
