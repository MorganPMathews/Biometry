#part 1: model modification and selection
library(bbmle)
library(biometrydata)

#1a: Do two species of bean beetle compete, and how strongly?------------------------------
beetles <- beetle_competition_interspecific
beetles <- beetles[beetles$pair == "Cmac_Csub" & !is.na(beetles$progeny_1), ]
#plot
plot(beetles$start_females_1 + beetles$start_females_2,
     beetles$progeny_1 / beetles$start_females_1,
     xlab = "females in the dish, both species",
     ylab = "C. maculatus offspring per female")
#starting model - competition model in the Beverton-Holt family
beetle.fit <- mle2(progeny_1 ~ dnbinom(mu = R * start_females_1 /
                                         (1 + (a11 * start_females_1 +
                                                 a12 * start_females_2)^d),
                                       size = k),
                   start = list(R = 20, a11 = 0.1, a12 = 0.1, d = 1, k = 2),
                   lower = c(R = 0.01, a11 = 0, a12 = 0, d = 0.1, k = 0.01),
                   upper = c(R = 1000, a11 = 10, a12 = 10, d = 10, k = 100),
                   method = "L-BFGS-B",
                   data = beetles)
#coefficients and AIC
coef(beetle.fit)
AIC(beetle.fit) #655.9222
#model 1 - Negative Binomial Beverton-Holt Model (Fix d = 1)
#addresses Q1 - Is d needed?
fit_bh <- mle2(progeny_1 ~ dnbinom(mu = R * start_females_1 /
                                     (1 + a11 * start_females_1 + a12 * start_females_2),
                                   size = k),
               start = list(R = 20, a11 = 0.1, a12 = 0.1, k = 2),
               lower = c(R = 0.01, a11 = 0, a12 = 0, k = 0.01),
               upper = c(R = 1000, a11 = 10, a12 = 10, k = 100),
               method = "L-BFGS-B",
               data = beetles)
#model 2 - Negative Binomial, Intraspecific Competition Only (Set a12 = 0)
#addresses Q2 - Does C. subinnotatus affect C. maculatus at all?
fit_no_a12 <- mle2(progeny_1 ~ dnbinom(mu = R * start_females_1 /
                                         (1 + (a11 * start_females_1)^d),
                                       size = k),
                   start = list(R = 20, a11 = 0.1, d = 1, k = 2),
                   lower = c(R = 0.01, a11 = 0, d = 0.1, k = 0.01),
                   upper = c(R = 1000, a11 = 10, d = 10, k = 100),
                   method = "L-BFGS-B",
                   data = beetles)
#model 3 - Negative Binomial, Symmetric Competition (Set a11 = a12 = a1; nlminb optimizer)
#addresses Q3 - Is C. subinnotatus as harmful as C. maculatus?
b_coefs <- coef(beetle.fit)
fit_equal_a <- mle2(
  progeny_1 ~ dnbinom(
    mu = R * start_females_1 / (1 + (a1 * (start_females_1 + start_females_2))^d),
    size = k
  ),
  start = list(
    R  = unname(b_coefs["R"]),
    a1 = mean(c(b_coefs["a11"], b_coefs["a12"])), # Average of a11 and a12
    d  = unname(b_coefs["d"]),
    k  = unname(b_coefs["k"])
  ),
  lower = c(R = 0.01, a1 = 0.0001, d = 0.1, k = 0.01),
  upper = c(R = 1000, a1 = 10, d = 10, k = 100),
  optimizer = "nlminb", #better for non-linear likelihood surfaces
  data = beetles
)
#model 4 - Negative Binomial Ricker Model
#addresses Q4 - Alternative model shape
fit_ricker <- mle2(progeny_1 ~ dnbinom(mu = R * start_females_1 *
                                         exp(-a11 * start_females_1 - a12 * start_females_2),
                                       size = k),
                   start = list(R = 20, a11 = 0.01, a12 = 0.01, k = 2),
                   lower = c(R = 0.01, a11 = 0, a12 = 0, k = 0.01),
                   upper = c(R = 1000, a11 = 10, a12 = 10, k = 100),
                   method = "L-BFGS-B",
                   data = beetles)
#compare all models with AICtab()
AICtab(beetle.fit, fit_bh, fit_no_a12, fit_equal_a, fit_ricker,
       weights = TRUE, delta = TRUE, base = TRUE, logLik = TRUE)
#result: The negative binomial ricker model is the best model (fit_ricker). Competition is
#exploitation-driven, where over-crowding causes resource depletion and the population
#declines at high egg counts.


#1b: Do migratory hawks and eagles have longer wings for their size?-----------------------
hawks <- avonet
hawks <- hawks[hawks$Family1 == "Accipitridae", c("Species1", "Wing.Length", "Mass", "Migration")]
hawks <- na.omit(hawks)
hawks$migration <- factor(hawks$Migration, levels = 1:3,
                          labels = c("resident", "partial", "migrant"))
#plot
plot(log(hawks$Mass), log(hawks$Wing.Length), col = hawks$migration,
     xlab = "log body mass", ylab = "log wing length")
legend("topleft", levels(hawks$migration), col = 1:3, pch = 1)
#starting model - wing length is lognormal, and its log rises linearly with log mass - power law
hawk.fit <- mle2(Wing.Length ~ dlnorm(meanlog = int + b * log(Mass),
                                      sdlog = exp(log_s)),
                 start = list(int = 3, b = 0.3, log_s = -2),
                 parameters = list(int ~ migration,
                                   b ~ migration,
                                   log_s ~ migration),
                 data = hawks)
#coefficients and AIC
coef(hawk.fit)
AIC(hawk.fit) #2645.972
#model 1 - Lognormal Constrained Intercept Model
#addresses Q1 - Do migrants have longer wings for their mass? (altering int)
hawk.fit.no_int <- mle2(Wing.Length ~ dlnorm(meanlog = int + b * log(Mass),
                                             sdlog = exp(log_s)),
                        start = list(int = 3, b = 0.3, log_s = -2),
                        parameters = list(b ~ migration,
                                          log_s ~ migration), # int is now constant
                        data = hawks)
#model 2 - Lognormal Constrained Slope Model
#addresses Q2 - Does wing length scale with mass differently in migrants? (altering b)
hawk.fit.no_slope <- mle2(Wing.Length ~ dlnorm(meanlog = int + b * log(Mass),
                                               sdlog = exp(log_s)),
                          start = list(int = 3, b = 0.3, log_s = -2),
                          parameters = list(int ~ migration,
                                            log_s ~ migration), # b is now constant
                          data = hawks)
#model 3 - Lognormal Constant Variance Model
#addresses Q3 - Are some migration classes more variable than others? (altering log_s)
hawk.fit.equal_var <- mle2(Wing.Length ~ dlnorm(meanlog = int + b * log(Mass),
                                                sdlog = exp(log_s)),
                           start = list(int = 3, b = 0.3, log_s = -2),
                           parameters = list(int ~ migration,
                                             b ~ migration), # log_s is now constant
                           data = hawks)
#model 4 - Lognormal Binary Class Model
#addresses Q4 - Is the distinction between partial migrants and full migrants needed?
#(altering data/categories and creating a new binary migration column)
hawks$migrant_vs_resident <- factor(ifelse(hawks$migration == "resident", "resident", "migrant"))
hawk.fit.binary <- mle2(Wing.Length ~ dlnorm(meanlog = int + b * log(Mass),
                                             sdlog = exp(log_s)),
                        start = list(int = 3, b = 0.3, log_s = -2),
                        parameters = list(int ~ migrant_vs_resident,
                                          b ~ migrant_vs_resident,
                                          log_s ~ migrant_vs_resident),
                        data = hawks)
#compare all models with AICtab()
AICtab(hawk.fit, hawk.fit.no_int, hawk.fit.no_slope, hawk.fit.equal_var, hawk.fit.binary,
       weights = TRUE, delta = TRUE, base = TRUE, logLik = TRUE)
#result: The Lognormal Binary Class Model is the best model (hawk.fit.binary). It fits best
#because partial and full migrants share nearly identical evolutionary adaptations for long
#distance travel, making their wing-to-mass scaling biologically indistinguishable from one
#another. Splitting them into separate groups adds unnecessary complexity without
#demonstrating any distinct ecological differences in how their wings scale with body size.


#1c: What explains how many fish species a diver records on a reef?------------------------
reef <- reef_fish_surveys
reef$depth <- reef$DEPTH / 10
reef$visibility <- reef$UNDERWATER_VISIBILITY / 10
reef$year <- reef$YEAR - 2014
reef$region <- factor(reef$REGION_DESCRIPTION,
                      levels = c("Dry Tortugas",
                                 "Florida Keys: from Key West to Miami",
                                 "Miami to North Martin County"),
                      labels = c("Tortugas", "Keys", "North"))
reef$protected <- ifelse(reef$MPA_NAME == "Unprotected", "no", "yes")
#plot
plot(reef$DEPTH, reef$species_richness, col = rgb(0, 0, 0, 0.1),
     xlab = "depth (m)", ylab = "species recorded")
#starting model - richness is Poisson, its log rises and then falls with depth
#depends on visibility, year, region, and protection
reef.fit <- mle2(species_richness ~ dpois(lambda = exp(int +
                                                         d1 * depth +
                                                         d2 * depth^2 +
                                                         v * visibility +
                                                         t * year)),
                 start = list(int = 3, d1 = 0, d2 = 0, v = 0, t = 0),
                 parameters = list(int ~ region + protected),
                 data = reef)
#coefficients and AIC
coef(reef.fit)
AIC(reef.fit) #76092.11
#addresses Q1 - Is the Poisson the right distribution?
dispersion_ratio <- var(reef$species_richness, na.rm = TRUE) / mean(reef$species_richness, na.rm = TRUE) #ratio is 4.43 - negative binomial may be a better fit since ratio is >1
#model 1 - Negative Binomial
#addresses Q1 - Handles overdispersion evident from var()/mean()
reef.nb <- mle2(
  species_richness ~ dnbinom(mu = exp(int + 
                                        d1 * depth + 
                                        d2 * depth^2 + 
                                        v * visibility + 
                                        t * year),
                             size = k),
  start = list(int = 3, d1 = 0, d2 = 0, v = 0, t = 0, k = 10),
  parameters = list(int ~ region + protected),
  data = reef
)
#model 2 - Negative Binomial, Linear Depth Only
#addresses Q2 - Which effects are large enough with N = 8,560?
reef.linear_depth <- mle2(
  species_richness ~ dnbinom(mu = exp(int + 
                                        d1 * depth + 
                                        v * visibility + 
                                        t * year),
                             size = k),
  start = list(int = 3, d1 = 0, v = 0, t = 0, k = 10),
  parameters = list(int ~ region + protected),
  data = reef
)
#model 3 - Negative Binomial, Visibility Removed
#addresses Q3 - What might a visibility effect be measuring, considering that visibility depends on the day?
reef.no_vis <- mle2(
  species_richness ~ dnbinom(mu = exp(int + 
                                        d1 * depth + 
                                        d2 * depth^2 + 
                                        t * year),
                             size = k),
  start = list(int = 3, d1 = 0, d2 = 0, t = 0, k = 10),
  parameters = list(int ~ region + protected),
  data = reef
)
#model 4 - Negative Binomial, Region Only
#addresses Q4 - Does region alone account for variation?
reef.no_protection <- mle2(
  species_richness ~ dnbinom(mu = exp(int + 
                                        d1 * depth + 
                                        d2 * depth^2 + 
                                        v * visibility + 
                                        t * year),
                             size = k),
  start = list(int = 3, d1 = 0, d2 = 0, v = 0, t = 0, k = 10),
  parameters = list(int ~ region), # Removed 'protected'
  data = reef
)
#compare all models with AICtab()
AICtab(reef.fit, reef.nb, reef.linear_depth, reef.no_vis, reef.no_protection,
       weights = TRUE, delta = TRUE, base = TRUE, logLik = TRUE)
#back transform
best_coefs <- coef(reef.nb)
multipliers <- exp(best_coefs)
print(round(multipliers, 4))
#result: The Negative Binomial is the best model (reef.nb). Reef fish species richness
#exhibits significant overdispersion (var()/mean()), making a Poisson distribution a poor
#fit. Biologically, species richness on coral reefs is typically patchy with some areas of
#high richness and some with none. The Poisson model assumes a constant mean-variance
#relationship while the Negative Binomial distribution models the species richness clusters
#and environmental heterogeneity by accounting for high dispersion. The alternative models
#were poorer fits because they excluded important biological and sampling factors.


#part 2: focal dataset model selection-----------------------------------------------------
#select one or more response variables - two or more models and compare them via AIC values
#explain which model(s) are best

#getting set up
#read in dataset
lakeannie <- read.csv("https://github.com/MorganPMathews/Biometry/raw/refs/heads/main/USF_Water_Atlas_Lake_Annie.csv")
#variables of interest - Trophic State Index, True Color, pH
#reformat data
library(tidyverse)
tsi_color_pH <- lakeannie |>
  filter(Characteristic == "Trophic State Index: Florida DEP" | 
           Characteristic == "True Color" |
           Characteristic == "pH") |>
#data is now 1986 to the present - trophic state index, true color, and pH not included in dataset prior to 1986
  #rename
mutate(Characteristic = recode(Characteristic, 
                          "Trophic State Index: Florida DEP" = "Trophic State Index")) |> 
#make sure result value is numeric
mutate(ResultValue = as.numeric(ResultValue)) |> 
#extract year from date column and make it a variable
mutate(Year = as.numeric(str_sub(SampleDate, start = -4))) |>
#select necessary columns
select(StationID, 
        SampleDate,
        Year,
        Characteristic, 
        ResultValue) |> 
#pivot characteristics into columns - if a characteristic has multiple result values for the same row, average them together
pivot_wider(names_from = Characteristic,
            values_from = ResultValue,
            values_fn = mean) |>
#additional changes for modelling
#rename
rename(
  tsi = `Trophic State Index`,
  color = `True Color`,
  ph = pH,
  year = Year
) |> 
#drop incomplete rows 
drop_na(tsi, color, ph, year) |> 
#center Year so year_c = 0 represents baseline in 1986
mutate(year_c = year - min(year, na.rm = TRUE))

#Response Variable 1: Trophic State Index (TSI)
#Determine whether lake eutrophication is primarily driven by long-term time trends (Year),
#CDOM browning (True Color), acidity shifts (pH), or an interaction between organic matter
#and pH.
#model 1 - Normal, Baseline Intercept
tsi.baseline <- mle2(
  tsi ~ dnorm(mean = int, sd = exp(log_sigma)),
  start = list(int = mean(tsi_color_pH$tsi), log_sigma = log(sd(tsi_color_pH$tsi))),
  data = tsi_color_pH
)
#model 2 - Normal, Temporal Trend Only
tsi.time <- mle2(
  tsi ~ dnorm(mean = int + b_year * year_c, sd = exp(log_sigma)),
  start = list(int = 50, b_year = 0, log_sigma = 2),
  data = tsi_color_pH
)
#model 3 - Normal, Water Quality Drivers (Color and pH)
tsi.wq <- mle2(
  tsi ~ dnorm(mean = int + b_color * color + b_ph * ph, sd = exp(log_sigma)),
  start = list(int = 50, b_color = 0, b_ph = 0, log_sigma = 2),
  data = tsi_color_pH
)
#model 4 - Normal, Full Additive Model (Time, Color, and pH)
tsi.full <- mle2(
  tsi ~ dnorm(mean = int + b_year * year_c + b_color * color + b_ph * ph, sd = exp(log_sigma)),
  start = list(int = 50, b_year = 0, b_color = 0, b_ph = 0, log_sigma = 2),
  data = tsi_color_pH
)
#model 5 - Normal, Interaction Model (Time + Color * pH)
tsi.inter <- mle2(
  tsi ~ dnorm(mean = int + b_year * year_c + b_color * color + b_ph * ph + b_cxp * (color * ph), sd = exp(log_sigma)),
  start = list(int = 50, b_year = 0, b_color = 0, b_ph = 0, b_cxp = 0, log_sigma = 2),
  data = tsi_color_pH
)
#compare models
AICtab(tsi.baseline, tsi.time, tsi.wq, tsi.full, tsi.inter, weights = TRUE, base = TRUE)
#result: The best model for Trophic State Index is the normal full additive model that
#includes time, color, and pH (tsi.full). As documented by Gaiser et al. (2009), CDOM
#accounts for over 83% of transparency fluctuations in Lake Annie. CDOM consists of organic
#humic acids that darken the water column while also altering lake acidity. Including both
#color and ph allows the model to show how humic inputs dictate whether Lake Annie enters a
#dystrophic, eutrophic, or mixotrophic state. The other models ignore how CDOM loading
#fluctuates between wet and dry years due to natural climate oscillations, as well as how
#the catchment has changed over time.

#Response Variable 2: True Color (Browning / CDOM)
#Predict terrestrial organic matter inputs over time and acidity.
#model 1 - Normal, Linear Time
color.linear <- mle2(
  color ~ dnorm(mean = int + b_year * year_c, sd = exp(log_sigma)),
  start = list(int = mean(tsi_color_pH$color), b_year = 0, log_sigma = 2),
  data = tsi_color_pH
)
#model 2 - Nomral, Quadratic Time
color.quad <- mle2(
  color ~ dnorm(mean = int + b_year1 * year_c + b_year2 * (year_c^2), sd = exp(log_sigma)),
  start = list(int = mean(tsi_color_pH$color), b_year1 = 0, b_year2 = 0, log_sigma = 2),
  data = tsi_color_pH
)
#Model 3 - Normal, Time and Acidity
color.ph <- mle2(
  color ~ dnorm(mean = int + b_year * year_c + b_ph * ph, sd = exp(log_sigma)),
  start = list(int = mean(tsi_color_pH$color), b_year = 0, b_ph = 0, log_sigma = 2),
  data = tsi_color_pH
)
#compare models
AICtab(color.linear, color.quad, color.ph, weights = TRUE, base = TRUE)
#result: The best model for True Color is the normal quadratic time model (color.quad). As
#shown by Emery Boeck et al. (2023), Lake Annie's browning began following watershed
#catchment expansion, but shifts in color are driven by natural climate oscillations that
#affect rainfall. This model shows these changes in terrestrial humic runoff better than a
#linear trend.
