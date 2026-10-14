library(readr)
library(ggplot2)

#3 datasets
#dataset 1 - Adult body mass and egg mass, for 250 bird species, from the Amniote Life History Database. Treat adult body mass (g) as the predictor (x) and egg mass (g) as the response (y).
twoa <- read_rds("https://github.com/bmaitner/biometry_course/raw/refs/heads/main/midterm/b_2a.RDS")
#assess dataset 1 shape with scatterplot
ggplot(twoa, aes(x = Adult.Body.Mass.g, y = Egg.Mass.g)) +
  geom_point(alpha = 0.6, color = "darkseagreen") +
  labs(
    title = "Adult Body Mass vs. Egg Mass",
    x = "Adult Body Mass (g)",
    y = "Egg Mass (g)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"))
#make scatterplot lognormal
ggplot(twoa, aes(x = log10(Adult.Body.Mass.g), y = log10(Egg.Mass.g))) +
  geom_point(alpha = 0.6, color = "darkseagreen") +
  geom_smooth(method = "lm", color = "darkgreen", se = TRUE) +
  labs(
    title = "Log-Transformed Adult Body Mass vs. Egg Mass",
    x = "log10 Adult Body Mass (g)",
    y = "log10 Egg Mass (g)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"))
#calculate mean and sd
log_egg_mean <- mean(log10(twoa$Egg.Mass.g), na.rm = TRUE)
log_egg_sd <- sd(log10(twoa$Egg.Mass.g), na.rm = TRUE)
#dataset 1 distribution - lognormal
ggplot(twoa, aes(x = log10(Egg.Mass.g))) +
  geom_histogram(aes(y = after_stat(density)), na.rm = TRUE, bins = 20, fill = "darkseagreen", color = "white", alpha = 0.8) +
  stat_function(
    fun = dnorm, 
    args = list(mean = log_egg_mean, sd = log_egg_sd),
    color = "darkgreen", 
    linewidth = 1.2
  ) +
  labs(title = "Lognormal Distribution of Egg Mass",
       subtitle = "N = 250",
       x = "log10 Egg Mass (g)",
       y = "Density") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "darkgray"))

#dataset 1 additional evidence
egg_mean <- mean(twoa$Egg.Mass.g, na.rm = TRUE) #22.66348
egg_median <- median(twoa$Egg.Mass.g, na.rm = TRUE) #3.92

log_egg_mean <- mean(log10(twoa$Egg.Mass.g), na.rm = TRUE) #0.80278
log_egg_median <- median(log10(twoa$Egg.Mass.g), na.rm = TRUE) #0.59327

#A. dataset 1 shape - power law
#B. dataset 1 distribution - lognormal
#C. dataset 1 explanation - Biological body traits and mass follow power laws where y is proportional to a power of x, as shown in the scatterplot. The log-transformed scatterplot reflected that a lognormal curve would fit, and the distribution has a strong positive skew that is characteristic of a lognormal distribution. After applying a log10 transformation, it removes the extreme skew and normalizes the distribution. Comparing the mean and median provides additional evidence. On the original scale, the mean is 22.66348 while the median is 3.92, and on the log-transformed scale the mean is 0.80278 while the median is 0.59327.

#dataset 2 - Reef fish surveys from the Florida Keys, 250 surveys, from NOAA’s National Coral Reef Monitoring Program. Treat the total abundance of fish recorded on a survey as the predictor (x) and the number of species recorded as the response (y).
twob <- read_rds("https://github.com/bmaitner/biometry_course/raw/refs/heads/main/midterm/b_2b.RDS")
#assess dataset 2 shape with scatterplot
ggplot(twob, aes(x = Total.Fish.Abundance, y = Species.Richness)) +
  geom_point(alpha = 0.5) +
#linear model line
  geom_smooth(
    method = "lm", 
    formula = y ~ x, 
    aes(color = "Linear"), 
    se = FALSE
  ) +
#logarithmic model line to compare
  geom_smooth(
    method = "lm", 
    formula = y ~ log(x), 
    aes(color = "Logarithmic"), 
    se = FALSE
  ) +
  scale_color_manual(
    name = "Model Curve",
    values = c("Linear" = "navy", "Logarithmic" = "steelblue")
  ) +
  labs(
    title = "Species Richness vs. Total Fish Abundance",
    x = "Total Fish Abundance",
    y = "Species Richness"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(legend.position = "top")
#dataset 2 distributions - compare Poisson and negative binomial
#summary stats
richness_mean <- mean(twob$Species.Richness, na.rm = TRUE)
richness_var <- var(twob$Species.Richness, na.rm = TRUE)
dispersion <- richness_var / richness_mean
#calculate negative binomial size (k) parameter
nb_mu <- richness_mean
nb_size <- (richness_mean^2) / (richness_var - richness_mean)
#plot - Poisson
ggplot(twob, aes(x = Species.Richness)) +
  geom_histogram(
    aes(y = after_stat(density)), 
    binwidth = 1, 
    fill = "steelblue", 
    color = "white", 
    alpha = 0.8
  ) +
  stat_function(
    fun = function(x) dpois(round(x), lambda = richness_mean),
    color = "navy", 
    linewidth = 1.2,
    n = max(twob$Species.Richness, na.rm = TRUE) + 1
  ) +
  labs(
    title = "Poisson Distribution of Species Richness",
    subtitle = "N = 250",
    x = "Species Richness",
    y = "Density"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "darkgray"))
#plot - negative binomial
ggplot(twob, aes(x = Species.Richness)) +
  geom_histogram(
    aes(y = after_stat(density)), 
    binwidth = 1, 
    fill = "steelblue", 
    color = "white", 
    alpha = 0.8
  ) +
  stat_function(
    fun = function(x) dnbinom(round(x), size = nb_size, mu = nb_mu),
    color = "navy", 
    linewidth = 1.2,
    n = max(twob$Species.Richness, na.rm = TRUE) + 1
  ) +
  labs(
    title = "Negative Binomial Distribution of Species Richness",
    subtitle = paste0("N = 250"),
    x = "Species Richness",
    y = "Density"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "darkgray"))
#check for multiple peaks/humps in the pooled data
ggplot(twob, aes(x = Species.Richness)) +
  geom_density(fill = "steelblue", alpha = 0.4, color = "navy", linewidth = 1) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 1, fill = "darkgray", alpha = 0.5) +
  labs(
    title = "Species Richness Density",
    x = "Species Richness",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"))
  
#dataset 2 additional evidence
#Mean = 21.78 species per survey
#Variance = 112.56
#Variance-to-Mean Ratio = 5.17 
#Negative Binomial Dispersion (k) = 5.23

#A. dataset 2 shape - logarithmic
#B. dataset 2 distribution - negative binomial
#C. dataset 2 explanation - The scatterplot that shows both linear and logarithmic lines depicts that the shape is logarithmic. There is not a distribution in the bestiary that matches the distribution of this dataset. Because species richness represents discrete integer counts, I tested the Poisson distribution and negative binomial distribution, but they do not fit all the data. My histogram shows that there are two distinct humps at about 18 species and 35 species, which may reveal that two sub-populations were pooled together. However, within each group, species richness would likely follow a negative binomial distribution. This is because the variance is much greater than the mean, making the data too dispersed for a Poisson distribution, and the negative binomial is a better fit.

#dataset 3 - Benthic surveys of 150 reef stations in the Florida Keys. Rugosity is a measure of how structurally complex the sea floor is, measured from 0 to about 2. Treat rugosity as the predictor (x) and the percentage of the station covered by hard bottom as the response (y).
twoc <- read_rds("https://github.com/bmaitner/biometry_course/raw/refs/heads/main/midterm/b_2c.RDS")
#first convert percentage to proportion (0 to 1)
twoc$prop_cover <- twoc$Hard.Bottom.Cover.Pct / 100
#assess dataset 3 shape with scatterplot
ggplot(twoc, aes(x = Rugosity, y = prop_cover)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    title = "Hard Bottom Cover vs. Sea Floor Rugosity",
    x = "Rugosity",
    y = "Proportion Hard Bottom Cover"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"))
#summary stats
p_mean <- mean(twoc$prop_cover, na.rm = TRUE)
p_var  <- var(twoc$prop_cover, na.rm = TRUE)
#calculate shape1 (alpha) and shape2 (beta)
common_factor <- (p_mean * (1 - p_mean) / p_var) - 1
beta_shape1   <- p_mean * common_factor
beta_shape2   <- (1 - p_mean) * common_factor
#dataset 3 distribution - beta
ggplot(twoc, aes(x = prop_cover)) +
  geom_histogram(
    aes(y = after_stat(density)), 
    binwidth = 0.05, 
    fill = "lightblue", 
    color = "white", 
    alpha = 0.8
  ) +
  stat_function(
    fun = dbeta, 
    args = list(shape1 = beta_shape1, shape2 = beta_shape2),
    n = 200
  ) +
  labs(
    title = "Beta Distribution of Hard Bottom Cover",
    subtitle = paste0("N = 150"),
    x = "Proportion Hard Bottom Cover",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "darkgray")
  )

#A. dataset 3 shape - saturating/asymptotic plateau
#B. dataset 3 distribution - beta
#C. dataset 3 explanation - The scatterplot shows that hard bottom cover starts around 0.75-0.8 at low rugosity, rises slightly, and then plateus near 0.85-0.9 cover at higher rugosity as it approaches 1, or 100% cover. This shows a saturating or asymptotic plateau shape. Since the data for the proportion of cover is bounded between 0 and 1, a beta distribution was a sensible assumption. When tested, it followed the left-skewed, J-shaped distribution that showed how most stations were concentrated around 1 (100% cover).

##power analysis-----------------------------------------------------------------------
library(ggplot2)
library(tidyr)
library(dplyr)
#run simulation
#model parameters
a <- 2 #intercept
b <- 1 #slope
nsim <- 400 #number of iterations per combination
sdvec <- seq(1, 20, by = 1) #sd
nvec  <- c(10, 20, 50, 100) #test 4 different sample sizes
#matrix to store power results (rows = sd, columns = sample size)
power_matrix <- matrix(NA, nrow = length(sdvec), ncol = length(nvec))
colnames(power_matrix) <- paste0("N = ", nvec)

set.seed(123) #set seed for reproducible simulation results

#simulation loops
#sample size
for (k in 1:length(nvec)) {
  sample_size <- nvec[k]
  
  #standard deviation
  for (j in 1:length(sdvec)) {
    sd_val <- sdvec[j]
    pval <- numeric(nsim)
    
    for (i in 1:nsim) {
      x <- sample(x = 1:20, size = sample_size, replace = TRUE)
      y_det <- a + b * x
      y <- rnorm(n = sample_size, mean = y_det, sd = sd_val)
      
      m <- lm(y ~ x)
      
      #get p-value
      pval[i] <- coef(summary(m))["x", "Pr(>|t|)"]
    } #end i loop
    
    #calculate power
    power_matrix[j, k] <- sum(pval < 0.05) / nsim
  } #end j loop
  } #end k loop

#convert matrix to data frame
results_df <- data.frame(power_matrix, check.names = FALSE)
results_df$SD <- sdvec

#pivot all columns except SD
results_long <- results_df %>%
  pivot_longer(
    cols = -SD, 
    names_to = "Sample_Size", 
    values_to = "Power"
  ) %>%
  mutate(Sample_Size = factor(Sample_Size, levels = paste0("N = ", nvec)))

#plot
ggplot(results_long, aes(x = SD, y = Power, color = Sample_Size, shape = Sample_Size)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 0.80, linetype = "dashed", color = "gray40") + #80% power threshold
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(
    title = "Statistical Power vs. Noise Across Sample Sizes",
    subtitle = "Linear Model: y = 2 + 1*x (p < 0.05)",
    x = "Standard Deviation (Noise)",
    y = "Statistical Power (Proportion p < 0.05)",
    color = "Sample Size",
    shape = "Sample Size"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray30")
  )

#explanation - The greater the noise in a dataset, the less power it has. The greater the sample size, the greater the power. If you could either double your sample size or halve your measurement error, halving your measurement error would have a greater impact on the power. This is because standard error is noise divided by the square root of the sample size. Since noise is linear, halving the measurement error reduces the standard error by 50%.

