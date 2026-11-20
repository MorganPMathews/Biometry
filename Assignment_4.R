#Assignment 4

#goal: apply the modelling techniques discussed in chapter 9 (LMs, GLMs, etc.)
#to system of interest

#getting set up ----------------------------------------------------------

library(bbmle)
#read in dataset
lakeannie <- read.csv("https://github.com/MorganPMathews/Biometry/raw/refs/heads/main/USF_Water_Atlas_Lake_Annie.csv")
#variables of interest - Trophic State Index, True Color, pH
#reformat data
library(tidyverse)
tsi_color_pH <- lakeannie |>
  filter(Characteristic == "Trophic State Index: Florida DEP" | 
           Characteristic == "True Color" |
           Characteristic == "pH") |>
  #data is now 1986 to the present - trophic state index, true color, and pH not included
  #in dataset prior to 1986
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
  #pivot characteristics into columns - if a characteristic has multiple result values for
  #the same row, average them together
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

# research question and models --------------------------------------------

#question: Is the long-term decline in Lake Annie's eutrophication (Trophic State Index, or
#TSI) primarily driven by a continuous temporal trend, color, pH, or a combination of
#temporal and water quality factors?

#candidate models
#Model 1: Baseline/Null Model
tsi.baseline <- lm(tsi ~ 1, data = tsi_color_pH)
#Model 2: Temporal Trend Only
tsi.time <- lm(tsi ~ year_c, data = tsi_color_pH)
#Model 3: Water Quality Drivers Only (Color and pH)
tsi.wq <- lm(tsi ~ color + ph, data = tsi_color_pH)
#Model 4: Full Additive Model (Time, Color, and pH)
tsi.full <- lm(tsi ~ year_c + color + ph, data = tsi_color_pH)
#Model 5: Time + Color
tsi.time_color <- lm(tsi ~ year_c + color, data = tsi_color_pH)
#Model 6: Time + pH
tsi.time_ph    <- lm(tsi ~ year_c + ph, data = tsi_color_pH)

#compare models
AICtab(tsi.baseline, tsi.time, tsi.wq, tsi.full, tsi.inter, tsi.time_color, 
       weights = TRUE, base = TRUE)

#summary stats of best models
summary(tsi.full)
summary(tsi.time)

#interpretation ----------------------------------------------------------

#explanation of distribution and form: I assumed a normal distribution because the best fit
#for the data from previous assignments is normal. I also used a standard linear regression
#(lm()), assuming that model residuals are normally distributed with constant variance,
#because TSI is a continuous physical index score rather than discrete count data (Poisson)
#or bounded binary/probability data (Binomial). Predictors are modeled as direct additive
#effects on TSI, so a transformation link (like log or logit) is not necessary. Lastly,
#model residuals represent symmetric random deviations around the predicted mean with
#constant variance across years and water quality ranges.

#explanation of predictors: 

#tsi.baseline assumes TSI fluctuates randomly around a constant mean across all years and
#water quality conditions (no long-term trend or color/pH drivers).

#tsi.time assumes a linear change over time where TSI changes at a constant annual rate due
#to long-term, consistent trends.

#tsi.wq assumes TSI fluctuates purely in response to water color and pH, independent of
#time.

#tsi.full assumes TSI is an additive function of long-term time trends, water color, and
#lake pH simultaneously.

#tsi.time_color assumes TSI changes linearly over time while while fluctuating with short
#-term water color variations.

#tsi.time_ph assumes TSI changes linearly over time while responding to lake pH shifts.

#biological meaning:

#The simple time model (tsi.time) is within 1 AIC unit of the full additive model
#(tsi.full),demonstrating that time alone explains almost all the variation in model fit.
#This could indicate that long-term changes such as lake catchment size, human influences
#on hydrology and the broader landscape (groundwater withdrawals, channelization, land
#development, soil changes, nutrient inputs etc.), and temperature and rainfall
#(potentially related to climate change) may have a larger effect on a lake's trophic state
#index than other water quality metrics.

#By calculating summary statistics with summary(tsi.time), the time model indicates a
#statistically significant (p < 0.001) long-term decline in lake TSI over the study period.
#At baseline in 1986 (year_c = 0), the estimated mean Trophic State Index for Lake Annie
#was 53.36. The slope estimate is -0.415, demonstrating that TSI has declined by
#approximately 0.42 units per year over the 38-year record. A simple temporal trend alone
#accounts for nearly one-third (32.85%) of total variation in TSI. Residuals are
#symmetrically distributed around zero (median = -0.52, IQR = [-4.32, 4.16]), confirming
#that a linear error structure is appropriate.

#Color and pH alone (tsi.wq) perform worse than the null baseline model, showing how 
#short-term changes in color or pH does not drive TSI fluctuations. 

#Note that observations taken repeatedly from Lake Annie over time lack statistical
#independence

#check models with data --------------------------------------------------

#residual plots for best models:
par(mfrow = c(2, 2))
plot(tsi.time)
par(mfrow = c(1, 1))
#results: Residuals are spread relatively evenly, but there is some skew at higher values.
#The Q-Q plot shows that it's normal.

par(mfrow = c(2, 2))
plot(tsi.full)
par(mfrow = c(1, 1))
#results: Residuals are spread relatively evenly at low values but are uneven at higher
#values. The Q-Q plot shows that it's normal.

#tsi.time model fit with 95% confidence and prediction -------------------
library(ggplot2)

#create prediction grid for tsi.time model:
new_data <- data.frame(
  year_c = seq(min(tsi_color_pH$year_c), max(tsi_color_pH$year_c), length.out = 100)
)

#95% confidence interval (for the mean trend line)
ci_preds <- predict(tsi.time, newdata = new_data, interval = "confidence")

#95% prediction interval (for individual data points)
pi_preds <- predict(tsi.time, newdata = new_data, interval = "prediction")

#combine predictions into a tidy data frame
plot_df <- data.frame(
  year_c = new_data$year_c,
  fit    = ci_preds[, "fit"],
  ci_lwr = ci_preds[, "lwr"],
  ci_upr = ci_preds[, "upr"],
  pi_lwr = pi_preds[, "lwr"],
  pi_upr = pi_preds[, "upr"]
)

#create figure
ggplot() +
  #raw data points
  geom_point(data = tsi_color_pH, aes(x = year_c, y = tsi), 
             color = "gray30", alpha = 0.6, size = 2) +
  #95% prediction interval
  geom_ribbon(data = plot_df, aes(x = year_c, ymin = pi_lwr, ymax = pi_upr, fill = "95% Prediction Band"), 
              alpha = 0.12) +
  geom_line(data = plot_df, aes(x = year_c, y = pi_lwr), color = "lightgreen", linetype = "dotted", size = 0.8) +
  geom_line(data = plot_df, aes(x = year_c, y = pi_upr), color = "lightgreen", linetype = "dotted", size = 0.8) +
  
  #95% confidence interval
  geom_ribbon(data = plot_df, aes(x = year_c, ymin = ci_lwr, ymax = ci_upr, fill = "95% Confidence Band"), 
              alpha = 0.25) +
  
  #fitted mean trend line
  geom_line(data = plot_df, aes(x = year_c, y = fit, color = "Fitted Linear Trend"), 
            size = 1.2) +
  
  #colors
  scale_color_manual(name = "Model Layers", values = c("Fitted Linear Trend" = "darkgreen")) +
  scale_fill_manual(name = "Uncertainty Bands", values = c("95% Confidence Band" = "darkgreen", "95% Prediction Band" = "lightgreen")) +
  
  #axis labels and title
  labs(
    title    = "TSI Temporal Trend with 95% Confidence and Prediction Intervals",
    subtitle = "Model tsi.time: Baseline TSI = 53.36, annual decline slope = -0.42 units/year",
    x        = "Years Since 1986",
    y        = "Trophic State Index (TSI)",
  ) +

  #theme  
  theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(hjust = 0.5, face = "bold", size = 12),
    plot.subtitle      = element_text(hjust = 0.5, color = "gray30", size = 9.5),
    
    axis.title         = element_text(face = "bold", size = 10),
    panel.grid.minor   = element_blank(),
    
   #legend design
    legend.position    = c(0.82, 0.82),
    legend.background  = element_rect(fill = "white", color = "gray80", size = 0.3),
    legend.title       = element_text(size = 8.5, face = "bold"),
    legend.text        = element_text(size = 7.5),
    legend.key.size    = unit(0.35, "cm"),
    legend.margin      = margin(3, 5, 3, 5),
    legend.spacing.y   = unit(0.1, "cm")
  )

#interpretation of plot:

#The prediction plot with 95% confidence bands confirms the fit and assumptions of the
#simple temporal trend model (tsi.time). The solid regression line shows the continuous,
#long-term decline in Lake Annie's Trophic State Index from a mean baseline of ~53.4 in
#1986 down to ~37 present-day. The narrow 95% confidence interval surrounding the trend
#line confirms that the overall trend is estimated precisely. The observed TSI data points
#mostly fall within the 95% prediction band, showing symmetrical distributed above and
#below the line with consistent variance, which confirms constant error variance and 
#normal distribution.

#tsi.full model fit with 95% confidence and prediction -------------------

#create prediction grid for tsi.full model:
new_data <- data.frame(
  year_c = seq(min(tsi_color_pH$year_c, na.rm = TRUE), 
               max(tsi_color_pH$year_c, na.rm = TRUE), 
               length.out = 100),
  color  = mean(tsi_color_pH$color, na.rm = TRUE),
  ph     = mean(tsi_color_pH$ph, na.rm = TRUE)
)

#calculate 95% confidence interval (for the mean trend line)
ci_preds <- predict(tsi.full, newdata = new_data, interval = "confidence")

#calculate 95% prediction interval (for individual data points)
pi_preds <- predict(tsi.full, newdata = new_data, interval = "prediction")

#combine predictions into a tidy data frame
plot_df <- data.frame(
  year_c = new_data$year_c,
  fit    = ci_preds[, "fit"],
  ci_lwr = ci_preds[, "lwr"],
  ci_upr = ci_preds[, "upr"],
  pi_lwr = pi_preds[, "lwr"],
  pi_upr = pi_preds[, "upr"]
)

#create figure
ggplot() +
  #raw data points
  geom_point(data = tsi_color_pH, aes(x = year_c, y = tsi), 
             color = "gray30", alpha = 0.6, size = 2) +
  
  #95% prediction interval
  geom_ribbon(data = plot_df, aes(x = year_c, ymin = pi_lwr, ymax = pi_upr, fill = "95% Prediction Band"), 
              alpha = 0.12) +
  geom_line(data = plot_df, aes(x = year_c, y = pi_lwr), color = "lightgreen", linetype = "dotted", size = 0.8) +
  geom_line(data = plot_df, aes(x = year_c, y = pi_upr), color = "lightgreen", linetype = "dotted", size = 0.8) +
  
  #95% confidence interval
  geom_ribbon(data = plot_df, aes(x = year_c, ymin = ci_lwr, ymax = ci_upr, fill = "95% Confidence Band"), 
              alpha = 0.25) +
  
  #fitted mean trend line
  geom_line(data = plot_df, aes(x = year_c, y = fit, color = "Fitted Linear Trend"), 
            size = 1.2) +
  
  #folors
  scale_color_manual(name = "Model Layers", values = c("Fitted Linear Trend" = "darkgreen")) +
  scale_fill_manual(name = "Uncertainty Bands", values = c("95% Confidence Band" = "darkgreen", "95% Prediction Band" = "lightgreen")) +
  
  #axis labels and title
  labs(
    title    = "TSI Full Model Fit with 95% Confidence and Prediction Intervals",
    subtitle = "Model: tsi.full (evaluated at mean True Color and pH)",
    x        = "Years Since 1986",
    y        = "Trophic State Index (TSI)"
  ) +
  
  #theme  
  theme_minimal(base_size = 11) +
  theme(
    #title and subtitle
    plot.title         = element_text(hjust = 0.5, face = "bold", size = 12),
    plot.subtitle      = element_text(hjust = 0.5, color = "gray30", size = 9.5),
    
    axis.title         = element_text(face = "bold", size = 10),
    panel.grid.minor   = element_blank(),
    
    #legend design
    legend.position    = c(0.82, 0.82),
    legend.background  = element_rect(fill = "white", color = "gray80", size = 0.3),
    legend.title       = element_text(size = 8.5, face = "bold"),
    legend.text        = element_text(size = 7.5),
    legend.key.size    = unit(0.35, "cm"),
    legend.margin      = margin(3, 5, 3, 5),
    legend.spacing.y   = unit(0.1, "cm")
  )

#The tsi.full model plot looks very similar to the tsi.time plot, even though it has two
#additional predictors. While it shows similar trends and has just as good of a fit, the
#simpler model is sufficient to explain TSI considering the data included in the dataset.
#However, additional predictors not included in this dataset could potentially create a
#better, more informative model by going beyond water quality for a more comprehensive
#analysis at the broader lake system.
