#PART 1------------------------------------------------------------------------
#1.Number of surviving individuals within a forest plot, out of a known starting number
#A - Discrete - The data are integers, with counts of individual trees that can't be divided into fractions.
#B - Binomial - There is a known starting sample size with only two outcomes (alive or dead).

#2.Species abundance (counts of individuals in a plot)
#A - Discrete - The data are integers, as it consists of counts of individual organisms.
#B - Poisson - There is no maximum for organism counts. Assuming the individuals are independent and randomly distributed with the variance equal to the mean, then a Poisson would be appropriate.

#3.Pollinator visitation rate (visits per flower per unit time)
#A - Discrete - The data are integers, as it consists of counts of pollinator visits that occur over a certain amount of time.
#B - Poisson - There is no maximum for pollinator visits. Assuming the individuals are independent and randomly distributed with the variance equal to the mean, then a Poisson would be appropriate.

#4.Ages (in years) of surviving individuals within a reserve
#A - Discrete - Age in years is only integers.
#B - Geometric - A geometric distribution shows discrete time steps before death.

#5.Body size (e.g., body mass, length)
#A - Continuous - Body mass and length can have decimal values.
#B - Lognormal - Organisms grow exponentially over time, making body sizze lognormal.

#6.Biomass per unit area
#A - Continuous - Biomass ranges with decimal values, which creates continuous data.
#B - Lognormal - Biomass is only positive with potentially a right skew or large variation.

#7.Time to germination
#A - Continuous - Time is inherently continuous data particularly if it includes minutes and seconds.
#B - Gamma - It measures the continuous waiting time until germination occurs. Gamma can include the multiple physiological steps that are part of germination.

#8.Proportion of habitat covered by vegetation
#A - Continuous - This data can include decimal values.
#B - Beta - Beta is a continuous distribution with a specific range that is ideal for bounded proportions.

#9.Larval settlement success (number settled out of larvae released)
#A - Discrete - This data are integers, as it includes counts of individual larvae.
#B - Binomial - It counts number of success out of a fixed, known number of larvae with bounds of 0 and the known number.

#10.Proportion of infected fish in a population (parasite prevalence)
#A - Continuous - This can be continuous data considering it's a proportion, which can yield decimal values.
#B - Beta - Beta is a continuous distribution ideal for proportions as it is bounded between 0 and 1.

#11.Daily temperature at a field site
#A - Continuous - Temperature is inherently continuous but particularly if it is measured to the decimal, such as 85.5º.
#B - Normal - Temperature measurements are continuous and symmetric.

#12.Population growth rate, meaning next year’s population size divided by this year’s
#A - Continuous - A growth rate includes decimals.
#B - Lognormal - Population growth rates are multiplicative over time, which makes taking the logarithm ideal.

##PART 2-----------------------------------------------------------------------

#read in dataset
lakeannie <- read.csv("https://github.com/MorganPMathews/Biometry/raw/refs/heads/main/USF_Water_Atlas_Lake_Annie.csv")

#variables of interest - Trophic State Index, True Color, pH
#histogram distribution is sufficient for each

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
              values_fn = mean)

##Trophic State Index
#histogram for Trophic State Index
library(ggplot2)
library(tidyr)
tsi_color_pH |> 
  pivot_longer(cols = c(`Trophic State Index`), 
               names_to = "Parameter",
               values_to = "Value") |> 
#build histogram
  ggplot(aes(x = Value, fill = Parameter)) +
  geom_histogram(na.rm = TRUE, bins = 20, color = "white", alpha = 0.8)+
#color and labels
  scale_fill_manual(values = c("lightgreen")) + 
  labs(title = "Distribution of Lake Annie Trophic State Index",
       subtitle = "1986-2026",
       x = "Trophic State Index (TSI)",
       y = "Count (Frequency)") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "darkgray"))

#log histogram for Trophic State Index due to skew
library(ggplot2)
library(tidyr)
#log-transform Trophic State Index and calculate mean and sd to fit a normal curve
log_tsi  <- log10(tsi_color_pH$`Trophic State Index`)
log_mean <- mean(log_tsi, na.rm = TRUE)
log_sd   <- sd(log_tsi, na.rm = TRUE)
#build log histogram
tsi_color_pH |> 
  pivot_longer(cols = c(`Trophic State Index`), 
               names_to = "Parameter",
               values_to = "Value") |> 
ggplot(aes(x = log10(Value), fill = Parameter)) +
  geom_histogram(aes(y = after_stat(density)), na.rm = TRUE, bins = 20, color = "white", alpha = 0.8) +
#add curve
  stat_function(
    fun = dnorm, 
    args = list(mean = log_mean, sd = log_sd),
    color = "darkgreen", 
    linewidth = 1.2
  ) +
#color and labels
  scale_fill_manual(values = c("lightgreen")) + 
  labs(title = "Log-Transformed Distribution of Lake Annie Trophic State Index",
       subtitle = "1986-2026",
       x = "Log10 Trophic State Index (TSI)",
       y = "Count (Frequency)") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "darkgray"))

##True Color
#histogram for True Color
tsi_color_pH |> 
  pivot_longer(cols = c(`True Color`), 
               names_to = "Parameter",
               values_to = "Value") |> 
#build histogram
  ggplot(aes(x = Value, fill = Parameter)) +
  geom_histogram(na.rm = TRUE, bins = 20, color = "white", alpha = 0.8) +
#colors and labels
  scale_fill_manual(values = c("darkblue")) + 
  labs(title = "Distribution of Lake Annie True Color",
       subtitle = "1986-2026",
       x = "True Color",
       y = "Count (Frequency)") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "darkgray"))

#log histogram for True Color due to skew
library(ggplot2)
library(tidyr)
#log-transform True Color and calculate mean and sd to fit a normal curve
log_tsi  <- log10(tsi_color_pH$`True Color`)
log_mean <- mean(log_tsi, na.rm = TRUE)
log_sd   <- sd(log_tsi, na.rm = TRUE)
#build log histogram
tsi_color_pH |> 
  pivot_longer(cols = c(`True Color`), 
               names_to = "Parameter",
               values_to = "Value") |> 
  ggplot(aes(x = log10(Value), fill = Parameter)) +
  geom_histogram(aes(y = after_stat(density)), na.rm = TRUE, bins = 20, color = "white", alpha = 0.8) +
  #add curve
  stat_function(
    fun = dnorm, 
    args = list(mean = log_mean, sd = log_sd),
    color = "darkblue", 
    linewidth = 1.2
  ) +
  #color and labels
  scale_fill_manual(values = c("lightblue")) + 
  labs(title = "Log-Transformed Distribution of Lake Annie True Color",
       subtitle = "1986-2026",
       x = "Log10 True Color",
       y = "Count (Frequency)") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "darkgray"))

##pH
#histogram for pH
tsi_color_pH |> 
  pivot_longer(cols = c(`pH`), 
               names_to = "Parameter",
               values_to = "Value") |> 
#build histogram
  ggplot(aes(x = Value, fill = Parameter)) +
  geom_histogram(na.rm = TRUE, bins = 20, color = "white", alpha = 0.8) +
#color and labels
  scale_fill_manual(values = c("lightblue")) + 
  labs(title = "Distribution of Lake Annie pH",
       subtitle = "1986-2026",
       x = "pH",
       y = "Count (Frequency)") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "darkgray"))

##variables - discrete or continuous:
#Trophic State Index - discrete (no decimals in dataset)
#True Color - discrete (no decimals in dataset)
#pH - continuous (decimal values in dataset)

##means and medians
#Trophic State Index
mean(tsi_color_pH$`Trophic State Index`, na.rm = TRUE) #mean = 43.66547
median(tsi_color_pH$`Trophic State Index`, na.rm = TRUE) #median = 41
#True Color
mean(tsi_color_pH$`True Color`, na.rm = TRUE) #mean = 15.47674
median(tsi_color_pH$`True Color`, na.rm = TRUE) #median = 15
#pH
mean(tsi_color_pH$pH, na.rm = TRUE) #mean = 7.197077
median(tsi_color_pH$pH, na.rm = TRUE) #median = 7.2975
#the mean and median for each variable are close

##distributions
#Trophic State Index follows a lognormal distribution. The mean (43.67) is greater than the median (41). These values as well as the histogram show that the distribution is right-skewed. After a log10 transformation, the log-transformed Trophic State Index fits a normal distribution as demonstrated by the overlaid curve.
#True Color follows a lognormal distribution. The mean and median are approximately equal, but the histogram shows that it is right-skewed with a long tail. After a log10 transformation, the log-transformed True Color fits a normal distribution as demonstrated by the overlaid curve.
#pH follows a normal distribution. The mean and median are close to equal, showing a symmetric distribution around a typical neutral pH of 7 that is depicted in the histogram.
