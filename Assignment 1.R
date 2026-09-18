#description: Lake Annie water quality data from the USF Water Atlas that combines county, SWFWMD, FDEP, USGS, and LAKEWATCH citizen science data. Data is from 1971 to the present.
#source: https://polk.wateratlas.usf.edu/waterbodies/lakes/160723/

#read in dataset
lakeannie <- read.csv("https://github.com/MorganPMathews/Biometry/raw/refs/heads/main/USF_Water_Atlas_Lake_Annie.csv")
#identify class of dataset
class(lakeannie) #data.frame
#variables of interest and classes
class(lakeannie$Characteristic) #character
class(lakeannie$ResultValue) #numeric
class(lakeannie$SampleDate) #date

#examine structure and correct formatting
#filter by water quality parameters of interest - trophic state index, true color, pH
install.packages("tidyverse")
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

#summary statistics
summary(tsi_color_pH[c("Trophic State Index", "True Color", "pH")])

#3 figures
#figure 1 - Changes in Lake Annie's Trophic State Index (TSI) over time, from 1986 to 2026.
#make scatterplot
ggplot(data = tsi_color_pH, aes(x = Year, y = `Trophic State Index`)) +
#color and labels
  geom_point(color = "darkgreen", size = 2.5, alpha = 0.7, na.rm = TRUE) +
  geom_smooth(method = "lm", color = "black", linewidth = 0.8, se = FALSE, na.rm = TRUE) +
  labs(title = "Changes in Lake Annie's Trophic State Index (TSI) Over Time",
       subtitle = "1986-2026",
       x = "Year",
       y = "Trophic State Index") +
#theme
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, color = "gray50"))

#figure 2 - Historical water color variation of Lake Annie since 1986.
#horizontal boxplot for True Color grouped by 5-year intervals
tsi_color_pH |> 
  ggplot(aes(y = factor(Year - (Year %% 5)),
             x = `True Color`, 
             fill = factor(Year - (Year %% 5)))) +
#add boxplot
  geom_boxplot(na.rm = TRUE, alpha = 0.7) +
#apply color palette
scale_fill_viridis_d(option = "mako") + 
#add labels
labs(title = "Historical Water Color of Lake Annie (Since 1986)",
     subtitle = "Grouped by 5-year intervals",
     y = "5-Year Period Starting Year",
     x = "True Color (PCU)") +
#theme
theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "darkgray"))
panel.grid.minor = element_blank()

#figure 3 - Distributions of the pH, Trophic State Index, and True Color in Lake Annie over 40 years (1986-2026).
tsi_color_pH |> 
#pivot data into long format so ggplot can split them into panels
pivot_longer(cols = c(`Trophic State Index`, `True Color`, pH), 
             names_to = "Parameter",
             values_to = "Value") |> 
#build histogram
ggplot(aes(x = Value, fill = Parameter)) +
geom_histogram(na.rm = TRUE, bins = 20, color = "white", alpha = 0.8) +
#split into 3 separate histograms with their own axis scales
facet_wrap(~Parameter, scales = "free") + 
#color and labels
scale_fill_manual(values = c("navy", "steelblue", "lightblue")) +
  labs(title = "Distributions of Lake Annie Water Quality Parameters",
       subtitle = "1986-2026",
       x = "Measured Value",
       y = "Count (Frequency)") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "darkgray"))
