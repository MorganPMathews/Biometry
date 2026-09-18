#grouping continued
library(tidyverse)
avo <- read_rds("https://tinyurl.com/avonetbirddata")
avo_w_family_mass |>
  summarise(total_species = n()) #this still groups it by family
#fix grouping (ungroup) to get one number of total species
avo_w_family_mass |>
ungroup() |>
  summarise(total_species = n())
#slice functions - ex. get largest bird species in each family
biggest_in_fams <- avo |>
  group_by(Family1) |>
  slice_max(Mass, n=1) |>
  select(Family1, Species1, Mass)

#ggplot2
#"gg" = "grammar of graphics"
library(tidyverse)
library(ggridges)
avo <- read_rds("https://tinyurl.com/avonetbirddata")
#histogram
avo |>
  ggplot(mapping = aes(x = Hand.Wing.Index)) +
  geom_histogram()
#distribution
avo|>
  ggplot(mapping = aes(x = Hand.Wing.Index)) +
  geom_density() #can click tab after typing geom to see your options
#scatterplots
avo |>
  ggplot(mapping = aes(x = Hand.Wing.Index,
                       y = Kipps.Distance)) +
  geom_point()
avo |>
  ggplot(mapping = aes(x = Hand.Wing.Index,
                       y = Beak.Length_Culmen)) +
  geom_point()
#geom_smooth() to add a line
avo |>
  ggplot(mapping = aes(x = Hand.Wing.Index,
                       y = Beak.Width)) +
  geom_point() +
  geom_smooth()
#trying geom_line()
avo |>
  ggplot(mapping = aes(x = Hand.Wing.Index,
                       y = Kipps.Distance)) +
  geom_line()
#better geom_line()
avo |>
  mutate(rounded_hwi = round(Hand.Wing.Index)) |>
  group_by(rounded_hwi) |>
  summarise(mean_kipps = mean(Kipps.Distance)) |>
  ggplot(mapping = aes(x = rounded_hwi,
                       y = mean_kipps)) +
  geom_line()
#geom_abline()
kipps_v_hwi_model <- avo |>
  lm(formula = Kipps.Distance ~ Hand.Wing.Index)
#geom_smooth()
ggplot(data = avo,
       mapping = aes(x = Hand.Wing.Index,
                     y = Kipps.Distance)) +
  geom_point () +
  geom_smooth(method = "lm")
#geom_bar()
avo |>
  group_by(Order1) |>
  summarise(mean_mass = mean(Mass)) |>
  slice_max(n = 5, order_by = mean_mass) |>
  ggplot(mapping = aes(y = mean_mass, x = Order1)) +
  geom_bar(stat = "identity")

avo |> 
  filter(Order1 %in% c("Struthioniformes",
                       "Cathartiformes",
                       "Gaviiformes",
                       "Sphenisciformes",
                       "Ciconiiformes")) |>
  ggplot(mapping = aes(x = Order1)) +
  geom_bar()

avo |> 
  filter(Order1 %in% c("Struthioniformes",
                       "Cathartiformes",
                       "Gaviiformes",
                       "Sphenisciformes",
                       "Ciconiiformes")) |>
  ggplot(mapping = aes(x = Order1,
                       y = Wing.Length)) +
  geom_boxplot()

#geom_violin - width, indicating that there is more data at those values
avo |> 
  filter(Order1 %in% c("Struthioniformes",
                       "Cathartiformes",
                       "Gaviiformes",
                       "Sphenisciformes",
                       "Ciconiiformes")) |>
  ggplot(mapping = aes(x = Order1,
                       y = Wing.Length)) +
  geom_violin()

#continuous x, categorical y - flips x and y axis presentation
avo |>
  group_by(Order1) |>
  summarise(mean_mass = mean(Mass)) |>
  slice_max(n = 5, order_by = mean_mass) |>
  ggplot(mapping = aes(y = mean_mass, x = Order1)) +
  geom_bar(stat = "identity") +
  coord_flip()

#geom_density_ridges
install.packages("ggridges")
avo |> 
  group_by(Order1) |>
  mutate(mean_mass = mean(Mass)) |>
  filter(Order1 %in% c("Struthioniformes",
                       "Cathartiformes",
                       "Gaviiformes",
                       "Sphenisciformes",
                       "Ciconiiformes")) |>
  ggplot(mapping = aes(x = Wing.Length,
                       y = Order1,
                       group = Order1)) +
  geom_density_ridges()

#geom_count
avo |>
  filter(Family1 %in% c("Psittacidae", "Cacatuidae")) |>
  ggplot(mapping = aes(x = Family1, y = Habitat)) +
  geom_count()

#geom_tile - can be good for looking at correlations
avo |>
  group_by(Family1, Trophic.Level) |>
  mutate(n_species = n(),
         log10_species = n() |> log10()) |>
  filter(Family1 %in% c("Psittacidae", "Cacatuidae")) |>
  ggplot(mapping = aes(x = Family1,
                       y = Trophic.Level,
                       fill = log10_species)) +
  geom_tile()

#color and fill
avo |>
  filter(Family1 %in% c("Psittacidae", "Cacatuidae")) |>
  group_by(Family1, Habitat) |>
  summarise(mean_mass = mean(Mass)) |>
  slice_max(n = 5, order_by = mean_mass) |>
  ggplot(mapping = aes(y = mean_mass,
                       x = Family1, fill = Habitat)) +
  geom_bar(stat = "identity", position = "dodge")

#facet_wrap() - separates plots
avo |>
  filter(Family1 %in% c("Psittacidae", "Cacatuidae")) |>
  ggplot(mapping = aes(x = Mass,
                       y = Wing.Length)) +
  geom_point() +
  facet_wrap(~Family1)

avo |>
  filter(Family1 %in% c("Psittacidae", "Cacatuidae")) |>
  ggplot(mapping = aes(x = Mass,
                       y = Wing.Length)) +
  geom_point() +
  facet_grid(Family1 ~ Trophic.Niche)

#label figure
avo |>
  ggplot(mapping = aes(x = Hand.Wing.Index,
                       y = Kipps.Distance)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm") +
  labs(title = "Kipp's distance rises with hand-wing index",
       x = "Hand-wing index",
       y = "Kipp's distance (mm)",
       caption = "Data: AVONET") +
  theme_bw()
#save figure
my_plot <- avo |>
  ggplot(mapping = aes(x = Mass, y = Wing.Length)) +
  geom_point()
ggsave(filename = "my_figure.png",
       plot = my_plot,
       width = 8,
       height = 5,
       dpi = 300)
