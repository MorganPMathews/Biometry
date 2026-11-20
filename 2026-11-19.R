#code and data style

library(tidyverse)
library(lintr) #checks your style
library(styler) #fixes your spacing
library(biometrydata)

avo <- avonet
nrow(avo)

lint(text = "MeanMass = mean(x, na.rm=T)") #lists the problem
style_text("MeanMass = mean(x, na.rm=T)") #fixes what it can

lint()
lint_dir()
style_file()
style_dir()

#pivot longer
avo_wide <- avo |>
  select(Species1, Mass, Wing.Length, Beak.Width, Beak.Length_Culmen)

avo_long <- avo_wide |>
  pivot_longer(cols = Mass:Beak.Length_Culmen,
               names_to = "trait",
               values_to = "trait_value")
avo_long

#plot
ggplot(avo_long, aes(x = trait_value)) +
  geom_histogram(bins = 40) +
  scale_x_log10() +
  facet_wrap(~ trait, scales = "free")

#pivot wider
avo_wide2 <- avo_long |>
  pivot_wider(id_cols = Species1, #what identifies a row
              names_from = trait, #new column names come from here
              values_from = trait_value)
all.equal(as.data.frame(avo_wide2), avo_wide)

#bobwhite, one column per year
bw_wide <- bobwhite_counts |>
  select(route, year, count) |>
pivot_wider(id_cols = route, #what identifies a row
            names_from = year, #new column names come from here
            values_from = count)
sum(is.na(bw_wide))

bw_long <- bw_wide |>
  pivot_longer(cols = !route,
               names_to = "year",
               values_to = "count")

#which bird family has the most pointed wings, on average, among families with at least 10 species?
pointiest_wings <- avo |>
#families with at least 10 species
group_by(Family1) |>
  summarise(HWI_mean = mean(Hand.Wing.Index), species_per_family = n()) |>
  filter(species_per_family)



