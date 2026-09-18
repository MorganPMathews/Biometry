install.packages("tidyverse")
library(tidyverse)
avo <- read_rds("https://tinyurl.com/avonetbirddata")
#base R
parrot_fam1 <- avo[which(avo$Family1 == "Psittacidae"),]
#dyplr
parrot_fam2 <- avo|>
  filter(Family1 == "Psittacidae")

#filtering on multiple columns using &
#base R
forest_parrots1 <- avo[which(avo$Family1 == "Psittacidae" &
                               avo$Habitat == "Forest"),]
#dyplr
forest_parrots2 <- avo |>
filter(Family1 == "Psittacidae" & Habitat == "Forest")
#dyplr but sequential
forest_parrots3 <- avo |>
filter(Family1 == "Psittacidae") |>
filter(Habitat == "Forest")     

#practice: filter dataset by an order and a trophic level
birds_1 <- avo |>
filter(Order1 == "Accipitriformes" & Trophic.Level == "Carnivore")

#filtering on multiple columns
parrots_or_cockatoos <- avo |>
filter(Family1 == "Psittacidae" | 
         Family1 == "Cacatuidae")
#using %in% and a vector
parrots_or_cockatoos <- avo |> 
  filter(Family1%in% c("Psittacidae","Cacatuidae"))
#filtering by combining &, |, and ()
fp_or_wc <- avo |>
  filter((Family1 == "Psittacidae" &
            Habitat == "Forest") |
          (Family1 == "Cacatuidae" &
            Habitat == "Woodland"))
#filter using |
birds2 <- avo |>
  filter((Trophic.Level == "Carnivore" & Habitat == "Forest") &
           (Family1 == "Accipitridae" | Family1 == "Falconidae"))

#arrange() to sort by a column
avo_by_mass <- avo |>
  arrange(Mass)
avo_by_mass_dec <- avo |>
  arrange(-Mass)
avo_by_mass_dec2 <- avo |>
  arrange(desc(Mass))
#arrange by wing length
longest_wing_length <- avo |>
  arrange(-Wing.Length)
head(longest_wing_length$Species1, 1)

#distinct() to remove duplicate rows, can be used to review all fields
avo_no_dupes <- avo |>
  distinct()
#can also be used on a particular field
avo_fams <- avo |>
  distinct(Family1)
#can be used for combination of fields
family_trophic_levels <- avo |>
  distinct(Family1, Trophic.Level)

#mutate () to create new variables or modify existing ones
avo_w_beak_diff <- avo |>
  mutate(beak_length_difference =
           Beak.Length_Culmen - Beak.Length_Nares) |>
  arrange(-beak_length_difference)
  head(avo_w_beak_diff$Species1, 1)

#select () to select fields to keep (or remove)
#keep only family, species, mass
fsm <- avo |>
  select(Family1, Species1, Mass)
#remove range size
no_rangesize <-
  avo |> select(!Range.Size)
#can also use ":" to select multiple fields
avo_tax <- avo |>
  select(Species1:Order1) #select all fields including and from Species1 to Order1

#rename() to change column names
#rename columns to be species, family, order
avo_tax1 <- avo |>
  rename(Species = Species1,
         Family = Family1,
         Order = Order1)
#can do this during a select() as well
avo_tax2 <- avo |>
select(Species = Species1,
       Family = Family1,
       Order = Order1,
       Mass)

#relocate() to have important variables on the left-most columns
avo_relocated <- avo |>
  relocate(Order1, Family1, Species1, Mass, Wing.Length)

#group_by() to group rows together, good for summary statistics
avo_w_family_mass <- avo |>
  group_by(Family1) |>
  mutate(fam_means_mass = mean(Mass))
#summarize() if we only want data at the family level
family_mass <- avo |>
  group_by(Family1) |>
  summarise(Mean_Mass = mean(Mass))

#more than one statistic
avo |>
  group_by(Trophic.Level) |>
  summarise(n = n(),
            mean_mass = mean(Mass),
            median_mass = median(Mass))
#error example - need to omit NA's
avo |>
  group_by(Trophic.Level) |>
  summarise(mean_range = mean(Range.Size, na.rm = TRUE))
  




  



