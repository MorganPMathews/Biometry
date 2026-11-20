#practice with lint using Addins
Hawks <- avo |>
  filter(Family1 == "Accipitridae") 
mean_mass <- mean(Hawks$Mass, na.rm = TRUE)
habitatsummary <- Hawks |>
  group_by(Habitat) |>
  summarise(m = mean(Mass), w = mean(Wing.Length))
