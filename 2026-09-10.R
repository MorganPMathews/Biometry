avonet <- read.csv("data/Avonet/AVONET1_BirdLife.csv")
#assessing many variables at once - put name of dataset then names of columns you're interested in
beak <- avonet[c("Beak.Length_Culmen",
                 "Beak.Length_Nares",
                 "Beak.Width",
                 "Beak.Depth")]
round(cor(beak),2) #correlation between variables, round to two digits
#plot
pairs(x=beak,#variable
      pch=16,#this line and following are design elements
      cex=0.3,
      col=rgb(red=0,
              green=0,
              blue=0,
              alpha=0.15))
#filter data
pelicans <- avonet[which(
  avonet$Order1 == "Pelecaniformes"),]
nrow(pelicans) #output: 110 rows
#make a boxplot - Figure 1
boxplot(log10(Mass)~Family1,
        data=pelicans)
#make two plots side by side - Figure 2
carnivores <- avonet[which(
  avonet$Trophic.Level == "Carnivore"),] #subset carnivores
scavengers <- avonet[which(
  avonet$Trophic.Level == "Scavenger"),] #subset scavengers
par(mfrow = c(1,2)) #get plots side by side
hist(log10(carnivores$Mass),
     main = "Carnivore") #carnivore histogram
hist(log10(scavengers$Mass),
     main = "Scavenger") #scavenger histogram
#put it back to NOT side by side
par(mfrow = c(1,1))
#make a scatterplot
plot(x = log10(avonet$Mass),
     y = log10(avonet$Wing.Length),
     xlab = "Body mass (log10 g)", #title and units of x-axis
     ylab = "Wing length (log mm)", #title and units of y-axis
     main = "Wing length scales with body mass") #title
fit <- lm(log10(Wing.Length)~log10(Mass), #add line
          data = avonet)
abline(fit, col = "red", lwd = 3) #design of line
#saving a figure
png("avonet_scatterplot.png",
    width = 900, height = 650)
plot(log10(avonet$Mass),
     log10(avonet$Wing.Length))
dev.off()

