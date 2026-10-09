#functions to evaluate distributions
#mean()
#median()
#var() - variance
#sd() - standard deviation

library(readr)
avonet <- read_rds("https://tinyurl.com/avonetbirddata")
hist(avonet$Hand.Wing.Index)
mean(avonet$Hand.Wing.Index)
median(avonet$Hand.Wing.Index)
hist(log10(avonet$Mass))
mean(avonet$Mass)
median(avonet$Mass)

#desctools install to do Mode
install.packages("DescTools")
library(DescTools)
Mode(avonet$Mass)
var(avonet$Mass)
sd(avonet$Mass)
round(var(avonet$Mass)) == round((sd(avonet$Mass)^2))
mean(avonet$Range.Size, na.rm = TRUE)
var(avonet$Range.Size, na.rm = TRUE)
