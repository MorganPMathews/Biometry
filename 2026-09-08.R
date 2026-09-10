avonet <- read.csv("data/Avonet/AVONET1_BirdLife.csv")
#view a histogram of mass values
hist(avonet$Mass)
#state range
range(avonet$Mass)
#make it a log scale - body size data usually needs to be log
hist(log10(avonet$Mass),breaks=40)
#two continuous variables - plot and value
plot(x=avonet$Mass,y=avonet$Wing.Length)
cor(avonet$Mass,avonet$Wing.Length,use="complete.obs")
#make it a log-log plot and get a correlation value - note the outliers
plot(x=avonet$Mass,y=avonet$Wing.Length,log="xy")
cor(log10(avonet$Mass),log10(avonet$Wing.Length),use="complete.obs")
#adjust when there are too many points
plot(x=avonet$Mass,y=avonet$Wing.Length,
     log="xy",
     pch=16,
     cex=0.4,
     col=rgb(0,0,0,alpha=0.3))
#boxplot for a categorical predictor - ~ means "as a function of" what follows
boxplot(log10(avonet$Mass)~Trophic.Level,
        data=avonet)
#another way to view this data with one categorical variable
table(avonet$Habitat)
barplot(sort(table(avonet$Habitat),
             decreasing=TRUE),
        las=2)
#checking that data migrated is correct
mean(avonet$Migration)
#remove NA values
mean(avonet$Migration, na.rm = TRUE)
#identify class
class(avonet$Migration)
#correct the class
avonet$Migration <- as.factor(avonet$Migration)

#work through 2.6 from book - starting at 2.6.2
install.packages("emdbook")
library(emdbook)
data(SeedPred)
str(SeedPred)
#attach seed removal (predation) data
attach(SeedPred)
#separate out the 10-m and 25-m transect data from the full seed removal data set
SeedPred_10 = subset(SeedPred, dist == 10)
SeedPred_25 = subset(SeedPred, dist == 25)
# split into groups and apply function
s10_means = tapply(SeedPred_10$seeds,
                   list(SeedPred_10$date, SeedPred_10$species),
                   mean, na.rm = TRUE)
s25_means = tapply(SeedPred_25$seeds,
                   list(SeedPred_25$date, SeedPred_25$species),
                   mean, na.rm = TRUE)
#matrix plot - plots all columns against x variable, plot 10-m data on log scale
matplot(s10_means, log = "y", type = "b", col = 1,
        pch = 1:8, lty = 1)
matlines(s25_means, type = "b", col = "gray", pch = 1:8,
         lty = 1)
#jittered plot
plot(jitter(SeedPred$available), jitter(SeedPred$taken))
#bubble plot
install.packages("plotrix")
library(plotrix)
sizeplot(SeedPred$available, SeedPred$taken, scale = 0.5,
         pow = 0.5, xlim = c(-2, 6), ylim = c(-2, 5))
#plot the numbers in each category
t1 = table(SeedPred$available, SeedPred$taken)
text(row(t1) - 1, col(t1) - 1, t1)
#balloonplot
install.packages("gplots")
library(gplots)
balloonplot(t1)
#mosaic plot
plot(t1)
#another way to do mosaic plot
mosaicplot(~available + taken, data = SeedPred)
#bar plot
barplot(t(log10(t1 + 1)), beside = TRUE,
        xlab = "Available", ylab = "log10(1+# observations)")
#another way to do bar plot
barplot(t(t1 + 1), log = "y", beside = TRUE,
        xlab = "Available", ylab = "1+# observations")
#bar plot of mean fraction taken