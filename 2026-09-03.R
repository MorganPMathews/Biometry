#different ways to pull data from online (both from my github page)
avonet <- read.csv("AVONET1_BirdLife.csv")

avonet_v3 <-
read.csv("https://raw.githubusercontent.com/MorganPMathews/Biometry/refs/heads/main/AVONET1_BirdLife.csv")

identical(avonet, avonet_v3)
#shows that you CANNOT have incombatible commands and file types
amphibians <- read.csv(
"oo_32985.xlsx")
#make R able to read Excel (only needs to be done once)
install.packages("readxl")
#try again but ERROR - installed does not mean available
read_xlsx("oo_32985.xlsx")
#try again but with library
library(readxl)
read_xlsx("oo_32985.xlsx")
#get first few rows to view data
amphibians <-
read_xlsx("oo_32985.xlsx")
head(amphibians)
#tell R where to start
amphibians <- read_xlsx(
"oo_32985.xlsx",
skip=3,
sheet=1,
na="DD")
head(amphibians)
#data types in R
class("something")
class(1)
class(1L)
class(TRUE)
#summary stats - ERROR because NA strings are stored as -999
amniotes <- read.csv("Amniote_Database_Aug_2015.csv")
mean(amniotes$litter_or_clutch_size_n)
mean(amniotes$longevity_y)
#fix
amniotes <- read.csv("Amniote_Database_Aug_2015.csv",
na.strings = -999)
mean(amniotes$litter_or_clutch_size_n, na.rm = TRUE)
#check class
class("AVONET1_BirdLife.csv")
#assessing data within an object
avonet[1]
avonet[[1]]
#find the names
names(avonet)
#assess species information
avonet$Species1
avonet[1]
avonet[,1]
#assess information for particular row
avonet[1,]
avonet["1",]
#assess class 
class(avonet$Species1)
class(avonet[,2])
#check class on all fields (structure)
str(avonet)
#other quick ways to check objects
summary(avonet)
head(avonet)
rownames(avonet)
colnames(avonet)
table() #used for categorical variables, shows how often combinations occur
#convert to different data types using "as" commands
as.factor()
as.character()
as.data.frame()
as.matrix()
#NA - multiple ways to deal with NAs
na.omit