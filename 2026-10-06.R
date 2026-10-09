#deterministic - y = a + bx
a <- 2
b <- 1
x <- 1:20

y_det <- a + b * x
plot(x = x, y = y_det)

#add noise - make stochastic version
y_stoch <- rnorm(n = 20, mean = y_det, sd = 2) #each point has its own mean
plot(x = x, y = y_stoch) #running this several times produces slightly different outcome

#add standard deviation multiplier - sd varies with mean, higher values have more variation
sd_multiplier <- 0.3

y_stoch <- rnorm(n = 20, mean = y_det, sd = y_det * sd_multiplier)
plot(x = x, y = y_stoch)
points(x, y_det, col = "blue")

#shows how high sd can make it difficult to detect pattern
a <- 2
b <- 1
x <- 1:20

y_det <- a + b * x

y_stoch <- rnorm(n = 20, mean = y_det, sd = 9)
plot(x = x, y = y_stoch)
abline(a = a, b = b, col = "red")

#add negative binomial noise
a <- 20
b <- 1
k <- 5
x <- runif(50, min = 0, max = 5)

y_det <- a * b / (b + x)
y <- rnbinom(n = 50, mu = y_det, size = k)
plot(x = x, y = y)

#altering parameters - tried different values of k, a, b
a <- 50
b <- 2
k <- 100 #controls spread
x <- runif(50, min = 0, max = 5)

y_det <- a * b / (b + x)
y <- rnbinom(n = 50, mu = y_det, size = k)

plot(x = x, y = y, ylim = c(0,60))
points(x, y_det, col = "blue", pch = 16)

#groups - make a and b vectors, one value per group
g <- factor(rep(1:2, each = 25))

a <- c(20, 10)
b <- c(1,2)

#indexing by group
x <- runif(50, min = 0, max = 5)

y_det <- a[g] * b[g] / (b[g] + x)

plot(x = x, y = y_det, col = g, pch = 16)

#add the noise
k <- 5

y <- rnbinom(n = 50, mu = y_det, size = k)

plot(x = x, y = y, col = g, pch = as.numeric(g))
legend("topright", bty = "n", col = 1:2, pch = 1:2,
       legend = c("species 1", "species 2"))

#changing parameters
a <- c(50, 2) #changing these
b <- c(1,2)
k <- 5

y_det <- a[g] * b[g] / (b[g] + x)
y <- rnbinom(n = 50, mu = y_det, size = k)
plot(x = x, y = y, col = g, pch = as.numeric(g))

#building a simulation - need model, distribution, parameters
#simulate an experiment - red = fake experiment, open circles = real one
library(emdbook)
data(ReedfrogFuncresp)
frogs <- ReedfrogFuncresp

p <- (60 * frogs$Initial / (100 + frogs$Initial)) / frogs$Initial

killed_sim <- rbinom(n = nrow(frogs), size = frogs$Initial, prob = p)

plot(frogs$Initial, killed_sim, pch = 16, col = "red",
     xlab = "tadpoles in the tank", ylab = "number killed")
points(frogs$Initial, frogs$Killed, pch = 1, cex = 1.4)

#Brian needs to update slides bc he didn't load in data in examples
#a poisson has mean = variance, so ratio should be about 1
#match a negative binomial, then simulate from it
#compare fake routes and real ones
#you don't always need a curve

#all examples show that you build data your model predicts, compare it to what you actually got, and repeat it because one dataset is one draw



