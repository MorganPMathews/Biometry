#probability density function of a normal distribution - d(norm)
x_vector <- seq(from = -10, to = 10, by = 0.1)
plot(dnorm(x = x_vector, mean = 0, sd = 1) ~x_vector)
#change the mean, keep sd the same - mean moves the distribution
plot(dnorm(x = x_vector, mean = 0, sd = 1) ~ x_vector, type = "l")
plot(dnorm(x = x_vector, mean = 3, sd = 1) ~ x_vector, type = "l")
#keep the mean, change sd - sd spreads the distribution
plot(dnorm(x = x_vector, mean = 0, sd = 2) ~ x_vector, type = "l")
plot(dnorm(x = x_vector, mean = 0, sd = 4) ~ x_vector, type = "l")

#pnorm() - probability below curve
#quartile - qnorm() - q is p backwards - give it a probability and it gives you a value
#random - rnorm() - random numbers

#get random set of values from distribution
draws <- rnorm(n = 10000, mean = 10, sd = 2) #changing n changes how well curve matches distribution
#make histogram
hist(draws, freq = FALSE)
#draw curve
curve(dnorm(x, mean = 10, sd = 2), add = TRUE, col = "turquoise", lwd = 2)

#binomial distribution
k <- seq(from = 0, to = 20, by = 1)
plot(dbinom(x = k, size = 20, prob = 0.5) ~k, ylim = c(0,0.3))
#alternatives
plot(dbinom(x = k, size = 20, prob = 0.1) ~k, ylim = c(0,0.3))
plot(dbinom(x = k, size = 20, prob = 0.9) ~k, ylim = c(0,0.3))
plot(dbinom(x = k, size = 5, prob = 0.5) ~k, ylim = c(0,0.3))