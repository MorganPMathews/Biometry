2*8
sqrt(25)
x <- sqrt(36)
x
?sqrt
example(sqrt)
help.search("correlation")
#below demonstrates noise and signal
frogs <- c(1.1, 1.3, 1.7, 1.8, 1.9, 2.1, 2.3, 2.4, 2.5, 2.8, 3.1, 3.3, 3.6, 3.7, 3.9, 4.1, 4.5, 4.8, 5.1, 5.3)
set.seed(101)
tadpoles <- rnorm(n = 20,
                  mean = 2 * frogs,
                  sd = 0.5)
plot(x = frogs, y = tadpoles)
abline (a = 0, b = 2)
tadpoles_noisy <- rnorm(20, 2 * frogs, sd = 3)
plot (frogs, tadpoles_noisy)
abline(a = 0, b = 2)
#nested function below - linear model of tadpoles against frogs - coefficient gives coefficient of relationships
coef(lm(tadpoles ~ frogs))
coef(lm(tadpoles_noisy ~ frogs))
confint(lm(tadpoles ~ frogs))
#replicate the simulation 1000 times
slopes_clean <- replicate (1000,
coef(lm(rnorm(20, 2 * frogs, sd = 0.5) ~ frogs))[2])

slopes_noisy <- replicate (1000,
coef(lm(rnorm(20, 2 * frogs, sd = 3) ~ frogs))[2])
slopes_clean
#get summary values - shows that when noise is higher we need more data
mean(slopes_clean)
mean(slopes_noisy)
sd(slopes_clean)
sd(slopes_noisy)
#summarize and test
mean (tadpoles)
summary (tadpoles)
cor(frogs, tadpoles)
cor.test(frogs, tadpoles)
#run it again, different random draw - simulates demographic stochasticity
set.seed(202)
tadpoles2 <- rnorm(20, 2 * frogs, sd = 0.5)
cor(frogs, tadpoles2)