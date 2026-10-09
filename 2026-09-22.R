#functions are just code someone else wrote - can type function without () for details
#make your own function example
c_to_f <- function(temp_c)
{(temp_c * 9/5) + 32}
c_to_f(c(0,37,100))

library(emdbook)
data(ReedfrogSizepred)
tadpoles <- ReedfrogSizepred
plot(tadpoles$TBL, tadpoles$Kill)

#michaelis menten function
# a stretches vertically, b horizontally
michaelis_menten <- function (x, a = 1, b = 1)
{a*x / (b+x)}
#try it out
michaelis_menten(x = 2) 
michaelis_menten(x = 2, a = 3)
#plot with curve
curve(michaelis_menten(x, a = 4, b = 5), from = 0, to = 40)
#plot it against data
plot(tadpoles$TBL, tadpoles$Kill, xlim = c(0,40), ylim = c(0,6),
     xlab = "Tadpole body length (mm)", ylab = "Number killed")
curve(michaelis_menten(x, a = 4, b = 5), add = TRUE, col = "red", lwd = 2) #add = TRUE adds to existing plot

#negative exponential
negexp <- function(x, a = 1, b = 1)
{a*exp(-b*x)}
curve(negexp(x), from = 0, to = 7)
#put michaelis menten curve on top
curve(michaelis_menten(x, a = 2, b = 1), add = TRUE, col = "red")
#change values to test plot alterations
curve(negexp(x, a = 1, b = 1), from = 0, to = 7, ylim = c(0,3))
curve(negexp(x, a = 1, b = 1), add = TRUE, col = "blue")
curve(negexp(x, a = 1, b = 1), add = TRUE, col = "lightgreen")
curve(negexp(x, a = 1, b = 1), add = TRUE, col = "turquoise")
curve(negexp(x, a = 1, b = 3), from = 0, to = 7, ylim = c(0,3))
curve(negexp(x, a = 1, b = 5), from = 0, to = 7, ylim = c(0,3))
curve(negexp(x, a = 1, b = 0.5), from = 0, to = 7, ylim = c(0,3))

#where does it fall by half?
plot(tadpoles$TBL, tadpoles$Kill, xlim = c(0,40), ylim = c(0,6),
     xlab = "Tadpole body length (mm)", ylab = "Number killed")
curve(negexp(x, a = 6, b = 0.1), add = TRUE, col = "red", lwd = 2)

#ricker function
ricker <- function(x, a = 1, b = 1)
{a*x*exp(-b*x)}
ricker(x = 2)
ricker(0, a = 1, b = 1)
ricker(c(10, 100, 1000), a = 1, b = 1)
#where is the peak?
xvec <- seq(0, 20, length.out = 10000)
yvec <- ricker(xvec, a = 1, b = 0.25)
xvec[which.max(yvec)]
#example
a_try <- 1
b_try <- 0.1
curve(ricker(x, a = a_try, b = b_try), from = 0, to = 40)
xvec <- seq(0, 40, length.out = 10000)
yvec <- ricker(xvec, a = a_try, b = b_try)
xvec[which.max(yvec)]
1/b_try #result of curve agrees with result of equation

library(emdbook)
data(ReedfrogSizepred)
tadpoles <- ReedfrogSizepred
a_try <- 0.906
b_try <- 0.0833
plot(tadpoles$TBL, tadpoles$Kill, xlim = c(0,40), ylim = c(0,6))
curve(ricker(x, a = a_try, b = b_try), add = TRUE, col = "lightpink", lwd = 2)
sum((tadpoles$Kill - ricker(tadpoles$TBL, a_try, b_try))^2)


