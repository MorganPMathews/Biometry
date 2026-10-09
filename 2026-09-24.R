#setup
library(emdbook)
data(ReedfrogSizepred)
data(ReedfrogFuncresp)
tadpoles <- ReedfrogSizepred
kills_by_density <- ReedfrogFuncresp

ricker <- function(x, a = 1, b = 1)
{a*x*exp(-b*x)}

michaelis_menten <- function(x, a = 1, b = 1)
{a*x/(b+x)}

##############################################

a_try <- 69 #asymptote
b_try <- 142 #half-saturation point

plot(kills_by_density$Initial, kills_by_density$Killed,
     xlim = c(0,100), ylim = c(0,40))
curve(michaelis_menten(x, a = a_try, b_try), add = TRUE, col = "turquoise", lwd = 2)

sum((kills_by_density$Killed -
       michaelis_menten(kills_by_density$Initial, a_try, b_try))^2)

##############################################

#change fit of ricker
power_ricker <- function(x, height, peak, alpha)
{height*(x/peak*exp(1-x/peak))^alpha}

alpha_try <- 37

plot(tadpoles$TBL, tadpoles$Kill, xlim = c(0,40), ylim = c(0,6))
curve(power_ricker(x, height = 4, peak = 12, alpha = alpha_try),
      add = TRUE, col = "turquoise", lwd = 2)

sum((tadpoles$Kill - power_ricker(tadpoles$TBL, 4, 12, alpha_try))^2)

##############################################

#ifelse()
hockey_stick <- function(x, a, s)
{ifelse(x < s, a*x, a*s)}

##############################################

#threshold
threshold <- function(x, a1, a2, s)
{ifelse(x < s, a1, a2)}
curve(threshold(x, a1 = 1, a2 = 3, s = 5), from = 0, to = 10)

##############################################

#calculus - derivative
d_ricker <- D(expression(a*x*exp(-b*x)), "x")
d_ricker

eval(d_ricker, list(a = 1, b = 0.25, x =1/0.25))

eval(d_ricker, list(a = 2, b = 0.25, x = 0))

##############################################
#textbook practice - 3.6

#plot the logistic and its derivative
logist = expression(exp(x)/(1 + exp(x)))
dfun = deriv(logist, "x", function.arg = TRUE)
xvec = seq(-4, 4, length = 40)
y = dfun(xvec)
plot(xvec, y)
lines(xvec, attr(y, "grad"))

#piecewise
curve(ifelse(x < 5, 1 + x, 6 - 3 * (x - 5)), from = 0, to = 10, col = "turquoise")

curve(ifelse(x < 5, 1 + x, ifelse(x < 8, 6 - 3 * (x - 5), -3 + 2 * (x - 8))), from = 0, to = 10, col = "turquoise")

#use eval to fill in parameters
d1 = D(expression(a * x/(b + x)), "x")
eval(d1, list(a = 2, b = 1, x = 3))








