crucial = 0.165910681040351
other = 0.859347714804543
r = seq(0,crucial, .0001)
upper = function(r) (2*r**2 - 4*r + sqrt(-4*r**4 + 12*r**2 - 8*r + 1) + 3)/(4*(r**2 - 2*r + 1))
interior = function(r) (2*r**2 - 4*r - sqrt(-4*r**4 + 12*r**2 - 8*r + 1) + 3)/(4*(r**2 - 2*r + 1))
lower = function(r) (2*r**2 - 4*r - sqrt(-4*r**4 + 12*r**2 - 8*r + 1) + 1)/(4*(r**2 - 2*r + 1))


plot(r, upper(r), ylab="p",xlim=c(0,1), ylim=c(0,1), type='l', lty=2)
lines(r, interior(r), lty=1)
lines(r, lower(r), lty=2)
#segments(crucial, 0, crucial, 1, lty=3)
#segments(crucial, 0.13, crucial, 0.87, lty=3)
segments(0, 1, 1, 1)
segments(0, 0, 1, 0, lty=2)
points(0,0, pch=21, bg="white")
points(0,1, pch=21, bg="white")
points(0,.5,pch=21, bg="black")
#points(crucial, other, pch=21,bg="white")
#points(crucial,1- other, pch=21,bg="white")
#points(1,1, pch=21, bg="black")

first = seq(0,.333, .001)
second = seq(.333,1, .001)
firstline = function(a) a
secondline = function(a) a
plot(seq(0,.333,.001), rep(1, 334), ylab="p", xlab="r", xlim=c(0,1), ylim=c(0,1), type='l', lty=2)
#axis(1, at=c(0,.2,.333,.4,.6,.8, 1), labels=c("0.0", "0.2", expression(frac(c,b)), "0.4", "0.6", "0.8", "1.0"))
#axis(3, at=.333, labels=expression(r == frac(c,b)))
#segments(crucial, 0, crucial, 1, lty=3)
segments(.333, 1, 1, 1)
segments(0,0, .333, 0)
segments(.333, 0, 1,0, lty=2)
segments(.333,1,.333,0, lty=3)
points(rep(.333, 11),seq(0,1,.1), pch=21, bg="gray")
#text(.4, .7, expression(r == frac(c,b)))

first = seq(0,.8, .001)
second = seq(.8,1, .001)
f1 = function(x) 2*x**2 - 3*x + 1
f2 = function(x) x**2 - 2*x + 1
par(mar=c(5,4,4,2)+.3)
plot(first, f1(first), type='l', xlab='p', ylab=expression(dot(p)), xlim=c(0,1))
lines(second, f2(second))
segments(0, 0, 1,0, lty=2)



f1 = function(x) (2 + 2*x)/(3 + x)
plot(f1, type='l', xlim=c(0,1), ylim=c(0,1))
abline(0,1)


x = seq(0,1,length=100)
f = function(x, r) 2*x**2*(1-r)**2 - x*(2*(1-r)**2 + 1) + (1 + r**2) + x
par(mar=c(5,4,4,2)+.3)
plot(x, f(x, 0) - x, type='l', xlab='p', ylab=expression(paste(Delta,"p")))
lines(x, rep(0,length(x)), lty=2)

par(mar=c(5,4,4,2)+.3)
plot(x, f(x, 0), type='l', xlab='p', ylab="p'", ylim=c(0,1))
lines(x, x, lty=2)

plot(x, f(x, 0), type='l', xlab='p', ylab="p'", ylim=c(0,1))
lines(x, f(x,.1))
lines(x, f(x,crucial), lty=3)
lines(x, f(x,.2))
lines(x, x, lty=2)

plot(x, f(x, 0) - x, type='l', xlab='p', ylab=expression(paste(Delta,"p")))
lines(x, f(x,.1) - x)
lines(x, f(x,crucial) - x, lty=3)
lines(x, f(x,.2) - x)
lines(x, x*0, lty=2)


# x = seq(0,1,length=100)
# f1 = function(a,b,x) a*x/(a*x + b*(1-x))
# plot(x, f1(.1,.5,x), type='l', xlim=c(0,1), ylim=c(0,1), ylab="p'", xlab="p")
# abline(0,1, lty=2)
# plot(x, f1(.1,.5,x) - x, type='l', xlim=c(0,1), ylim=c(-1,1), ylab=expression(Delta), xlab="p")
# abline(0,0, lty=2)
# 
# plot(x, f1(.5,.1,x), type='l', xlim=c(0,1), ylim=c(0,1), ylab="p'", xlab="p")
# abline(0,1, lty=2)
# plot(x, f1(.5,.1,x) - x, type='l', xlim=c(0,1), ylim=c(-1,1), ylab=expression(Delta), xlab="p")
# abline(0,0, lty=2)
