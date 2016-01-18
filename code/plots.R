library(ggplot2)
library(dplyr)
library(reshape)

# Finding relation between logistic and LRP
f = function(x, a, b) x*(1-x)*((b - a)/(a*(1-x)+b*x))
fa = function(x, a, b) x*(1-x)*((b - a)/a) # upper curve
fb = function(x, a, b) x*(1-x)*((b - a)/b) # lower curve
fr = function(x, Q, B, M, v) B*Q*(Q*exp(-B*(-M + x)) + 1)**(-1/v)*exp(-B*(-M + x))/(v*(Q*exp(-B*(-M + x)) + 1))
# k*((delta - 1)*exp(k*(-x + 0.309016994374947)) + 1)**(-delta/(delta - 1))*exp(k*(-x + 0.309016994374947))
# s = function(x,a,v) a*(1-x^v)*x

curve(f(x, .1, .5), xlim=c(0,1), xlab='p', ylab=expression(dot(p)))
curve(fr(x, 1, 2, .5, 2), add=T)
curve(fr(x, 2, 2, .5, 2), add=T)




# Try this out for differen parameters
# a=.1, b=.5
par(mar=c(5,5.4,4.1,2.1))
curve(fa(x, .1, .5), xlim=c(0,1), xlab='p', ylab=expression(dot(p)))
curve(fb(x, .1, .5), add=T)
curve(f(x, .1, .5),  add=T)
text(.2, .8, expression(dot(p)[alpha]))
text(.2, .05, expression(dot(p)[beta]))
text(.2, .4, expression(dot(p)))
# a=.5,b=.1
curve(fb(x, .5, .1), xlim=c(0,1), xlab='p', ylab=expression(dot(p)))
curve(fa(x, .5, .1), xlim=c(0,1), add=T)
curve(f(x, .5, .1), xlim=c(0,1), add=T)
# Plot solution for parameters and initial solution  for NB: x=t
ga = function(x, a, b, x0) ((x0/(1-x0))*exp(x*(b - a)/a))/(1 + (x0/(1-x0))*exp(x*(b - a)/a))
gb = function(x, a, b, x0) ((x0/(1-x0))*exp(x*(b - a)/b))/(1 + (x0/(1-x0))*exp(x*(b - a)/b))
curve(ga(x, .1, .5, .01), xlim=c(0,10), xlab='t', ylab='p')
curve(gb(x, .1, .5, .01), xlim=c(0,10), add=T)
text(1, .8, expression(p[alpha]))
text(4, .05, expression(p[beta]))


curve(g(x, .1, .5), xlim=c(0,1), xlab='p', ylab=expression(dot(p)))
curve(r(), add=T)




text(.2, .8, expression(dot(p)[alpha]))
text(.2, .05, expression(dot(p)[beta]))
text(.2, .4, expression(dot(p)))


           

# Let's try discrete version
g = function(a,b,x) a*x/(a*x + b*(1-x)) - x
generate.p = function(init.p,a,b,n) {
  vector = rep(0, n)
  vector[1] = init.p
  for (i in 2:n){
    vector[i] = vector[i-1] + g(a,b,vector[i-1])
  }
  return(vector)
}

test = generate.p(.01, .5, .1, 10)
plot(test, ylim=c(0,1), xlab="Time", ylab=expression("p"["not"]), type="l", cex.lab=2)
text(3,.8, expression(frac(beta, alpha) > 1), cex=2)



ft = function(x, a, b) .5*fa(x, a, b) +  .5*fb(x,a,b)
# Let's try to find the form
curve(ft(x, .1,.5), xlim=c(0,1),add=T)
Fa = integrate(fa, lower=0, upper=1, x=x)
# http://stackoverflow.com/questions/22274275/compute-multiple-integral-and-plot-them-with-r


p = seq(0,1,.001)
f = function(a,b,x) a*x/(a*x + b*(1-x))
plot(p, f(.5, .1,p))
abline(0,1, lty=2)
g = function(a,b,x) a*x/(a*x + b*(1-x)) - x
plot(p, g(.5, .1,p), ylim=c(0,1))
abline(0,1, lty=2)
h = function(a,b,x) -a*b*log(a*b - b**2 + x*(a**2 - 2*a*b + b**2))/(a - b)**2 + a*x/(a - b) - x**2/2
plot(p, h(.5, .1,p), ylim=c(0,2))
abline(0,1, lty=2)

generate.p = function(init.p,a,b,n) {
  vector = rep(0, n)
  vector[1] = init.p
  for (i in 2:n){
    vector[i] = vector[i-1] + g(a,b,vector[i-1])
  }
  return(vector)
}

generate.q = function(init.p,a,b,n) {
  vector = rep(0, n)
  vector[1] = init.p
  for (i in 2:n){
    vector[i] = vector[i-1] + g(a,b,vector[i-1])*(i/n)
  }
  return(vector)
}
q.test = generate.q(.1, .8, .3, 10)
par(mar=c(5,5.4,4.1,2.1))
plot(q.test, ylim=c(0,1), xlab="Time", ylab=expression("p"["neg V neg"]), type="l", cex.lab=2)
text(3,.8, expression(b > frac(1,4)), cex=2)

par(mar=c(5,5,4.1,2.1))
test = generate.p(.01, .9, .2, 10)
plot(test, ylim=c(0,1), xlab="Time", ylab=expression("p"["not"]), type="l", cex.lab=2)
text(3,.8, expression(frac(beta, alpha) > 1), cex=2)

test = generate.p(.1, .2, .9, 10)
plot(test, ylim=c(0,1), xlab="Time", ylab=expression("p"["not"]), type="l", cex.lab=2)
text(3,.8, expression(frac(beta, alpha) < 1), cex=2)

x <- seq(0, 1, length=100)
hx <- dbeta(x, 1, 3)
plot(x, hx, type="l", xlab="T", ylab='Density', axes=F)
Axis(side=1, at=c(0,1))
# Axis(side=2, at=c(0,3))
library(ggplot2)
x <- seq(0, 1, len = 100)
p <- qplot(x, geom = "blank")
stat <- stat_function(aes(x = x, y = ..y..), fun = dbeta, colour="red", n = 1000,
                      args = list(shape1 = 1, shape2 = 3))
p + stat


# Drift 1
drift1.full = read.csv('drift1.csv', header=T, sep=",")
drift1 = data.frame(time = as.numeric(drift1.full$x),
                    blue = as.numeric(drift1.full$y),
                    red = as.numeric(drift1.full$y.1))
drift1 = tbl_df(drift1)
drift1 = drift1 %>% mutate(total = blue + red) %>% mutate(blue.p = blue/total) %>% mutate(red.p = red/total)
  
ggplot() + geom_line(data=drift1, aes(x=time, y=blue.p), color='blue') + 
  geom_line(data=drift1, aes(x=time, y=red.p), color='red') + ylab("Proportion") + xlab("Time") +     theme(text = element_text(size=20))


# Drift 2
drift2.full = read.csv('drift2.csv', header=T, sep=",")
drift2 = data.frame(time = as.numeric(drift2.full$x),
                    blue = as.numeric(drift2.full$y),
                    red = as.numeric(drift2.full$y.1))
drift2 = tbl_df(drift2)
drift2 = drift2 %>% mutate(total = blue + red) %>% mutate(blue.p = blue/total) %>% mutate(red.p = red/total)

ggplot() + geom_line(data=drift2, aes(x=time, y=blue.p), color='blue') + 
  geom_line(data=drift2, aes(x=time, y=red.p), color='red') + ylab("Proportion") + xlab("Time") +     theme(text = element_text(size=20))

# Drift 3
drift3.full = read.csv('drift3.csv', header=T, sep=",")
drift3 = data.frame(time = as.numeric(drift3.full$x),
                    blue = as.numeric(drift3.full$y),
                    red = as.numeric(drift3.full$y.1))
drift3 = tbl_df(drift3)
drift3 = drift3 %>% mutate(total = blue + red) %>% mutate(blue.p = blue/total) %>% mutate(red.p = red/total)

ggplot() + geom_line(data=drift3, aes(x=time, y=blue.p), color='blue') + scale_x_continuous(limits=c(0,6000),breaks=c(0, 1000, 2000, 3000, 4000, 5000, 6000)) + 
  geom_line(data=drift3, aes(x=time, y=red.p), color='red') + ylab("Proportion") + xlab("Time") +     theme(text = element_text(size=20))

# Selection 1
selection1.full = read.csv('selection1.csv', header=T, sep=",")
selection1 = data.frame(time = as.numeric(selection1.full$x),
                    blue = as.numeric(selection1.full$y),
                    red = as.numeric(selection1.full$y.1))
selection1 = tbl_df(selection1)
selection1 = selection1 %>% mutate(total = blue + red) %>% mutate(blue.p = blue/total) %>% mutate(red.p = red/total)

ggplot() + geom_line(data=selection1, aes(x=time, y=blue.p), color='blue') + scale_x_continuous(breaks=c(0, 1000, 2000, 3000, 4000, 5000, 6000)) + 
  geom_line(data=selection1, aes(x=time, y=red.p), color='red') + ylab("Proportion") + xlab("Time") +     theme(text = element_text(size=20))
# Selection 2
selection2.full = read.csv('selection2.csv', header=T, sep=",")
selection2 = data.frame(time = as.numeric(selection2.full$x),
                        blue = as.numeric(selection2.full$y),
                        red = as.numeric(selection2.full$y.1))
selection2 = tbl_df(selection2)
selection2 = selection2 %>% mutate(total = blue + red) %>% mutate(blue.p = blue/total) %>% mutate(red.p = red/total)

ggplot() + geom_line(data=selection2, aes(x=time, y=blue.p), color='blue') + 
  geom_line(data=selection2, aes(x=time, y=red.p), color='red') + ylab("Proportion") + xlab("Time") +     theme(text = element_text(size=20))

# Generate map for plotting languages involved with Jespersen's Cycle
library(ggmap)
library(mapproj)
jc.data = read.csv(file='~/Dropbox/ucsd/code/test.csv',header=T)
newmap <- getMap(resolution = "low")
plot(newmap)
points(jc.data$longitude, jc.data$latitude, col = "red", cex = .6, pch=16)
#############################################################################
library(VGAM)
# Beta-binomial distribution where alpha=1, beta=2, and n=100
t.probs = dbetabinom.ab(x=1:100, size=100, shape1=1, shape2=2)
# Cumulative distribution is roughly 10% matching starting state when t^* = 68
sum(t.probs[68:100])
# Create initial distributions
epsilon = .05 # epsilon is distributed to other strategies
s.probs = rep(epsilon/100, 100)
s.probs[68] = 1 - (99/100)*epsilon # sum(s.probs) length(s.probs)
m1.probs = rep(epsilon/100, 100)
m1.probs[84] = 1 - (99/100)*epsilon # sum(m1.probs) length(m1.probs)
m2.probs = rep(epsilon/100, 100)
m2.probs[34] = 1 - (99/100)*epsilon # sum(m2.probs) length(m2.probs)

b=.2
# Sender expected utilities
EU.s = rep(0, 100)
for (s in c(1:100)){
  print(s)
  t.star = s/100
  # sending message m1
  EU.s.m1 = NULL
  for (t in c(1:s)){
    for (a in c(1:100)){
      EU.s.m1 = append(EU.s.m1, t.probs[t]*m1.probs[a]*(1 - (t/100 - a/100 - (1-t/100)*b )**2))
    }    
  }
  # sending message m2
  EU.s.m2 = NULL  
  for (t in c(1:s)){
    for (a in c(1:100)){
      EU.s.m2 = append(EU.s.m2, t.probs[t]*m2.probs[a]*(1 - (t/100 - a/100 - (1-t/100)*b )**2))
    }    
  }
  EU.s[s] = sum(EU.s.m1) + sum(EU.s.m2)
}

plot(EU.s)

# Discrete-time Replicator equation
x,i.next = x.i*(s.i/s.bar) # proportion of x.i at next point in time is proportion times fitness divided by average fitness


library(VGAM)
# Plot distribution over coherence relations
s1 = dbetabinom.ab(x=1:100, size=100, shape1=4, shape2=2)
par(mar=c(5,5.4,4.1,2.1))
hist(s1, yaxt='n', ylab='Count', xaxt='n', xlab='Coherence relation given negation', main='', cex.lab=1.5)
#plot(s1, type='l', yaxt='n', ylab='', xaxt='n', xlab='Coherence Relation given Negation')
help(dbeta)
s2 = sort(rbeta(10000, 3, 2))
s2 = dbeta(size=100, shape1=3, shape2=2)
plot(s2, type='l', yaxt='n', ylab='', xlab='Discourse Status')
s3 = dbetabinom.ab(x=1:100, size=100, shape1=2, shape2=2)
plot(s3, type='l', yaxt='n', ylab='', xaxt='n', xlab='Discourse Status')

