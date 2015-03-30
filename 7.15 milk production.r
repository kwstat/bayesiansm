# 7.15.r
# Time-stamp: c:/x/stat/books/congdon/7.15.r

require(rjags)

jdat <- list(y=c(117, 118.6, 120, 115.5, 115.6, 115.4, 120.2, 122.7, 121.5, 123.4, 128.5, 130, 135.8),
             x=c(12, 11.8, 11.7, 11.4, 11.2, 11.1, 11, 11, 10.8, 10.7, 10.8, 10.9, 11))
with(jdat, plot(y~x, xlab="Million cows", ylab="Milk production"))

m1 <-
"model  {
# fixed precisions, as in Harrison and West, Ch 3
W <- 0.05
W.inv <- 1/W
tau.y  <- 1

# observation model 
for (t in 1:13){
  y[t] ~ dnorm(mu[t], tau.y)
  mu[t] <- x[t] * beta[t]
}

# state model, settings for year 1
beta[1] ~ dnorm(10, 0.01)
for (t in 2:13){
  beta[t]  ~ dnorm(beta[t-1], W.inv)
}

# one-step ahead forecasts
for (t in 1:13){
  beta.new[t]  ~ dnorm(beta[t], W.inv)
}

# Problem.  I had this error message:
# Failed to set trace monitor for mu.new
# Solution is to fully define mu.new by setting the first value to
# any value.  Then mu.new can be monitored.
# See: http://sourceforge.net/p/mcmc-jags/discussion/610037/thread/ac5faa5d/
mu.new[1] <- 0

for (t in 2:13) {
  mu.new[t] <- beta.new[t-1] * x[t]
  y.new[t] ~ dnorm(mu.new[t], tau.y)
}

}"

jinit <- list(beta=rep(10, 13))

jm1 <- jags.model(textConnection(m1),
                  data=jdat, inits=jinit, n.chains=3)

c1 <- coda.samples(jm1, c("beta", "mu.new"),
                   n.iter=10000)

#summary(c1)

library(lucid)
lucid(c1) # Match to Congdon, p 313
