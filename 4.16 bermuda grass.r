# 4.16

require(rjags)
m1 <-
"model {
for (i in 1:nobs) {
  yield[i] ~ dgamma(nu,mu[i])
  mu[i] <- nu * eta[i]
  yhat[i] <- 1/eta[i]
  eta[i] <- beta0                 
            + beta[1]/(N[Nit[i]+1] + a[1])  
            + beta[2]/(P[Phos[i]+1] + a[2])
            + beta[3]/(K[Pot[i]+1] + a[3])}
  beta0 ~ dnorm(0,0.0001)
  nu ~ dgamma(0.01,0.01)
  a[1] ~ dnorm(40,0.01)
  a[2] ~ dnorm(22,0.01)
  a[3] ~ dnorm(32,0.01)
  for (j in 1:3) {
    beta[j] ~ dnorm(0,0.0001) I(0,)
  }
}"

jdat1 <- list(nobs=64,
              yield=c(1.98,2.13,2.19,1.97,2.38,2.24,2.1,2.6,2.18,2.56,2.22,2.47,2.22,2.47,2.94,2.48,3.88,3.91,3.66,4.07,4.35,
                4.59,4.47,4.55,4.14,4.36,4.55,4.35,4.26,4.72,4.83,4.85,4.4,4.91,5.1,5.23,
                5.01,5.64,5.68,5.6,4.77,5.69,5.8,6.07,5.17,5.45,5.85,6.43,4.43,5.31,5.15,5.87,4.95,6.27,6.49,6.54,5.22,6.27,6.35,6.72,
                5.66,6.24,7.11,7.32),
              Pot=c(0,1,2,3,0,1,2,3,0,1,2,3,0,1,2,3,0,1,2,3,0,1,2,3,0,1,2,3,0,1,2,3,
                0,1,2,3,0,1,2,3,0,1,2,3,0,1,2,3,0,1,2,3,0,1,2,3,0,1,2,3,0,1,2,3),
              Nit=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3),
              Phos=c(0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,0,0,0,0,1,1,1,1,2,2,2,2,
                3,3,3,3,0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3),
              N=c(0,100,200,400),
              P=c(0,22,44,88),
              K=c(0,42,84,168))

jinit1 <- list(beta0=0, beta=c(0.5,0.5,0.5), nu=20)
jm1 <- jags.model(textConnection(m1), data=jdat1,
                   inits=jinit1, n.chains=3)
c1 <- coda.samples(jm1, c("beta0","beta", "a"),
                   n.iter=10000)

# Results nearly identical go Congdon
print(summary(c1)$statistics[,1:2],dig=1)

# ----------------------------------------------------------------------------

# My version...

require(rjags)

m2 <- 
"model {
  for(i in 1:nobs) {
    yield[i] ~ dgamma(nu, mu[i])
    mu[i] <- nu * eta[i]
    eta[i] <- b0 + b1 / (N[i]+a1) + b2 / (P[i]+a2) + b3 / (K[i]+a3)
    yhat[i] <- 1 / eta[i]
  }

  # Hyperparameters
  nu ~ dgamma(0.01, 0.01)
  a1 ~ dnorm(40, 0.01) # Informative priors
  a2 ~ dnorm(22, 0.01)
  a3 ~ dnorm(32, 0.01)
  b0 ~ dnorm(0, 0.0001)
  b1 ~ dnorm(0, 0.0001) I(0,) # Keep b1 non-negative
  b2 ~ dnorm(0, 0.0001) I(0,)
  b3 ~ dnorm(0, 0.0001) I(0,)
}"

library(agridat)

  jdat <- with(welch.bermudagrass,
               list(yield=yield, N=n, P=p, K=k, nobs=64))
  jinit = list(a1=40, a2=22, a3=32, b0=.1, b1=10, b2=1, b3=1)

  j1 <- jags.model(textConnection(m2), data=jdat,
                   inits=jinit, n.chains=3)
  c1 <- coda.samples(j1, c("b0","b1","b2","b3", "a1","a2","a3"),
                     n.iter=10000)

  # Results nearly identical go Congdon
  print(summary(c1)$statistics[,1:2],dig=1)
  ##       Mean     SD
  ## a1  44.85  4.123
  ## a2  23.63  7.37
  ## a3  35.42  8.57
  ## b0   0.092 0.0076
  ## b1  13.23  1.34
  ## b2   1.186 0.47
  ## b3   1.50  0.48

  d2 <- coda.samples(j1, "yhat", n.iter=10000)
  dat$yhat <- summary(d2)$statistics[,1]
  with(dat, plot(yield, yield-yhat))
