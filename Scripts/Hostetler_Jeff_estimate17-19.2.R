###############################################################################
# Model aerial survey data from POI, mitigation feature, etc. from 2017-2019
# Bayesian version in JAGS
# New to v1: include all UAS surveys so far, eliminate 2016 airplane survey
# New to v2: just load most of the stuff in already run in v1, also include 
# plotting graphs (formerly separate)
###############################################################################
library(jagsUI)

# Settings
options(max.print = 17000)
visits <- c('2017-1-31', '2017-2-1', '2018-1-7', '2019-1-31')
nVisits <- length(visits)
sites <- c(paste("Canal", c(1:12)), "Mit Pools", "Wooten's", "Big Cypress Viewing",
           "Big Cypress Lollipop")
nSites <- length(sites)
nSitesAllYears <- nSites - 2
sitesAllYears <- sites[1:nSitesAllYears]
nReps <- c(2, 1, rep(2, 4), 0, rep(2, 5), 0, rep(2, 5))
# nPassesBySite <- matrix(rep(c(10, 5, 10, 5, 6, 10, 5, 10, 4, 6, 7, 10, 6),
#                             c(4, 2, 2, 2, 1, 6, 3, 1, 1, 2, 2, 1, 1)),
#                         nSites, nVisits, dimnames = list(sites, visits))
nPasses <- 10 #max(nPassesBySite)
# models <- c('M0', 'Mt')
# nModels <- length(models)

#yMax <- 47
# Augment data set by x potential individuals per site
#nz <- 53
nUpper <- 100
# 
# Initial values
inits.p0 <- function() {
  z <- array(1, c(nSites, nUpper))
  return(list(z = z, p = rbeta(1, 1, 1)))
}
inits.p.rand <- function() {
  z <- array(1, c(nSites, nUpper))
  return(list(z = z, mean.p = rbeta(1, 1, 1), sd = runif(1, 0, 5)))
}
inits.p0.2 <- function() {
  z <- array(1, c(nSitesCMR.2nd, nUpper))
  return(list(z = z, p = rbeta(1, 1, 1)))
}
inits.p0.3 <- function() {
  z <- array(1, c(max(nSitesCMR.open), nVisits, nUpper))
  return(list(z = z, p = rbeta(1, 1, 1)))
}
inits.p.rand3 <- function() {
  z <- array(1, c(max(nSitesCMR.open), nVisits, nUpper))
  return(list(z = z, mean.p = rbeta(1, 1, 1), sd = runif(1, 0, 5)))
}
inits.p.rand.1visit <- function() {
  z <- array(1, c(nSites, nUpper))
  return(list(z = z, mean.p = rbeta(1, 1, 1), sd = runif(1, 0, 5)))
}
inits.p.bb <- function() {
  z <- array(1, c(nSites, nVisits, nUpper))
  Ncount0 <- array(NA, c(nSites, nVisits))
  for (visit in 1:nVisits) {
    for (site in 1:nSites)
      Ncount0[site, visit] <- runif(1, M[site, visit], nUpper)
  }
  return(list(z = z, Ncount0 = Ncount0, mean.p = rbeta(nVisits, 1, 1),
              rho = runif(nVisits, 0, 0.4)))
}
inits.p.bb.1visit <- function(M) {
  z <- array(1, c(nSites, nUpper))
  return(list(z = z, mean.p = rbeta(1, 1, 1),
              l.rho = qlogis(runif(1, 0.15, 0.6))))
}
inits.p.bb.v3 <- function() {
  z <- array(1, c(nSites, nVisits, nUpper))
  Ncount0 <- array(NA, c(nSites, nVisits))
  for (visit in 1:nVisits) {
    for (site in 1:nSites)
      Ncount0[site, visit] <- runif(1, M[site, visit], nUpper)
  }
  return(list(z = z, Ncount0 = Ncount0, alpha = pmax(rgamma(nVisits, 1, 1), 0.5),
             beta = pmax(rgamma(nVisits, 1, 1), 0.5)))
}

inits.p.rand.v4 <- function() {
  z <- array(1, c(nSites, nVisits, nUpper))
  Ncount0 <- array(NA, c(nSites, nVisits))
  for (visit in 1:nVisits) {
    for (site in 1:nSites)
      Ncount0[site, visit] <- runif(1, M[site, visit], nUpper)
  }
  return(list(z = z, Ncount0 = Ncount0, mean.p = rbeta(nVisits, 1, 1), sd.lp = runif(nVisits, 0, 3)))
}

inits.p.visit.v4 <- function() {
  z <- array(1, c(nSites, nVisits, nUpper))
  Ncount0 <- array(NA, c(nSites, nVisits))
  for (visit in 1:nVisits) {
    for (site in 1:nSites)
      Ncount0[site, visit] <- runif(1, M[site, visit], nUpper)
  }
  return(list(z = z, Ncount0 = Ncount0, mean.p = rbeta(nVisits, 1, 1)))
}

inits.p.bb.v5 <- function() {
  z <- array(1, c(nSites, nVisits, nUpper))
  return(list(z = z, alpha = pmax(rgamma(nVisits, 1, 1), 0.5),
             beta = pmax(rgamma(nVisits, 1, 1), 0.5)))
}

inits.p.visit.v5 <- function() {
  z <- array(1, c(nSites, nVisits, nUpper))
  return(list(z = z, mean.p = rbeta(nVisits, 1, 1)))
}

inits.p.rand.v5 <- function() {
  z <- array(1, c(nSites, nVisits, nUpper))
  return(list(z = z, mean.p = rbeta(nVisits, 1, 1), sd.lp = runif(nVisits, 0, 3)))
}


# Parameters monitored
params.p0 <- c("N", "p", "Ntotal") #"NsiteDiff", "NtimeDiff", , 'lambdaTotal'
params.p.rand <- c("N", "Ntotal", "Ndiff", 'lambdaTotal', "mean.p", "sd.lp", 'p', 'p.star')#"p", "NsiteDiff", "NtimeDiff"
params.p.bb <- c("N", "mean.p", "rho", "p", "Ntotal", "Ndiff", 'lambdaTotal')#, 'lambda',"NsiteDiff", "NtimeDiff",
params.p.bb.1visit <- c("N", "mean.p", "rho", "p", "Ntotal", 'p.star')#"NsiteDiff", "NtimeDiff",
params.p.bb.v4 <- c("N", "Ntotal", "Ndiff", 'lambdaTotal', "mean.p", "rho", "p", "p.star")#, 'lambda',"NsiteDiff", "NtimeDiff",
params.p.bb.v5 <- c("N", "Ntotal", "Ndiff", 'lambdaTotal', "mean.p", "rho", "p", "p.star")#, 'lambda',"NsiteDiff", "NtimeDiff",
params.p.visit.v4 <- c("N", "Ntotal", "Ndiff", 'lambdaTotal', "mean.p", "p.star")

# MCMC settings
ni <- 60000
nt <- 1
nb <- 20000
nc <- 3


# Specify model in BUGS language
sink("model.p0.1visit.sameM.jags")
cat("
model {
  # Priors
  p ~ dbeta(1, 1)
  #for (visit in 1:nVisits) {
  for (site in 1:nSitesCMR) {
    omega[site] ~ dbeta(1, 1)
      
    # Likelihood
    for (i in 1:M){
      z[site,i] ~ dbern(omega[site])			# Inclusion indicators
        for (j in 1:nPasses[site]){
          yaug[site,i,j] ~ dbern(p.eff[site,i,j])
          p.eff[site,i,j] <- z[site,i] * p		# Can only be detected if z=1
        } #j

    } #i
    # Derived quantities
    N[site] <- sum(z[site,1:M])
  } #site
  Ntotal <- sum(N[1:nSitesCMR])
#   lambdaTotal <- Ntotal[2:nVisits]/Ntotal[1:(nVisits-1)]
}
",fill = TRUE)
sink()

sink("model.p.rand.1visit.sameM.jags")
cat("
model {
  # Priors
  mean.p ~ dbeta(1, 1)
  mean.lp <- logit(mean.p)
  tau <- 1 / (sd * sd)
  sd ~ dunif(0, 5)

  for (site in 1:nSitesCMR) {
    omega[site] ~ dbeta(1, 1)
    # Likelihood
    for (j in 1:nPasses[site]){
      logit(p[site, j]) <- eps[site, j]
      p1[site, j] <- 1 - p[site, j]
      eps[site, j] ~ dnorm(mean.lp, tau)T(-6, 6) 
    }

    for (i in 1:M){
      z[site, i] ~ dbern(omega[site])			# Inclusion indicators
      for (j in 1:nPasses[site]){
        yaug[site,i,j] ~ dbern(p.eff[site,i,j])
        p.eff[site, i, j] <- z[site, i] * p[site, j]		# Can only be detected if z=1
      } #j
    } #i
      
      # Derived quantities
   N[site] <- sum(z[site, 1:M])
   p.star[site] <- 1 - prod(p1[site, 1:nPasses[site]])
  } #site
  Ntotal <- sum(N[1:nSitesCMR])
}
",fill = TRUE)
sink()

# Specify beta-binomial (single visit) model in BUGS language
sink("model.p.bb.1visit.sameM.jags")
cat("
model {
  # Priors
  mean.p ~ dbeta(1, 1)
  l.rho ~ dnorm(0, 0.4)T(-4, 4)
  logit(rho) <- l.rho
#  rho ~ dbeta(1, 1)T(0.01, 0.99)
  alpha <- mean.p*(1-rho)/rho
  beta <- (1-mean.p)*(1-rho)/rho
  for (site in 1:nSitesCMR) {
    omega[site] ~ dbeta(1, 1)
    # Likelihood
    for (j in 1:nPasses[site]){
      p[site, j] ~ dbeta(alpha, beta)
      p1[site, j] <- 1 - p[site, j]
    }
    for (i in 1:M){
      z[site,i] ~ dbern(omega[site])			# Inclusion indicators
      for (j in 1:nPasses[site]){
        yaug[site,i,j] ~ dbern(p.eff[site,i,j])
        p.eff[site,i,j] <- z[site,i] * p[site,j]		# Can only be detected if z=1
      } #j
    } #i
    
    # Derived quantities
    N[site] <- sum(z[site,1:M])
    p.star[site] <- 1 - prod(p1[site, 1:nPasses[site]])
  } #site
  Ntotal <- sum(N[1:nSitesCMR])
}
",fill = TRUE)
sink()

# Specify beta-binomial (single visit) model in BUGS language
sink("model.p.bb.1visit.v2.sameM.jags")
cat("
model {
  # Priors
  mean.p ~ dbeta(1, 1)
  l.rho ~ dnorm(0, 0.4)T(-4, 4)
  logit(rho) <- l.rho
#  rho ~ dbeta(1, 1)T(0.01, 0.99)
  alpha ~ <- mean.p*(1-rho)/rho
  beta <- (1-mean.p)*(1-rho)/rho
  for (site in 1:nSitesCMR) {
    omega[site] ~ dbeta(1, 1)
    # Likelihood
    for (j in 1:nPasses[site]){
      p[site, j] ~ dbeta(alpha, beta)
      p1[site, j] <- 1 - p[site, j]
    }
    for (i in 1:M){
      z[site,i] ~ dbern(omega[site])			# Inclusion indicators
      for (j in 1:nPasses[site]){
        yaug[site,i,j] ~ dbern(p.eff[site,i,j])
        p.eff[site,i,j] <- z[site,i] * p[site,j]		# Can only be detected if z=1
      } #j
    } #i
    
    # Derived quantities
    N[site] <- sum(z[site,1:M])
    p.star[site] <- 1 - prod(p1[site, 1:nPasses[site]])
  } #site
  Ntotal <- sum(N[1:nSitesCMR])
}
",fill = TRUE)
sink()



# Specify beta-binomial model in BUGS language
# Allow counts and CMR
sink("model.p.bb.count.sameM.jags")
cat("
model {
  # Priors
  mean.p ~ dbeta(1, 1)
  rho ~ dunif(0, 0.5)
  alpha <- mean.p*(1-rho)/rho
  beta <- (1-mean.p)*(1-rho)/rho
  for (visit in 1:nVisits) {
    for (site in 1:nSitesCMR[visit]) {
      omega[site,visit] ~ dbeta(1, 1)
      # Likelihood
      for (j in 1:nPasses){
        p[site,visit,j] ~ dbeta(alpha, beta)
      }
      for (i in 1:M[site,visit]){
        z[site,visit,i] ~ dbern(omega[site,visit])			# Inclusion indicators
        for (j in 1:nPasses){
          yaug[site,visit,i,j] ~ dbern(p.eff[site,visit,i,j])
          p.eff[site,visit,i,j] <- z[site,visit,i] * p[site,visit,j]		# Can only be detected if z=1
        } #j
      } #i
      
      # Derived quantities
      N[site,visit] <- sum(z[site,visit,1:M[site,visit]])
    } #site
    for (site in 1:nSitesCount[visit]) {
      # Prior
      Ncount0[site, visit] ~ dunif(0, M.count[site, visit])
      Ncount[site, visit] <- round(Ncount0[site, visit])
#      Ncount[site, visit] ~ dpois(M.count[site, visit])
      # Likelihood
      for (j in 1:nPasses){
        p.count[site,visit,j] ~ dbeta(alpha, beta)
        count[site, visit, j] ~ dbin(p.count[site,visit,j], Ncount[site, visit]) #
      }
    } #site
    Ntotal[visit] <- sum(N[1:nSitesCMR[visit],visit]) + sum(Ncount[1:nSitesCount[visit], visit])
  } #visit
  lambdaTotal <- Ntotal[2:nVisits]/Ntotal[1:(nVisits-1)]
}
",fill = TRUE)
sink()

# Specify beta-binomial model in BUGS language
# Allow counts and CMR with input structure that keeps track of everything for us?
sink("model.p.bb.count.sameM.v2.jags")
cat("
model {
  for (visit in 1:nVisits) {
    # Priors
    mean.p[visit] ~ dbeta(1, 1)
    rho[visit] ~ dunif(0, 0.5)
    alpha[visit] <- mean.p[visit]*(1-rho[visit])/rho[visit]
    beta[visit] <- (1-mean.p[visit])*(1-rho[visit])/rho[visit]
    for (site in 1:nSites) {
      omega[site,visit] ~ dbeta(1, 1)
      # Likelihood
      for (j in 1:nPasses){
        p[site,visit,j] ~ dbeta(alpha[visit], beta[visit])
        count[site, visit, j] ~ dbin(p[site,visit,j], N[site, visit]) 
      }
      for (i in 1:M){
        z[site,visit,i] ~ dbern(omega[site,visit])			# Inclusion indicators
        for (j in 1:nPasses){
          yaug[site,visit,i,j] ~ dbern(p.eff[site,visit,i,j])
          p.eff[site,visit,i,j] <- z[site,visit,i] * p[site,visit,j]		# Can only be detected if z=1
        } #j
      } #i
      
      # Prior for counts
      Ncount0[site, visit] ~ dunif(0, M)
      # Derived quantity, sometimes(?)
      N[site,visit] <- ifelse(equals(isCount[site,visit], 1), 
                              round(Ncount0[site, visit]),
                              ifelse(equals(isCount[site,visit], 2), 
                                     0,
                                     sum(z[site,visit,1:M])))
    } #site
    Ntotal[visit] <- sum(N[1:nSitesUAS,visit]) 
  } #visit
  # Derived quantities
  lambdaTotal <- Ntotal[2:nVisits]/Ntotal[1:(nVisits-1)]
  for (site in 1:nSitesUAS) {
    Ndiff[site, 1:(nVisits - 1)] <- N[site, 2:nVisits] - N[site, 1:(nVisits - 1)]
  }
}
",fill = TRUE)
sink()

sink("model.p.bb.count.sameM.v3.jags")
cat("
model {
  for (visit in 1:nVisits) {
    # Priors
    mean.p[visit] <- alpha[visit] / (alpha[visit] + beta[visit])
    rho[visit] <- 1 / (alpha[visit] + beta[visit] + 1)
    alpha[visit] ~ dgamma(1, 1) T(0.5, )
    beta[visit] ~ dgamma(1, 1) T(0.5, )
    for (site in 1:nSites) {
      omega[site,visit] ~ dbeta(1, 1)
      # Likelihood
      for (j in 1:nPasses){
        p[site,visit,j] ~ dbeta(alpha[visit], beta[visit])
        count[site, visit, j] ~ dbin(p[site,visit,j], N[site, visit]) 
      }
      for (i in 1:M){
        z[site,visit,i] ~ dbern(omega[site,visit])			# Inclusion indicators
        for (j in 1:nPasses){
          yaug[site,visit,i,j] ~ dbern(p.eff[site,visit,i,j])
          p.eff[site,visit,i,j] <- z[site,visit,i] * p[site,visit,j]		# Can only be detected if z=1
        } #j
      } #i
      
      # Prior for counts
      Ncount0[site, visit] ~ dunif(0, M)
      # Derived quantity, sometimes(?)
      N[site,visit] <- ifelse(equals(isCount[site,visit], 1), 
                              round(Ncount0[site, visit]),
                              ifelse(equals(isCount[site,visit], 2), 
                                     0,
                                     sum(z[site,visit,1:M])))
    } #site
    Ntotal[visit] <- sum(N[1:nSitesUAS,visit]) 
  } #visit
  # Derived quantities
  lambdaTotal <- Ntotal[2:nVisits]/Ntotal[1:(nVisits-1)]
  for (site in 1:nSitesUAS) {
    Ndiff[site, 1:(nVisits - 1)] <- N[site, 2:nVisits] - N[site, 1:(nVisits - 1)]
  }
}
",fill = TRUE)
sink()

sink("model.p.bb.count.sameM.v4.jags")
cat("
model {
  for (visit in 1:nVisits) {
    # Priors
    mean.p[visit] <- alpha[visit] / (alpha[visit] + beta[visit])
    rho[visit] <- 1 / (alpha[visit] + beta[visit] + 1)
    alpha[visit] ~ dgamma(1, 1) T(0.5, )
    beta[visit] ~ dgamma(1, 1) T(0.5, )
    for (site in 1:nSites) {
      omega[site,visit] ~ dbeta(1, 1)
      # Likelihood
      for (j in 1:nPassesBy[site, visit]){
        p[site,visit,j] ~ dbeta(alpha[visit], beta[visit])
        p1[site, visit, j] <- 1 - p[site, visit, j]
        count[site, visit, j] ~ dbin(p[site,visit,j], N[site, visit]) 
      }
      for (i in 1:M){
        z[site,visit,i] ~ dbern(omega[site,visit])			# Inclusion indicators
        for (j in 1:nPassesBy[site, visit]){
          yaug[site,visit,i,j] ~ dbern(p.eff[site,visit,i,j])
          p.eff[site,visit,i,j] <- z[site,visit,i] * p[site,visit,j]		# Can only be detected if z=1
        } #j
      } #i
      
      # Prior for counts
      Ncount0[site, visit] ~ dunif(0, M)
      # Derived quantities, sometimes(?)
      p.star[site, visit] <- 1 - prod(p1[site, visit, 1:nPassesBy[site, visit]])
      N[site,visit] <- ifelse(equals(isCount[site,visit], 1), 
                              round(Ncount0[site, visit]),
                              ifelse(equals(isCount[site,visit], 2), 
                                     0,
                                     sum(z[site,visit,1:M])))
    } #site
    Ntotal[visit] <- sum(N[1:nSitesUAS,visit]) 
  } #visit
  # Derived quantities
  lambdaTotal <- Ntotal[2:nVisits]/Ntotal[1:(nVisits-1)]
  for (site in 1:nSitesUAS) {
    Ndiff[site, 1:(nVisits - 1)] <- N[site, 2:nVisits] - N[site, 1:(nVisits - 1)]
  }
}
",fill = TRUE)
sink()

sink("model.p.rand.count.sameM.v4.jags")
cat("
model {
  for (visit in 1:nVisits) {
    # Priors
    mean.p[visit] ~ dbeta(1, 1)
    mean.lp[visit] <- logit(mean.p[visit])
    tau.lp[visit] <- 1 / (sd.lp[visit] * sd.lp[visit])
    sd.lp[visit] ~ dunif(0, 5)
    for (site in 1:nSites) {
      omega[site,visit] ~ dbeta(1, 1)
      # Likelihood
      for (j in 1:nPassesBy[site, visit]){
        logit(p[site, visit, j]) <- eps[site, visit, j]
        p1[site, visit, j] <- 1 - p[site, visit, j]
        eps[site, visit, j] ~ dnorm(mean.lp[visit], tau.lp[visit])T(-6, 6) 
        count[site, visit, j] ~ dbin(p[site,visit,j], N[site, visit]) 
      }
      for (i in 1:M){
        z[site,visit,i] ~ dbern(omega[site,visit])			# Inclusion indicators
        for (j in 1:nPassesBy[site, visit]){
          yaug[site,visit,i,j] ~ dbern(p.eff[site,visit,i,j])
          p.eff[site,visit,i,j] <- z[site,visit,i] * p[site,visit,j]		# Can only be detected if z=1
        } #j
      } #i
      
      # Prior for counts
      Ncount0[site, visit] ~ dunif(0, M)
      # Derived quantity, sometimes(?)
      N[site,visit] <- ifelse(equals(isCount[site,visit], 1), 
                              round(Ncount0[site, visit]),
                              ifelse(equals(isCount[site,visit], 2), 
                                     0,
                                     sum(z[site,visit,1:M])))
      p.star[site,visit] <- 1 - prod(p1[site, visit, 1:nPassesBy[site, visit]])
    } #site
    Ntotal[visit] <- sum(N[1:nSitesUAS,visit]) 
  } #visit
  # Derived quantities
  lambdaTotal <- Ntotal[2:nVisits]/Ntotal[1:(nVisits-1)]
  for (site in 1:nSitesUAS) {
    Ndiff[site, 1:(nVisits - 1)] <- N[site, 2:nVisits] - N[site, 1:(nVisits - 1)]
  }
}
",fill = TRUE)
sink()

sink("model.p.visit.count.sameM.v4.jags")
cat("
model {
  for (visit in 1:nVisits) {
    # Priors
    mean.p[visit] ~ dbeta(1, 1)
    for (site in 1:nSites) {
      omega[site,visit] ~ dbeta(1, 1)
      # Likelihood
      for (j in 1:nPassesBy[site, visit]){
        p[site,visit,j] <- mean.p[visit]
        p1[site, visit, j] <- 1 - p[site, visit, j]
        count[site, visit, j] ~ dbin(p[site,visit,j], N[site, visit]) 
      }
      for (i in 1:M){
        z[site,visit,i] ~ dbern(omega[site,visit])			# Inclusion indicators
        for (j in 1:nPassesBy[site, visit]){
          yaug[site,visit,i,j] ~ dbern(p.eff[site,visit,i,j])
          p.eff[site,visit,i,j] <- z[site,visit,i] * p[site,visit,j]		# Can only be detected if z=1
        } #j
      } #i
      
      # Prior for counts
      Ncount0[site, visit] ~ dunif(0, M)
      # Derived quantities, sometimes(?)
      p.star[site, visit] <- 1 - prod(p1[site, visit, 1:nPassesBy[site, visit]])
      N[site,visit] <- ifelse(equals(isCount[site,visit], 1), 
                              round(Ncount0[site, visit]),
                              ifelse(equals(isCount[site,visit], 2), 
                                     0,
                                     sum(z[site,visit,1:M])))
    } #site
    Ntotal[visit] <- sum(N[1:nSitesUAS,visit]) 
  } #visit
  # Derived quantities
  lambdaTotal <- Ntotal[2:nVisits]/Ntotal[1:(nVisits-1)]
  for (site in 1:nSitesUAS) {
    Ndiff[site, 1:(nVisits - 1)] <- N[site, 2:nVisits] - N[site, 1:(nVisits - 1)]
  }
}
",fill = TRUE)
sink()

sink("model.p.visit.sameM.v5.jags")
cat("
model {
  for (visit in 1:nVisits) {
    # Priors
    mean.p[visit] ~ dbeta(1, 1)
    for (site in 1:nSites) {
      omega[site,visit] ~ dbeta(1, 1)
      # Likelihood
      for (j in 1:nPassesBy[site, visit]){
        p[site,visit,j] <- mean.p[visit]
        p1[site, visit, j] <- 1 - p[site, visit, j]
      }
      for (i in 1:M){
        z[site,visit,i] ~ dbern(omega[site,visit])			# Inclusion indicators
        for (j in 1:nPassesBy[site, visit]){
          yaug[site,visit,i,j] ~ dbern(p.eff[site,visit,i,j])
          p.eff[site,visit,i,j] <- z[site,visit,i] * p[site,visit,j]		# Can only be detected if z=1
        } #j
      } #i
      
      # Derived quantities, sometimes(?)
      p.star[site, visit] <- 1 - prod(p1[site, visit, 1:nPassesBy[site, visit]])
      N[site,visit] <- sum(z[site,visit,1:M])
    } #site
    Ntotal[visit] <- sum(N[1:nSitesAllYears,visit]) 
  } #visit
  # Derived quantities
  lambdaTotal <- Ntotal[2:nVisits]/Ntotal[1:(nVisits-1)]
  for (site in 1:nSitesAllYears) {
    Ndiff[site, 1:(nVisits - 1)] <- N[site, 2:nVisits] - N[site, 1:(nVisits - 1)]
  }
}
",fill = TRUE)
sink()

sink("model.p.rand.sameM.v5.jags")
cat("
model {
  for (visit in 1:nVisits) {
    # Priors
    mean.p[visit] ~ dbeta(1, 1)
    mean.lp[visit] <- logit(mean.p[visit])
    tau.lp[visit] <- 1 / (sd.lp[visit] * sd.lp[visit])
    sd.lp[visit] ~ dunif(0, 5)
    for (site in 1:nSites) {
      omega[site,visit] ~ dbeta(1, 1)
      # Likelihood
      for (j in 1:nPassesBy[site, visit]){
        logit(p[site, visit, j]) <- eps[site, visit, j]
        p1[site, visit, j] <- 1 - p[site, visit, j]
        eps[site, visit, j] ~ dnorm(mean.lp[visit], tau.lp[visit])T(-6, 6) 
      }
      for (i in 1:M){
        z[site,visit,i] ~ dbern(omega[site,visit])			# Inclusion indicators
        for (j in 1:nPassesBy[site, visit]){
          yaug[site,visit,i,j] ~ dbern(p.eff[site,visit,i,j])
          p.eff[site,visit,i,j] <- z[site,visit,i] * p[site,visit,j]		# Can only be detected if z=1
        } #j
      } #i
      
      # Derived quantity
      N[site,visit] <- sum(z[site,visit,1:M])
      p.star[site,visit] <- 1 - prod(p1[site, visit, 1:nPassesBy[site, visit]])
    } #site
    Ntotal[visit] <- sum(N[1:nSitesAllYears,visit]) 
  } #visit
  # Derived quantities
  lambdaTotal <- Ntotal[2:nVisits]/Ntotal[1:(nVisits-1)]
  for (site in 1:nSitesAllYears) {
    Ndiff[site, 1:(nVisits - 1)] <- N[site, 2:nVisits] - N[site, 1:(nVisits - 1)]
  }
}
",fill = TRUE)
sink()



sink("model.p.bb.sameM.v5.jags")
cat("
model {
  for (visit in 1:nVisits) {
    # Priors
    mean.p[visit] <- alpha[visit] / (alpha[visit] + beta[visit])
    rho[visit] <- 1 / (alpha[visit] + beta[visit] + 1)
    alpha[visit] ~ dgamma(1, 1) T(0.5, )
    beta[visit] ~ dgamma(1, 1) T(0.5, )
    for (site in 1:nSites) {
      omega[site,visit] ~ dbeta(1, 1)
      # Likelihood
      for (j in 1:nPassesBy[site, visit]){
        p[site,visit,j] ~ dbeta(alpha[visit], beta[visit])
        p1[site, visit, j] <- 1 - p[site, visit, j]
      }
      for (i in 1:M){
        z[site,visit,i] ~ dbern(omega[site,visit])			# Inclusion indicators
        for (j in 1:nPassesBy[site, visit]){
          yaug[site,visit,i,j] ~ dbern(p.eff[site,visit,i,j])
          p.eff[site,visit,i,j] <- z[site,visit,i] * p[site,visit,j]		# Can only be detected if z=1
        } #j
      } #i
      
      # Derived quantities, sometimes(?)
      p.star[site, visit] <- 1 - prod(p1[site, visit, 1:nPassesBy[site, visit]])
      N[site,visit] <- sum(z[site,visit,1:M])
    } #site
    Ntotal[visit] <- sum(N[1:nSitesAllYears, visit]) 
  } #visit
  # Derived quantities
  lambdaTotal <- Ntotal[2:nVisits]/Ntotal[1:(nVisits-1)]
  for (site in 1:nSitesAllYears) {
    Ndiff[site, 1:(nVisits - 1)] <- N[site, 2:nVisits] - N[site, 1:(nVisits - 1)]
  }
}
",fill = TRUE)
sink()

sites <- c(paste("Canal", c(1:12)), "Mit Pools", "Wooten's", "Big Cypress 1",
           "Big Cypress 2") 
sites2 <- c(paste("Canal", c(1:12)), "Mit Pools", "Wooten's", "BC 1", "BC 2") 
sites3 <- c(paste("Canal", c(1:12)), "Mitigation Pools", "Wooten's", "Big Cypress 1",
            "Big Cypress 2") 
sites4 <- c(paste("Canal", c(1:12)), "Mit Pools", "Wooten's", "Big Cypress Viewing",
            "Big Cypress Lollipop") 


# Set up structures and read in data
M <- matrix(0, nSites, nVisits)
yaug <- array(NA, c(nSites, nVisits, nUpper, nPasses),
              dimnames = list(sites, visits, NULL, paste0('Min.', 1:nPasses)))


y1 <- read.csv('../POI_UAS_31Jan2017.csv')
y1$Site <- factor(y1$Pt, levels = c(1:12, "Mit", "Wooton's", "Big Cypress Viewing",
           "Big Cypress Lollipop"), labels = sites)
summary(y1)
summary(y1$Pt)
summary(y1$Site)

y2 <- read.csv('../POI_UAS_1Feb2017.csv')
y2$Site <- factor(y2$Pt, levels = c(1:12, "Mit", "Wooton's", "Big Cypress Viewing",
           "Big Cypress Lollipop"), labels = sites)
summary(y2)
summary(y2$Pt)
summary(y2$Site)
summary(y2$Comments)
subset(y2, Comments=='One manatee swam into the frame in the last min of the video')
# This manatee was swimming through main canal, not in canal 10 proper
y2[y2$Comments=='One manatee swam into the frame in the last min of the video', 'Min.6'] <- 0

y3 <- read.csv('../POI_UAS_7Jan2018_Final.csv')
y3$Site <- factor(y3$Pt, levels = c(1:12, "MitPools", "Wootens", "Big Cypress Viewing",
           "Big Cypress Lollipop"), labels = sites)
summary(y3)
summary(y3$Pt)
summary(y3$Site)

y4 <- read.csv('../POI_UAS19.csv')
y4$Site <- factor(y4$Pt, levels = sites4, labels = sites)
summary(y4)
summary(factor(y4$Pt))
summary(y4$Site)
subset(y4, Pt == "Big Cypress Lollipop")
subset(y4, Pt == "Big Cypress Viewing")

fill.yaug <- function(yaug, M, y, site, visit) {
  cmr0 <- as.matrix(subset(y, Site == sites[site])[, paste0('Min.', 1:nPasses)])
  if (sum(cmr0, na.rm = TRUE)>0) {
    yaug[site, visit, 1:nrow(cmr0), 1:nPasses] <- cmr0
    yaug[site, visit, (1+nrow(cmr0)):nUpper, 1:nPasses] <- 0
    M[site, visit] <- nrow(cmr0)
  }
  else {
    if (nrow(cmr0)>0) {
      nPassesHere <- max(which(!is.na(cmr0[1,])))
      yaug[site, visit, 1:nUpper, 1:nPassesHere] <- 0
    }
    M[site, visit] <- 0
  }
  return(list(yaug = yaug, M = M))
}

for (visit in 1:nVisits) {
  y <- get(paste0('y', visit))
  for (site in 1:nSites) {
    temp <- fill.yaug(yaug, M, y, site, visit)
    yaug <- temp$yaug
    M <- temp$M
  }
}
M
yaug[5, 1, 1:10, ]
yaug[5, 2, 1:20, ]
yaug[nSites, 3, 1:20, ]
yaug[nSites, 4, 1:20, ]
yaug[nSites-1, 4, 1:20, ]

nPassesBy <- apply(yaug[,,1,], 1:2, function(x) max(which(!is.na(x))))
nPassesBy[is.infinite(nPassesBy)] <- 1
nPassesBy
p.fill <- array(0, c(nSites, nVisits, nPasses))
for (site in 1:nSites)
  for (visit in 1:nVisits)
    if (nPassesBy[site, visit]>0)
      p.fill[site, visit, 1:nPassesBy[site, visit]] <- NA
# 
# jags.data.v5 <- list(yaug = yaug, 
#                      nVisits = nVisits, nSites = nSites, nSitesAllYears = nSitesAllYears, 
#                      nPassesBy = nPassesBy, M = nUpper, p = p.fill)
# 
# # start.time = Sys.time()          # Set timer: ART 4.8 hrs?        
# # out.p.rand <- jags(jags.data.v4, inits.p.rand.v4, params.p.rand, "model.p.rand.count.sameM.v4.jags", 
# #                  n.chains = 3, n.thin = 1, n.iter = ni*3, n.burnin = nb*3, parallel = T)
# # Sys.time() - start.time
# # 
# # out.p.rand
# # 
# # out.p.rand$mean$N - out.p.bb$mean$N
# 
# start.time = Sys.time()         # Set timer: ART 3 hrs
# out.p.bb <- jags(jags.data.v5, inits.p.bb.v5, params.p.bb.v5, "model.p.bb.sameM.v5.jags", 
#                  n.chains = 3, n.thin = 1, n.iter = ni*3, n.burnin = nb*3, parallel = T)
# Sys.time() - start.time
# 
# out.p.bb
# # 
# # out.p.rand$summary[!is.na(out.p.rand$summary[, 'Rhat']) & out.p.rand$summary[, 'Rhat']>=1.1, ]
# # out.p.bb18.6$summary[!is.na(out.p.bb18.6$summary[, 'Rhat']) & out.p.bb18.6$summary[, 'Rhat']>=1.1, ]
# save.image('estAll5.RData')
# 
# Try with only first six minutes.
# Because Holly ordered animals by which minute they first appeared in, can just lop off last 4 minutes
nPassesBy6 <- nPassesBy
for (i in 1:nVisits)
  nPassesBy6[,i] <- pmin(6, nPassesBy[,i])
dropped <- which(apply(yaug, 1:3, function(x) sum(x[1:6]) == 0 && sum(x[7:10]) > 0), arr.ind = T)
yaug6 <- yaug[,,,1:6]
M6 <- apply(yaug6, 1:2, function(x) sum(rowSums(x)>0))
M6[is.na(M6)] <- 0
p.fill6 <- p.fill[ , , 1:6]
# 
# jags.data.6min <- list(yaug = yaug6, M = nUpper, nVisits = nVisits, nSites = nSites, 
#                        nSitesAllYears = nSitesAllYears, nPassesBy = nPassesBy6, p = p.fill6)
# 
###########################################
# Run or rerun some models
###########################################

load('estAll5.RData')
start.time = Sys.time()         # Set timer: ART 3.8 hours
out.p.bb.6min <- jags(jags.data.6min, inits.p.bb.v5, params.p.bb.v5, "model.p.bb.sameM.v5.jags",
                 n.chains = 3, n.thin = 1, n.iter = ni*4, n.burnin = nb*4, parallel = T)
Sys.time() - start.time
save.image('estAll5.RData')
out.p.bb.6min
out.p.bb$summary[!is.na(out.p.bb$summary[, 'Rhat']) & out.p.bb$summary[, 'Rhat']>=1.1, ]
out.p.bb.6min$summary[!is.na(out.p.bb.6min$summary[, 'Rhat']) & out.p.bb.6min$summary[, 'Rhat']>=1.1, ]

start.time = Sys.time()         # Set timer: ART 4 hours?
out.p.rand <- jags(jags.data.v5, inits.p.rand.v5, params.p.rand, "model.p.rand.sameM.v5.jags", 
                 n.chains = 3, n.thin = 1, n.iter = ni*3, n.burnin = nb*3, parallel = T)
Sys.time() - start.time
save.image('estAll5.RData')
#out.p.rand
out.p.rand$summary[!is.na(out.p.rand$summary[, 'Rhat']) & out.p.rand$summary[, 'Rhat']>=1.1, ]

start.time = Sys.time()         # Set timer: ART 4 hours?
out.p.rand.6min <- jags(jags.data.6min, inits.p.rand.v5, params.p.rand, "model.p.rand.sameM.v5.jags", 
                 n.chains = 3, n.thin = 1, n.iter = ni*3, n.burnin = nb*3, parallel = T)
Sys.time() - start.time
save.image('estAll5.RData')
#out.p.rand.6min
out.p.rand$summary[!is.na(out.p.rand$summary[, 'Rhat']) & out.p.rand$summary[, 'Rhat']>=1.1, ]
out.p.rand.6min$summary[!is.na(out.p.rand.6min$summary[, 'Rhat']) & out.p.rand.6min$summary[, 'Rhat']>=1.1, ]



# Call JAGS from R 
start.time = Sys.time()         # Set timer: ART 3.1 hrs?
out.p0.6 <- jags(jags.data.6min, inits.p.visit.v5, params.p.visit.v4, "model.p.visit.sameM.v5.jags", 
                 n.chains = nc, n.thin = nt, n.iter = ni*3, n.burnin = nb*3, parallel = T)
Sys.time() - start.time
save.image('estAll5.RData')
out.p0.6$summary[!is.na(out.p0.6$summary[, 'Rhat']) & out.p0.6$summary[, 'Rhat']>=1.1, ]


#Try out binomial model (p[visit]) instead of beta-binomial, for real data
start.time = Sys.time()         # Set timer: ART 3.1 hrs?
out.p0 <- jags(jags.data.v5, inits.p.visit.v5, params.p.visit.v4, "model.p.visit.sameM.v5.jags", 
                 n.chains = nc, n.thin = nt, n.iter = ni*3, n.burnin = nb*3, parallel = T)
Sys.time() - start.time
save.image('estAll5.RData')

out.p0
out.p0$summary[!is.na(out.p0$summary[, 'Rhat']) & out.p0$summary[, 'Rhat']>=1.1, ]


# max(out.p.bb$sims.list$N)
# which(out.p.bb$sims.list$N[,1:12,]==100, arr.ind = T)
# which(out.p.bb$sims.list$N[,13:nSites,1]==100, arr.ind = T)
# max(out.p0$sims.list$N)
# which(out.p0$sims.list$N[,1:12,]==100, arr.ind = T)
# which(out.p0$sims.list$N[,13:nSites,1]==100, arr.ind = T)

library(ggplot2); library(plyr)

###########################################
# Get model outputs ready for plotting
###########################################

sites <- c(paste("Canal", c(1:12)), "Mit Pools", "Wooten's", "Big Cypress 1",
           "Big Cypress 2") 
sites2 <- c(paste("Canal", c(1:12)), "Mit Pools", "Wooten's", "BC 1", "BC 2") 
sites3 <- c(paste("Canal", c(1:12)), "Mitigation Pools", "Wooten's", "Big Cypress 1",
           "Big Cypress 2") 
sites4 <- c(paste("Canal", c(1:12)), "Mit Pools", "Wooten's", "Big Cypress Viewing",
            "Big Cypress Lollipop") 

rownames(nPassesBy) <- sites2
rownames(nPassesBy6) <- sites2
full.bb.est <- data.frame(Parameter = c(rep("N", nSites * nVisits), 
                                        rep('N total', nVisits), 
                                        rep('N diff', nSitesAllYears * (nVisits - 1)),
                                        rep('lambda', nVisits - 1), 
                                        rep(c('p mean','rho'), each = nVisits), 
                                        rep('p', nSites * nVisits * nPasses),
                                        rep('p*', nSites * nVisits), 'deviance'),
                          Site = factor(c(rep(sites, nVisits), rep('All', nVisits), rep(sitesAllYears, nVisits - 1), 
                                          rep('All', 3 * nVisits - 1), rep(sites, nVisits * nPasses), 
                                          rep(sites, nVisits), 'All'),
                                        levels = c(sites, 'All')),
                          Visit = c(rep(visits, each = nSites), visits, rep(visits[1:(nVisits - 1)], each = nSitesAllYears),
                                    visits[1:(nVisits - 1)], rep(visits, 2), rep(visits, nPasses, each = nSites),
                                    rep(visits, each = nSites), 'All'),
                          Pass = c(rep('All', nVisits * (nSites + 3) + (nVisits - 1) * (nSitesAllYears + 1)), 
                                   rep(1:nPasses, each = nSites * nVisits), rep('All', nVisits * nSites + 1)),
                          out.p.bb$summary)
full.bb.est
summary(full.bb.est)
subset(full.bb.est, Rhat >=1.1)
subset(full.bb.est, !(Parameter != 'deviance' & (Site %in% c(sitesAllYears, 'All') | 
                                                                Visit == visits[4] | Visit == 'All')))
full.bb.est <- subset(full.bb.est, Parameter != 'deviance' & (Site %in% c(sitesAllYears, 'All') | 
                                                                Visit == visits[4] | Visit == 'All'))
for (site in sites)
  for (visit in visits) {
    if (nrow(subset(full.bb.est, Site == site & Visit == visit & Parameter == 'p'))>0)
      full.bb.est <- full.bb.est[which(!(full.bb.est$Parameter == 'p' & 
                                         full.bb.est$Site == site & full.bb.est$Visit == visit &
                                         as.integer(as.character(full.bb.est$Pass))>nPassesBy[site, visit])),]
  }

full.bb.est$Parameter <- factor(full.bb.est$Parameter, 
                                  levels = c('N', 'N total', 'N diff', 'lambda', 
                                             'p mean', 'rho', 'p', 'p*')) 
full.bb.est$Visit <- factor(full.bb.est$Visit, levels = visits) 
full.bb.est$Pass <- factor(full.bb.est$Pass, levels = c(1:nPasses, 'All'))  
summary(full.bb.est)

bb.est.6min <- data.frame(Parameter = c(rep("N", nSites * nVisits), 
                                        rep('N total', nVisits), 
                                        rep('N diff', nSitesAllYears * (nVisits - 1)),
                                        rep('lambda', nVisits - 1), 
                                        rep(c('p mean','rho'), each = nVisits), 
                                        rep('p', nSites * nVisits * 6),
                                        rep('p*', nSites * nVisits), 'deviance'),
                          Site = factor(c(rep(sites, nVisits), rep('All', nVisits), rep(sitesAllYears, nVisits - 1), 
                                          rep('All', 3 * nVisits - 1), rep(sites, nVisits * 6), 
                                          rep(sites, nVisits), 'All'),
                                        levels = c(sites, 'All')),
                          Visit = c(rep(visits, each = nSites), visits, rep(visits[1:(nVisits - 1)], each = nSitesAllYears),
                                    visits[1:(nVisits - 1)], rep(visits, 2), rep(visits, 6, each = nSites),
                                    rep(visits, each = nSites), 'All'),
                          Pass = c(rep('All', nVisits * (nSites + 3) + (nVisits - 1) * (nSitesAllYears + 1)), 
                                   rep(1:6, each = nSites * nVisits), rep('All', nVisits * nSites + 1)),
                          out.p.bb.6min$summary)
bb.est.6min
summary(bb.est.6min)
subset(bb.est.6min, Rhat >=1.1)
full.bb.est['N[12,2]',]
subset(bb.est.6min, !(Parameter != 'deviance' & (Site %in% c(sitesAllYears, 'All') | 
                                                                Visit == visits[4] | Visit == 'All')))
bb.est.6min <- subset(bb.est.6min, Parameter != 'deviance' & (Site %in% c(sitesAllYears, 'All') | 
                                                                Visit == visits[4] | Visit == 'All'))
for (site in sites)
  for (visit in visits) {
    if (nrow(subset(bb.est.6min, Site == site & Visit == visit & Parameter == 'p'))>0)
      bb.est.6min <- bb.est.6min[which(!(bb.est.6min$Parameter == 'p' & 
                                         bb.est.6min$Site == site & bb.est.6min$Visit == visit &
                                         as.integer(as.character(bb.est.6min$Pass))>nPassesBy[site, visit])),]
  }
summary(bb.est.6min$Parameter)
bb.est.6min$Parameter <- factor(bb.est.6min$Parameter, 
                                  levels = c('N', 'N total', 'N diff', 'lambda', 
                                             'p mean', 'rho', 'p', 'p*')) 
bb.est.6min$Visit <- factor(bb.est.6min$Visit, levels = visits) 
bb.est.6min$Pass <- factor(bb.est.6min$Pass, levels = c(1:6, 'All'))  
summary(bb.est.6min)

full.rand.est <- data.frame(Parameter = c(rep("N", nSites * nVisits),
                                        rep('N total', nVisits),
                                        rep('N diff', nSitesAllYears * (nVisits - 1)),
                                        rep('lambda', nVisits - 1),
                                        rep(c('p mean','p sd'), each = nVisits),
                                        rep('p', nSites * nVisits * nPasses),
                                        rep('p*', nSites * nVisits), 'deviance'),
                          Site = factor(c(rep(sites, nVisits), rep('All', nVisits), rep(sites[1:nSitesAllYears], nVisits - 1),
                                          rep('All', 3 * nVisits - 1), rep(sites, nVisits * nPasses),
                                          rep(sites, nVisits), 'All'),
                                        levels = c(sites, 'All')),
                          Visit = c(rep(visits, each = nSites), visits, rep(visits[1:(nVisits - 1)], each = nSitesAllYears),
                                    visits[1:(nVisits - 1)], rep(visits, 2), rep(visits, nPasses, each = nSites),
                                    rep(visits, each = nSites), 'All'),
                          Pass = c(rep('All', nVisits * (nSites + 3) + (nVisits - 1) * (nSitesAllYears + 1)),
                                   rep(1:nPasses, each = nSites * nVisits), rep('All', nVisits * nSites + 1)),
                          out.p.rand$summary)
full.rand.est <- subset(full.rand.est, Parameter != 'deviance' & (Site %in% c(sitesAllYears, 'All') | 
                                                                Visit == visits[4] | Visit == 'All'))
for (site in sites)
  for (visit in visits) {
    if (nrow(subset(full.rand.est, Site == site & Visit == visit & Parameter == 'p'))>0)
      full.rand.est <- full.rand.est[which(!(full.rand.est$Parameter == 'p' &
                                         full.rand.est$Site == site & full.rand.est$Visit == visit &
                                         as.integer(as.character(full.rand.est$Pass))>nPassesBy[site, visit])),]
  }
full.rand.est <- transform(full.rand.est,
                           Visit = factor(Visit, levels = visits),
                           Parameter = factor(Parameter, levels = c('N', 'N total', 'N diff', 'lambda',
                                             'p mean', 'p sd', 'p', 'p*')),
                           Pass = factor(Pass, levels = c(1:nPasses, 'All')))

max.count <- data.frame(Parameter = 'Max Count', Site = factor(rep(sites, nVisits), levels = sites),
                        Visit = rep(visits, each = nSites),
                        Pass = 'All', 
                        mean = c(M), sd = 0, X2.5. = c(M), X25. = c(M), 
                        X50. = c(M), X75. = c(M), X97.5. = c(M),
                        Rhat = NA, n.eff = NA, overlap0 = NA, f = NA)
max.count <- rbind(max.count, 
                   data.frame(Parameter = 'Max Count', Site = "All",
                              Visit = visits,
                              Pass = 'All', 
                              mean = colSums(M[1:14,]), sd = 0, X2.5. = colSums(M[1:14,]), 
                              X25. = colSums(M[1:14,]), X50. = colSums(M[1:14,]), 
                              X75. = colSums(M[1:14,]), X97.5. = colSums(M[1:14,]),
                              Rhat = NA, n.eff = NA, overlap0 = NA, f = NA))
max.count <- subset(max.count, Site %in% c(sitesAllYears, 'All') | Visit == visits[4])

max.count6 <- data.frame(Parameter = 'Max Count', Site = factor(rep(sites, nVisits), levels = sites),
                        Visit = rep(visits, each = nSites),
                        Pass = 'All', 
                        mean = c(M6), sd = 0, X2.5. = c(M6), X25. = c(M6), 
                        X50. = c(M6), X75. = c(M6), X97.5. = c(M6),
                        Rhat = NA, n.eff = NA, overlap0 = NA, f = NA)
max.count6 <- rbind(max.count6, 
                   data.frame(Parameter = 'Max Count', Site = "All",
                              Visit = visits,
                              Pass = 'All', 
                              mean = colSums(M6[1:14,]), sd = 0, X2.5. = colSums(M6[1:14,]), 
                              X25. = colSums(M6[1:14,]), X50. = colSums(M6[1:14,]), 
                              X75. = colSums(M6[1:14,]), X97.5. = colSums(M6[1:14,]),
                              Rhat = NA, n.eff = NA, overlap0 = NA, f = NA))
max.count6 <- subset(max.count6, Site %in% c(sitesAllYears, 'All') | Visit == visits[4])

rand.est.6min <- data.frame(Parameter = c(rep("N", nSites * nVisits), 
                                        rep('N total', nVisits), 
                                        rep('N diff', nSitesAllYears * (nVisits - 1)),
                                        rep('lambda', nVisits - 1), 
                                        rep(c('p mean','p sd'), each = nVisits), 
                                        rep('p', nSites * nVisits * 6),
                                        rep('p*', nSites * nVisits), 'deviance'),
                          Site = factor(c(rep(sites, nVisits), rep('All', nVisits), rep(sitesAllYears, nVisits - 1), 
                                          rep('All', 3 * nVisits - 1), rep(sites, nVisits * 6), 
                                          rep(sites, nVisits), 'All'),
                                        levels = c(sites, 'All')),
                          Visit = c(rep(visits, each = nSites), visits, rep(visits[1:(nVisits - 1)], each = nSitesAllYears),
                                    visits[1:(nVisits - 1)], rep(visits, 2), rep(visits, 6, each = nSites),
                                    rep(visits, each = nSites), 'All'),
                          Pass = c(rep('All', nVisits * (nSites + 3) + (nVisits - 1) * (nSitesAllYears + 1)), 
                                   rep(1:6, each = nSites * nVisits), rep('All', nVisits * nSites + 1)),
                          out.p.rand.6min$summary)
rand.est.6min
summary(rand.est.6min)
subset(rand.est.6min, Rhat >=1.1)
divergent <- rownames(subset(rand.est.6min, Rhat >=1.1))
full.bb.est[divergent,]
full.rand.est[divergent,]
rand.est.6min <- subset(rand.est.6min, Parameter != 'deviance' & (Site %in% c(sitesAllYears, 'All') | 
                                                                Visit == visits[4] | Visit == 'All'))
for (site in sites)
  for (visit in visits) {
    if (nrow(subset(rand.est.6min, Site == site & Visit == visit & Parameter == 'p'))>0)
      rand.est.6min <- rand.est.6min[which(!(rand.est.6min$Parameter == 'p' & 
                                         rand.est.6min$Site == site & rand.est.6min$Visit == visit &
                                         as.integer(as.character(rand.est.6min$Pass))>nPassesBy[site, visit])),]
  }
summary(rand.est.6min$Parameter)
rand.est.6min$Parameter <- factor(rand.est.6min$Parameter, 
                                  levels = c('N', 'N total', 'N diff', 'lambda', 
                                             'p mean', 'p sd', 'p', 'p*')) 
rand.est.6min$Visit <- factor(rand.est.6min$Visit, levels = visits) 
rand.est.6min$Pass <- factor(rand.est.6min$Pass, levels = c(1:6, 'All'))  
summary(rand.est.6min)


both.est <- rbind(data.frame(Analysis = 'Count (All)', max.count),
                  data.frame(Analysis = 'All 10 Minutes', subset(full.bb.est, Parameter != "Max Count")),
                  data.frame(Analysis = '1st 6 Minutes', subset(bb.est.6min, Parameter != "Max Count")),
                  data.frame(Analysis = 'Count (6 Minutes)', max.count6))
both.est$Analysis <- factor(both.est$Analysis, 
                            levels = c('Count (All)', 'All 10 Minutes', 'Count (6 Minutes)', '1st 6 Minutes'))
summary(both.est)

rand.both.est <- rbind(data.frame(Analysis = 'Data', max.count),
                       data.frame(Analysis = 'All Passes', subset(full.rand.est, Parameter != "Max Count")),
                       data.frame(Analysis = '1st 6 Passes', rand.est.6min))

full.both.est <- rbind(data.frame(Analysis = 'Data', max.count),
                       data.frame(Analysis = 'Beta-binomial', subset(full.bb.est, Parameter != "Max Count")),
                       data.frame(Analysis = 'Logit Normal', full.rand.est))

both.est.6min <- rbind(data.frame(Analysis = 'Data', max.count),
                       data.frame(Analysis = 'Beta-binomial', subset(bb.est.6min, Parameter != "Max Count")),
                       data.frame(Analysis = 'Logit Normal', rand.est.6min))


full.bb.est <- rbind(max.count, full.bb.est)
bb.est.6min <- rbind(max.count, bb.est.6min)
summary(full.bb.est$Parameter)
summary(both.est$Parameter)
summary(full.both.est$Parameter)
summary(both.est.6min$Parameter)
summary(both.est$Visit)
summary(both.est$Pass)
full.both.est <- transform(full.both.est,
                           Visit = factor(Visit, levels = visits),
                           Parameter = factor(Parameter, levels = c('N', 'N total', 'N diff', 'lambda',
                                             'p mean', 'rho', 'p sd', 'p', 'p*')),
                           Pass = factor(Pass, levels = c(1:nPasses, 'All')))

both.est.6min <- transform(both.est.6min,
                           Visit = factor(Visit, levels = visits),
                           Parameter = factor(Parameter, levels = c('N', 'N total', 'N diff', 'lambda',
                                             'p mean', 'rho', 'p sd', 'p', 'p*')),
                           Pass = factor(Pass, levels = c(1:6, 'All')))

p0.est <- data.frame(Parameter = c(rep("N", nSites * nVisits), 
                                        rep('N total', nVisits), 
                                        rep('N diff', nSitesAllYears * (nVisits - 1)),
                                        rep('lambda', nVisits - 1), 
                                        rep('p mean', nVisits), 
                                        rep('p*', nSites * nVisits), 'deviance'),
                          Site = factor(c(rep(sites, nVisits), rep('All', nVisits), rep(sitesAllYears, nVisits - 1), 
                                          rep('All', 2 * nVisits - 1), 
                                          rep(sites, nVisits), 'All'),
                                        levels = c(sites, 'All')),
                          Visit = c(rep(visits, each = nSites), visits, rep(visits[1:(nVisits - 1)], each = nSitesAllYears),
                                    visits[1:(nVisits - 1)], visits, 
                                    rep(visits, each = nSites), 'All'),
                          Pass = 'All', 
                          out.p0$summary)
p0.est
summary(p0.est)
subset(p0.est, Rhat >=1.1)
p0.est <- subset(p0.est, Parameter != 'deviance' & (Site %in% c(sitesAllYears, 'All') | 
                                                                Visit == visits[4] | Visit == 'All'))
p0.est$Parameter <- factor(p0.est$Parameter, 
                                  levels = c('N', 'N total', 'N diff', 'lambda', 
                                             'p mean', 'p*')) 
p0.est$Visit <- factor(p0.est$Visit, levels = visits) 
summary(p0.est)

p0.est.6min <- data.frame(Parameter = c(rep("N", nSites * nVisits), 
                                        rep('N total', nVisits), 
                                        rep('N diff', nSitesAllYears * (nVisits - 1)),
                                        rep('lambda', nVisits - 1), 
                                        rep('p mean', nVisits), 
                                        rep('p*', nSites * nVisits), 'deviance'),
                          Site = factor(c(rep(sites, nVisits), rep('All', nVisits), rep(sitesAllYears, nVisits - 1), 
                                          rep('All', 2 * nVisits - 1), 
                                          rep(sites, nVisits), 'All'),
                                        levels = c(sites, 'All')),
                          Visit = c(rep(visits, each = nSites), visits, rep(visits[1:(nVisits - 1)], each = nSitesAllYears),
                                    visits[1:(nVisits - 1)], visits, 
                                    rep(visits, each = nSites), 'All'),
                          Pass = 'All', 
                          out.p0.6$summary)
p0.est.6min
summary(p0.est.6min)
subset(p0.est, Rhat >=1.1)
p0.est.6min <- subset(p0.est.6min, Parameter != 'deviance' & (Site %in% c(sitesAllYears, 'All') | 
                                                                Visit == visits[4] | Visit == 'All'))
p0.est.6min$Parameter <- factor(p0.est.6min$Parameter, 
                                  levels = c('N', 'N total', 'N diff', 'lambda', 
                                             'p mean', 'p*')) 
p0.est.6min$Visit <- factor(p0.est.6min$Visit, levels = visits) 
summary(p0.est.6min)

# Summarize detection probabilities by site only
dim(out.p.bb$sims.list$p)
summary(out.p.bb$sims.list$p[,16,1,])
p10 <- out.p.bb$sims.list$p
for (s in 1:nSites) {
  for (v in 1:nVisits) {
    if (nPassesBy[s,v]==1)
      p10[,s,v,] <- NA
    else if (nPassesBy[s,v]<nPasses)
      p10[,s,v,(nPassesBy[s,v] + 1):nPasses] <- NA
  }
}
p10.site.mean <- apply(p10, 2, mean, na.rm = T)
names(p10.site.mean) <- sites
p10.site.mean

p6 <- out.p.bb.6min$sims.list$p
for (s in 1:nSites) {
  for (v in 1:nVisits) {
    if (nPassesBy6[s,v]==1)
      p6[,s,v,] <- NA
    else if (nPassesBy6[s,v]<6)
      p6[,s,v,(nPassesBy6[s,v] + 1):6] <- NA
  }
}
p6.site.mean <- apply(p6, 2, mean, na.rm = T)
names(p6.site.mean) <- sites
p6.site.mean

###########################################
# Start actually plotting
###########################################

N.plot1 <- ggplot(subset(both.est, Parameter == "N" | Parameter == "Max Count" & Site != "All"), 
                  aes(x = Site, y = mean, ymin = X2.5., ymax = X97.5., color = Analysis, shape = Analysis))+
  geom_pointrange(size = 0.25, position = position_dodge(0.4)) + facet_grid(Visit ~ ., labeller = label_both) +
  labs(x="Site",y="Estimated Abundance")+theme_bw() +  scale_color_brewer(palette = 'Paired') 
png(file = paste0('POI_est_N_17-19_by_site_compare_', format(Sys.Date(), '%Y-%m-%d'), '.png'), width = 11, height = 7, units = 'in', res = 1200)
N.plot1
dev.off()

N.plot1 <- ggplot(subset(full.bb.est, Parameter == "N" | Parameter == "Max Count"), 
                  aes(x = Site, y = mean, ymin = X2.5., ymax = X97.5., color = Parameter))+
  geom_pointrange(size = 0.25, position = position_dodge(0.2)) + facet_grid(Visit ~ ., labeller = label_both) +
  labs(x="Site",y="Estimated Abundance")+theme_bw() +  scale_color_brewer(palette = 'Dark2') 
png(file = paste0('POI_est_N_17-19_10min_by_site_', format(Sys.Date(), '%Y-%m-%d'), '.png'), width = 14.5, height = 7, units = 'in', res = 1200)
N.plot1
dev.off()

N.plot4 <- ggplot(subset(both.est.6min, Parameter == "N" | Parameter == "Max Count"), 
                  aes(x = Site, y = mean, ymin = X2.5., ymax = X97.5., color = Analysis))+
  geom_pointrange(size = 0.25, position = position_dodge(0.2)) + facet_grid(Visit ~ ., labeller = label_both) +
  labs(x="Site",y="Estimated Abundance")+theme_bw() + scale_color_brewer(palette = 'Dark2') 
png(file = paste0('POI_est_N_17-19_6min_by_site_p-model_', format(Sys.Date(), '%Y-%m-%d'), '.png'), width = 14.5, height = 7, units = 'in', res = 1200)
N.plot4
dev.off()

N.plot4 <- ggplot(subset(bb.est.6min, Parameter == "N" | Parameter == "Max Count"), 
                  aes(x = Site, y = mean, ymin = X2.5., ymax = X97.5., color = Parameter))+
  geom_pointrange(size = 0.25, position = position_dodge(0.2)) + facet_grid(Visit ~ ., labeller = label_both) +
  labs(x="Site",y="Estimated Abundance")+theme_bw() + scale_color_brewer(palette = 'Dark2') 
png(file = paste0('POI_est_N_17-19_6min_by_site_', format(Sys.Date(), '%Y-%m-%d'), '.png'), width = 14.5, height = 7, units = 'in', res = 1200)
N.plot4
dev.off()

# N.plot4.5 <- ggplot(subset(full.both.est, Parameter == "N" & Site != "Canal 2" & Site != "Canal 7"), 
#                   aes(x = Site, y = mean, ymin = X2.5., ymax = X97.5., color = Analysis))+
#   geom_pointrange(size = 0.25, position = position_dodge(0.2)) + facet_grid(Visit ~ ., labeller = label_both) +
#   labs(x="Site",y="Estimated Abundance")+theme_bw() +  scale_color_brewer(palette = 'Dark2') 
# png(file = 'POI_est_N_16-18_by_small_site_compare3.png', width = 14, height = 7, units = 'in', res = 1200)
# N.plot4.5
# dev.off()
# 
# N.plot2 <- ggplot(subset(full.bb.est, 
#                          (Parameter == "N" | Parameter == "Max Count") & Site != "Canal 2" & Site != "Canal 7"), 
#                   aes(x = Site, y = mean, ymin = X2.5., ymax = X97.5., color = Parameter))+
#   geom_pointrange(size = 0.25, position = position_dodge(0.2)) + facet_grid(Visit ~ ., labeller = label_both) +
#   labs(x="Site",y="Estimated Abundance")+theme_bw() + scale_color_manual(values = c('#d8b365', '#5ab4ac'))
# png(file = 'POI.est.small.N.2016-18.by.site3.png', width = 14, height = 7, units = 'in', res = 1200)
# N.plot2
# dev.off()
# 
# N.plot2.5 <- ggplot(subset(full.bb.est, 
#                            (Parameter == "N" | Parameter == "Max Count") & 
#                              !(Site %in% c("Canal 2", "Canal 3", "Canal 7", "Canal 9", "Canal 12", 
#                                            "East River", "Wootens", "Big Cypress"))), 
#                     aes(x = Site, y = mean, ymin = X2.5., ymax = X97.5., color = Parameter))+
#   geom_pointrange(size = 0.25, position = position_dodge(0.2)) + facet_grid(Visit ~ ., labeller = label_both) +
#   labs(x="Site",y="Estimated Abundance")+theme_bw() 
# png(file = 'POI.est.very.small.N.2016.by.site3.png', width = 14, height = 7, units = 'in', res = 1200)
# N.plot2.5
# dev.off()
# 
N.plot3 <- ggplot(subset(full.both.est, Parameter == "N POI" | Parameter == "N total"),
                  aes(x = Visit, y = mean, ymin = X2.5., ymax = X97.5., color = Analysis))+ #, color = Parameter
  geom_pointrange(size = 0.25, position = position_dodge(0.2)) + ylim(c(0,NA)) +
  labs(x="Visit",y="Estimated Total Abundance")+theme_bw() + scale_color_brewer(palette = 'Dark2')
png(file = paste0('POI_est_N_total_17-19_10min_compare_', format(Sys.Date(), '%Y-%m-%d'), '.png'), width = 7, height = 5, units = 'in', res = 1200)
N.plot3
dev.off()

N.plot3 <- ggplot(subset(both.est, Parameter == "Max Count" & Site == "All" | Parameter == "N total"),
                  aes(x = Visit, y = mean, ymin = X2.5., ymax = X97.5., color = Analysis, shape = Analysis))+ #, color = Parameter
  geom_pointrange(size = 0.5, position = position_dodge(0.4)) + ylim(c(0,NA)) +
  labs(x="Visit",y="Estimated Total Abundance")+theme_bw() + scale_color_brewer(palette = 'Paired')
png(file = paste0('POI_est_N_total_17-19_compare_', format(Sys.Date(), '%Y-%m-%d'), '.png'), width = 7, height = 5, units = 'in', res = 1200)
N.plot3
dev.off()

N.plot3 <- ggplot(subset(both.est.6min, Parameter == "N POI" | Parameter == "N total"),
                  aes(x = Visit, y = mean, ymin = X2.5., ymax = X97.5., color = Analysis))+ #, color = Parameter
  geom_pointrange(size = 0.25, position = position_dodge(0.2)) + ylim(c(0,NA)) +
  labs(x="Visit",y="Estimated Total Abundance")+theme_bw() + scale_color_brewer(palette = 'Dark2')
png(file = paste0('POI_est_N_total_17-19_6min_compare_', format(Sys.Date(), '%Y-%m-%d'), '.png'), width = 7, height = 5, units = 'in', res = 1200)
N.plot3
dev.off()

# lam.plot1 <- ggplot(subset(full.bb.est, Parameter == "lambda"), 
#                     aes(x = Visit, y = mean, ymin = X2.5., ymax = X97.5.))+
#   geom_pointrange(color = 'purple') + ylim(0, NA) +
#   labs(y="Growth Estimate")+theme_bw() 
# png(file = paste0('POI_17-19_lambda_', format(Sys.Date(), '%Y-%m-%d'), '.png'), width = 5, height = 5, units = 'in', res = 1200)
# lam.plot1 + geom_hline(yintercept = 1, lty = 2, color='forestgreen')
# dev.off()

# par.plot1 <- ggplot(subset(full.bb.est, Parameter == "p mean" | Parameter == "rho"), 
#                     aes(x = Visit, y = mean, ymin = X2.5., ymax = X97.5., color = Parameter))+
#   geom_pointrange(position = position_dodge(0.2)) + ylim(0, 1) +
#   labs(y="Parameter Estimate")+theme_bw() + scale_color_brewer(palette = 'Set2')
# png(file = paste0('POI_17-19_detect_params_', format(Sys.Date(), '%Y-%m-%d'), '.png'), width = 5, height = 5, units = 'in', res = 1200)
# par.plot1
# dev.off()

par.plot1 <- ggplot(subset(full.both.est, Parameter == "p mean"), 
                    aes(x = Visit, y = mean, ymin = X2.5., ymax = X97.5., color = Analysis))+
  geom_pointrange(position = position_dodge(0.3)) + ylim(0, 1) +
  labs(y="Average Detection Probability Per Minute")+theme_bw() + scale_color_brewer(palette = 'Dark2')
png(file = paste0('POI_17-19_10min_mean_p_compare_', format(Sys.Date(), '%Y-%m-%d'), '.png'), width = 5, height = 4, units = 'in', res = 1200)
par.plot1
dev.off()

par.plot1 <- ggplot(subset(both.est.6min, Parameter == "p mean"), 
                    aes(x = Visit, y = mean, ymin = X2.5., ymax = X97.5., color = Analysis))+
  geom_pointrange(position = position_dodge(0.3)) + ylim(0, 1) +
  labs(y="Average Detection Probability Per Minute")+theme_bw() + scale_color_brewer(palette = 'Dark2')
png(file = paste0('POI_17-19_6min_mean_p_compare_', format(Sys.Date(), '%Y-%m-%d'), '.png'), width = 5, height = 4, units = 'in', res = 1200)
par.plot1
dev.off()

par.plot1 <- ggplot(subset(both.est, Parameter == "rho"), 
                    aes(x = Visit, y = mean, ymin = X2.5., ymax = X97.5., color = Analysis))+
  geom_pointrange(position = position_dodge(0.3)) + ylim(0, NA) +
  labs(y="Correlation in Availability")+theme_bw() + scale_color_brewer(palette = 'Dark2')
png(file = paste0('POI_17-19_rho_compare_', format(Sys.Date(), '%Y-%m-%d'), '.png'), width = 5, height = 4, units = 'in', res = 1200)
par.plot1
dev.off()

par.plot1 <- ggplot(subset(rand.both.est, Parameter == "p sd"), 
                    aes(x = Visit, y = mean, ymin = X2.5., ymax = X97.5., color = Analysis))+
  geom_pointrange(position = position_dodge(0.3)) + ylim(0, NA) +
  labs(y="Detection Variability")+theme_bw() + scale_color_brewer(palette = 'Dark2')
png(file = paste0('POI_17-19_p_sd_compare_', format(Sys.Date(), '%Y-%m-%d'), '.png'), width = 5, height = 4, units = 'in', res = 1200)
par.plot1
dev.off()


p.plot1 <- ggplot(subset(full.rand.est, Parameter == "p"), 
                  aes(x = Visit, y = mean, ymin = X2.5., ymax = X97.5., color = Pass))+
  geom_pointrange(size = 0.25, position = position_dodge(0.5)) + 
  labs(x="Visit",y="Estimated Detection / Availability")+facet_wrap(~ Site, nrow = 4)+ theme_bw() + 
  scale_color_manual(values = c('black', '#500000', '#800026', '#bd0026', '#e31a1c', '#fc4e2a', '#fd8d3c', '#feb24c', '#fed976', '#ffeda0')) #, '#ffffcc'
png(file = paste0('POI_17-19_individual_p_rand10_', format(Sys.Date(), '%Y-%m-%d'), '.png'), width = 11, height = 8, units = 'in', res = 1200)
p.plot1
dev.off()

p.est <- subset(both.est, Parameter == "p")
summary(p.est$Site)
p.est$Site <- factor(p.est$Site, levels = sites2, labels = sites3)
p.plot1 <- ggplot(subset(p.est, Analysis == '1st 6 Minutes'), 
                  aes(x = Visit, y = mean, ymin = X2.5., ymax = X97.5., color = Pass))+
  geom_pointrange(size = 0.25, position = position_dodge(0.5)) + 
  labs(x="Visit",y="Estimated Detection Probability")+facet_wrap(~ Site, nrow = 4)+
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(color = guide_legend(title = "Minute")) +
  scale_color_manual(values = c('black', '#500000', '#800026', '#bd0026', '#e31a1c', '#fc4e2a')) #, '#ffffcc'
png(file = paste0('POI_17-19_individual_p_bb6_', format(Sys.Date(), '%Y-%m-%d'), '.png'), width = 8, height = 7, units = 'in', res = 1200)
p.plot1
dev.off()

p.plot1 <- ggplot(subset(p.est, Analysis == 'All 10 Minutes'), 
                  aes(x = Visit, y = mean, ymin = X2.5., ymax = X97.5., color = Pass))+
  geom_pointrange(size = 0.25, position = position_dodge(0.5)) + 
  labs(x="Visit",y="Estimated Detection Probability")+facet_wrap(~ Site, nrow = 4)+ 
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(color = guide_legend(title = "Minute")) +
  scale_color_manual(values = c('black', '#500000', '#800026', '#bd0026', '#e31a1c', '#fc4e2a', '#fd8d3c', '#feb24c', '#fed976', '#ffeda0')) #, '#ffffcc'
png(file = paste0('POI_17-19_individual_p_bb10_', format(Sys.Date(), '%Y-%m-%d'), '.png'), width = 8, height = 7, units = 'in', res = 1200)
p.plot1
dev.off()

p.plot1 <- ggplot(subset(rand.est.6min, Parameter == "p"), 
                  aes(x = Visit, y = mean, ymin = X2.5., ymax = X97.5., color = Pass))+
  geom_pointrange(size = 0.25, position = position_dodge(0.5)) + 
  labs(x="Visit",y="Estimated Detection / Availability")+facet_wrap(~ Site, nrow = 4)+ theme_bw() + 
  scale_color_manual(values = c('black', '#500000', '#800026', '#bd0026', '#e31a1c', '#fc4e2a')) #, '#ffffcc'
png(file = paste0('POI_17-19_individual_p_rand6_', format(Sys.Date(), '%Y-%m-%d'), '.png'), width = 10, height = 8, units = 'in', res = 1200)
p.plot1
dev.off()

p.plot1 <- ggplot(subset(full.both.est, Parameter == "p" & Visit == '2019-1-31'), 
                  aes(x = Pass, y = mean, ymin = X2.5., ymax = X97.5., color = Analysis)) +
  geom_pointrange(size = 0.25, position = position_dodge(0.5)) + 
  labs(x="Pass",y="Estimated Detection / Availability") + theme_bw() + facet_wrap(~ Site, nrow = 4)+
  scale_color_brewer(type = 'qual', palette = 'Dark2')
png(file = 'POI_2019_individual_p10_compare.png', width = 12, height = 7, units = 'in', res = 1200)
p.plot1
dev.off()

p.plot1 <- ggplot(subset(both.est.6min, Parameter == "p" & Visit == '2019-1-31'), 
                  aes(x = Pass, y = mean, ymin = X2.5., ymax = X97.5., color = Analysis)) +
  geom_pointrange(size = 0.25, position = position_dodge(0.5)) + 
  labs(x="Pass",y="Estimated Detection / Availability") + theme_bw() + facet_wrap(~ Site, nrow = 4)+
  scale_color_brewer(type = 'qual', palette = 'Dark2')
png(file = 'POI_2019_individual_p6_compare.png', width = 10, height = 7, units = 'in', res = 1200)
p.plot1
dev.off()

nPassesByDF <- data.frame(adply(nPassesBy, 1:2, .id = c('Site', 'Visit')), X2.5. = 1.05, X97.5. = 1.05)
nPassesByDF <- subset(nPassesByDF, (Site %in% sitesAllYears) | Visit == visits[4])
nPassesBy6DF <- data.frame(adply(nPassesBy6, 1:2, .id = c('Site', 'Visit')), X2.5. = 1.05, X97.5. = 1.05)
nPassesBy6DF <- subset(nPassesBy6DF, (Site %in% sitesAllYears) | Visit == visits[4])
p.plot2 <- ggplot(subset(both.est, Parameter == "p*" & Analysis == 'All 10 Minutes'), 
                  aes(x = Site, y = mean, ymin = X2.5., ymax = X97.5., color = Visit, shape = Visit))+
  geom_pointrange(size = 0.5, position = position_dodge(0.8)) + 
  labs(x="Site",y="Estimated Cumulative Detection Probability") + theme_bw() +
  scale_color_manual(values = c('#66c2a4', '#41ae76', '#238b45', '#005824')) +
  geom_text(aes(label = V1), 
            data = nPassesByDF, size = 2.5,
            y = 1.025, position = position_dodge(0.8))
png(file = paste0('POI_17-19_site_level_p_bb10_', format(Sys.Date(), '%Y-%m-%d'), '.png'), width = 11, height = 7, units = 'in', res = 1200)
p.plot2
dev.off()

p.plot2 <- ggplot(subset(both.est, Parameter == "p*" & Analysis == '1st 6 Minutes'), 
                  aes(x = Site, y = mean, ymin = X2.5., ymax = X97.5., color = Visit, shape = Visit))+
  geom_pointrange(size = 0.5, position = position_dodge(0.8)) + 
  labs(x="Site",y="Estimated Cumulative Detection Probability") + theme_bw() +
  scale_color_manual(values = c('#66c2a4', '#41ae76', '#238b45', '#005824')) +
  geom_text(aes(label = V1), 
            data = nPassesBy6DF, size = 2.5,
            y = 1.025, position = position_dodge(0.8))
png(file = paste0('POI_17-19_site_level_p_bb6_', format(Sys.Date(), '%Y-%m-%d'), '.png'), width = 11, height = 7, units = 'in', res = 1200)
p.plot2
dev.off()

p.plot2 <- ggplot(subset(full.both.est, Parameter == "p*" & Analysis == "Logit Normal"), 
                  aes(x = Site, y = mean, ymin = X2.5., ymax = X97.5., color = Visit))+
  geom_pointrange(size = 0.5, position = position_dodge(0.8)) + 
  labs(x="Site",y="Estimated Cumulative Detection / Availability") + theme_bw() +
  scale_color_manual(values = c('#bae4b3', '#74c476', '#31a354', '#006d2c')) +
  geom_text(aes(label = V1), 
            data = nPassesByDF, size = 2.5,
            y = 1.025, position = position_dodge(0.8))
png(file = paste0('POI_17-19_site_level_p_rand10_', format(Sys.Date(), '%Y-%m-%d'), '.png'), width = 14.5, height = 7, units = 'in', res = 1200)
p.plot2
dev.off()

p.plot2 <- ggplot(subset(both.est.6min, Parameter == "p*" & Analysis == 'Logit Normal'), 
                  aes(x = Site, y = mean, ymin = X2.5., ymax = X97.5., color = Visit))+
  geom_pointrange(size = 0.5, position = position_dodge(0.8)) + 
  labs(x="Site",y="Estimated Cumulative Detection / Availability") + theme_bw() +
  scale_color_manual(values = c('#bae4b3', '#74c476', '#31a354', '#006d2c')) +
  geom_text(aes(label = V1), 
            data = nPassesBy6DF, size = 2.5,
            y = 1.025, position = position_dodge(0.8))
png(file = paste0('POI_17-19_site_level_p_rand6_', format(Sys.Date(), '%Y-%m-%d'), '.png'), width = 14.5, height = 7, units = 'in', res = 1200)
p.plot2
dev.off()

###########################################
# Output CSV files of model results
###########################################
write.csv(full.bb.est, 'bb.est.10min.csv', row.names = F)
write.csv(bb.est.6min, 'bb.est.6min.csv', row.names = F)
write.csv(full.rand.est, 'rand.est.10min.csv', row.names = F)
write.csv(rand.est.6min, 'rand.est.6min.csv', row.names = F)
write.csv(p0.est, 'p.visit.est.10min.csv', row.names = F)
write.csv(p0.est.6min, 'p.visit.est.6min.csv', row.names = F)

full.bb.est <- read.csv('bb.est.10min.csv')
bb.est.6min <- read.csv('bb.est.6min.csv')

summary(both.est$Site)
both.est <- transform(both.est,
                      Visit = factor(Visit, levels = visits),
                      # Parameter = factor(Parameter, levels = c('N', 'N total', 'N diff', 'lambda',
                      #                                          'p mean', 'rho', 'p sd', 'p', 'p*',
                      #                                          "Max Count")),
                      Pass = factor(Pass, levels = c(1:nPasses, 'All')),
                      Site = factor(Site, levels = c(sites, 'All'), labels = c(sites2, 'All')))

# Summarize differences in abundances and counts
summary(both.est$mean[both.est$Analysis=="1st 6 Minutes" & both.est$Parameter=="N"] - 
          both.est$mean[both.est$Analysis=="Count (6 Minutes)" & both.est$Parameter=="Max Count" & both.est$Site != "All"])
summary(both.est$mean[both.est$Analysis=="All 10 Minutes" & both.est$Parameter=="N"] - 
          both.est$mean[both.est$Analysis=="Count (All)" & both.est$Parameter=="Max Count" & both.est$Site != "All"])
(both.est$mean[both.est$Analysis=="1st 6 Minutes" & both.est$Parameter=="N total"] - 
          both.est$mean[both.est$Analysis=="Count (6 Minutes)" & both.est$Parameter=="Max Count" & both.est$Site == "All"])
summary(both.est$mean[both.est$Analysis=="All 10 Minutes" & both.est$Parameter=="N total"] - 
          both.est$mean[both.est$Analysis=="Count (All)" & both.est$Parameter=="Max Count" & both.est$Site == "All"])
