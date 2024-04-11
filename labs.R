# lab problem 1

library(MASS)
library(lmerTest)
N = 40
p.temp.vector = c()
# We repeat simulation for 500 times for each N (Step 3)
for (i in 1:500) {
  # Set seed
  set.seed(i)
  # Step 1: generate data
  # Generate id
  dat = data.frame(id = paste0(615,1:N))
  # Long format: each subject was followed by 8 days
  dat = as.data.frame(dat[rep(1:N, each = 8), ])
  names(dat) = 'id'
  # Make Day variable
  dat$Days = rep(1:8, times = N)
  # Simulate random error
  dat$err = rnorm(8*N,mean=0,sd=10)
  
  # Simulate (correlated) subject-level random effects for intercepts and slopes
  ## Covariance matrix
  S1 = diag(c(24.741, 5.922)) %*% matrix(c(1, 0, 0, 1), nrow=2) %*% diag(c(24.741, 5.922))
  ## Simulate realization of random intercept and slope
  U1 = as.data.frame(mvrnorm(N, mu=c(0,0), Sigma=S1))
  ## Add identifier (subject id)
  U1$id = paste0(615,1:N)
  ## Merge subject-level random effects back to data
  dat = merge(dat,U1,by='id')
  
  # Simulate the outcome: Reaction_ij
  dat$Reaction = (251.405 + dat$V1) + (5 + dat$V2)*dat$Days + dat$err
  
  # Step 2: test the null hypothesis
  mod = lmer(Reaction ~ Days + (Days | id), dat)
  p.value = summary(mod)$coef["Days","Pr(>|t|)"]
  # Save p value
  p.temp.vector = c(p.temp.vector,p.value)
}
# power
mean(p.temp.vector<0.05)









# lab problem 2

N = 40
beta.vector = c()
# We repeat simulation for 500 times for each N (Step 3)
for (i in 1:200) {
  # Set seed
  set.seed(i)
  # Step 1: generate data
  # Generate id
  dat = data.frame(id = paste0(615,1:N))
  # Long format: each subject was followed by 8 days
  dat = as.data.frame(dat[rep(1:N, each = 8), ])
  names(dat) = 'id'
  # Make Day variable
  dat$Days = rep(1:8, times = N)
  # Simulate random error
  dat$err = rnorm(8*N,mean=0,sd=30)
  
  # Simulate (correlated) subject-level random effects for intercepts and slopes
  ## Covariance matrix
  S1 = diag(c(24.741, 5.922)) %*% matrix(c(1, 0, 0, 1), nrow=2) %*% diag(c(24.741, 5.922))
  ## Simulate realization of random intercept and slope
  U1 = as.data.frame(mvrnorm(N, mu=c(0,0), Sigma=S1))
  ## Add identifier (subject id)
  U1$id = paste0(615,1:N)
  ## Merge subject-level random effects back to data
  dat = merge(dat,U1,by='id')
  
  # Simulate the outcome: Reaction_ij
  dat$Reaction = (251.405 + dat$V1) + (3 + dat$V2)*dat$Days + dat$err
  
  # Step 2: test the null hypothesis
  mod = lmer(Reaction ~ Days + (Days | id), dat)
  # Save beta3
  beta.vector = c(beta.vector,summary(mod)$coef['Days','Estimate'])
}
# plot distribution
hist(beta.vector,breaks=20)

