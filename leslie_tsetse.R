# Pupae survival
pup.death <- 0.05
pup.surv <- 0.95

# Adult survival
a.death <- 0.05 
ad.surv <- 0.95

# Fecundity
fec <- 0.5

# Initial size and pop matrix
N0 <- matrix(0, nrow=15, ncol=1)
N0[1,1] <- 1000 # Initial number of pupae
pop.mat <- matrix(0, nrow=15, ncol=15)

# Locations for demographic parameters
row.pup <- 2:7
col.pup <- 1:6
row.adult <- 8:15
col.adult <- 7:15

# Fill matrix for pupae
for(i in 1:6){
pop.mat[row.pup[i], col.pup[i]] <- pup.surv
}

# Fill matrix for adults
for(i in 1:8){
pop.mat[row.adult[i], col.adult[i]] <- adult.surv
}

# Fill fecundity
for(i in 11:15){
pop.mat[1,i] <- fec
}

# 100 year simulation
N[,1] <- N0
for(i in 1:99){
N[,i+1] <- pop.mat %*%  N[,i]
}

# Plot population
popn <- apply(N, 2, sum)
plot(1:100, popn, type="l")
