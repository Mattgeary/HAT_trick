# Pupae survival
pup.death <- 0.05
pup.surv <- 0.95

# Adult survival
adult.death <- 0.05 
adult.surv <- 0.95

# Fecundity
fec <- 0.5
breeding <- c(11:15)

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
for(i in 1:length(row.pup)){
pop.mat[row.pup[i], col.pup[i]] <- pup.surv
}

# Fill matrix for adults
for(i in 1:length(row.adult)){
pop.mat[row.adult[i], col.adult[i]] <- adult.surv
}

# Fill fecundity
for(i in min(breeding):max(breeding)){
pop.mat[1,i] <- fec
}

N <- matrix(0, nrow=15, ncol=100)

# 100 year simulation
N[,1] <- N0
for(i in 1:99){
N[,i+1] <- pop.mat %*%  N[,i]
}

# Plot population
popn <- apply(N, 2, sum)
plot(1:100, popn, type="l")
