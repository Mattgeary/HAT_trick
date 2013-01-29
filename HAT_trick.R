

############################### Tsetse Demographics and leslie matrix ####################################################
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
for(i in 1:6){
pop.mat[row.pup[i], col.pup[i]] <- pup.surv
}

# Fill matrix for adults
for(i in 1:8){
pop.mat[row.adult[i], col.adult[i]] <- adult.surv
}

# Fill fecundity
for(i in min(breeding):max(breeding)){
pop.mat[1,i] <- fec
}

print(N0)
print(pop.mat)
##########################################################################################################################
#################################### Create habitat grid #################################################################
size <- 10 # Number of grid cells - move to initital stages eventually 
days <- 30 # Number of days to simulate - move to initial stages eventually
hab.grid <- matrix(0, ncol=size, nrow=size)

########### Add habitat data ###############

##########################################################################################################################
#################################### Create list of grid cells ###########################################################

cell.popn <- list()

for(i in 1:nrow(hab.grid)){
	cell.popn[[i]] <- list()
	for(j in 1:ncol(hab.grid)){
		cell.popn[[i]][[j]] <- N0
	}
}

popn.whole <- matrix(0,nrow = size^2, ncol=days+1) 

initial.pop <- list()

for(i in 1:size){
	initial.pop[[i]] <- sapply(cell.popn[[i]], sum)
}

initial.pop <- unlist(initial.pop)

popn.whole[,1] <- initial.pop

initial.pop <- matrix(initial.pop, nrow= size, ncol=size, byrow=T)

current.pop <- list()

pop.grid <- list()

pop.grid[[1]] <- initial.pop

current.grid <- matrix(NA, nrow=size, ncol=size)

##########################################################################################################################
######################################## Matrix multiplication ###########################################################

for(y in 1:days){
	for(i in 1:nrow(hab.grid)){
		for(j in 1:ncol(hab.grid)){
			cell.popn[[i]][[j]] <-  pop.mat %*% matrix(cell.popn[[i]][[j]], ncol=1)
			#hab.grid[i,j] <- sum(cell.popn[[i]][[j]])
			current.grid[i,j] <- sum(cell.popn[[i]][[j]])
		}
	
	}

	current.pop[[i]] <- sapply(cell.popn[[i]], sum) 
	popn.whole[, y +1] <- unlist(current.pop)
	#pop.grid[[y+1]] <- matrix(unlist(current.pop), nrow=size, ncol=size, byrow=T)
	pop.grid[[y+1]] <- current.grid
}
