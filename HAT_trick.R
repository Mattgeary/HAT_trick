

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

# Movement probability
move.prob <- matrix(c(0, 0.00898,0.01250,0.01671,0.02170,0.02754,0.03423,0.04179,0.05019,0.05944,0.08043,rep(0.08043, 4)), ncol=1)

# Infection probability - should eventually be a column matrix
infect <- 0.1

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
days <- 100 # Number of days to simulate - move to initial stages eventually
hab.grid <- matrix(1, ncol=size, nrow=size)

########### Add habitat data ###############

##########################################################################################################################
############################### Prepare intial list of gid cells and empty matrices#######################################

## List to hold population sizes through each iteration #####
cell.popn <- list()

for(i in 1:nrow(hab.grid)){
	cell.popn[[i]] <- list()
	for(j in 1:ncol(hab.grid)){
		cell.popn[[i]][[j]] <- matrix(c(rep(0, max(row.adult) * 2)), ncol=2)
	}
}

cell.popn[[1]][[1]][,1] <- N0

#### Matrix to store population sizes for each grid cell through the run ######
popn.whole <- matrix(0,nrow = size^2, ncol=days+1) 


##### Calculate initial population size for each grid cell and store in the first column of popn.whole #####
initial.pop <- list()

for(i in 1:size){
	initial.pop[[i]] <- sapply(cell.popn[[i]], sum)
}

initial.pop <- unlist(initial.pop)

popn.whole[,1] <- initial.pop

##### Create map of initial population size #######

initial.pop <- matrix(initial.pop, nrow= size, ncol=size, byrow=T)

#### Declare variables for inside model#######

current.pop <- list() ### Population calculated during each run

pop.grid <- list() ### List storing maps of the population during each run

pop.grid[[1]] <- initial.pop ### First map in pop.grid set to be the initial population size

infection.grid <- list() ### List storing maps of the infected population during each run

infection.grid[[1]] <- initial.pop[,2] ### First map in infection.grid set to be the initial infected population size

current.grid <- matrix(NA, nrow=size, ncol=size) ### matrix to store the current population map during each run
current.grid.inf <- matrix(NA, nrow=size, ncol=size) ### matrix to store the current infected population map during each run
##### List to store movements of flies during each run #####
move <- list()
for(i in 1:nrow(hab.grid)){
	move[[i]] <- list()
	for(j in 1:ncol(hab.grid)){
		move[[i]][[j]] <- matrix(c(rep(0, max(row.adult) * 2)), ncol=2)
	}
}

move.un <- list()
for(i in 1:nrow(hab.grid)){
	move.un[[i]] <- list()
	for(j in 1:ncol(hab.grid)){
		move.un[[i]][[j]] <- 0
	}
}

move.inf <- move.un

move.grid.list <- list() ### List to store maps of movement during each run

move.grid.list[[1]] <- matrix(0, nrow=nrow(hab.grid), ncol=ncol(hab.grid)) ### First movements (i.e. 0) stored

##########################################################################################################################
######################################## Matrix multiplication ###########################################################
source("movement.R")
source("infected.R")
for(y in 1:days){
	for(i in 1:nrow(hab.grid)){
		for(j in 1:ncol(hab.grid)){
			cell.popn[[i]][[j]] <-  pop.mat %*% matrix(cell.popn[[i]][[j]], ncol=2)
		}
	}
	tryp <- infection(cell.popn, pop.mat, infect, row.pup, row.adult)
	cell.popn <- tryp$cell.popn
	move.fun <- HAT_move(cell.popn, move, move.prob, hab.grid)
	move.grid.list[[y+1]] <- move.fun$move.grid
	for(i in 1:nrow(hab.grid)){
		for(j in 1:ncol(hab.grid)){
		cell.popn[[i]][[j]] <- move.fun$new.pop[[i]][[j]] + move.fun$movements[[i]][[j]]
		current.grid[i,j] <- sum(cell.popn[[i]][[j]])
		current.grid.inf[i,j] <- sum(cell.popn[[i]][[j]][,2])
		}
		current.pop[[i]] <- sapply(cell.popn[[i]], sum)
	}
	popn.whole[,y +1] <- unlist(current.pop)
	pop.grid[[y+1]] <- current.grid
	infection.grid[[y+1]] <- current.grid.inf
	print(c("			DAY => ", y))
}

