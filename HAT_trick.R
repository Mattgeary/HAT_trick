

############################### Tsetse Demographics and leslie matrix ####################################################
# Pupae survival
pup.death <- 0.05
pup.surv <- 0.95

# Adult survival
adult.death <- 0.05 
adult.surv <- 0.95
feed.cycle = 3

# Locations for demographic parameters
row.pup <- 2:7
col.pup <- 1:6
max.age <- (max(row.pup) + (feed.cycle * 2) + 3)
row.adult <- 8:max.age
col.adult <- 7:max.age

# Fecundity
fec <- 0.5
breeding <- c(11:max.age)

# Initial size and pop matrix
N0 <- matrix(0, nrow=max.age, ncol=1)
N0[1,1] <- 1000 # Initial number of pupae
pop.mat <- matrix(0, nrow=max.age, ncol=max.age)


# Movement probability
move.prob <- matrix(c(0, 0.00898,0.01250,0.01671,0.02170,0.02754,0.03423,0.04179,0.05019,0.05944,0.08043,rep(0.08043, 5)), ncol=1)

# Infection probability - should eventually be a column matrix
#infect <- 0.1
infect <- list(probe = data.frame("human" = 0.1, "OE" = 0.1), feed = data.frame("human" = 0.1, "OE"= 0.1), mature = data.frame("tsetse" = 0.033, "human" = 0.05, "OE" = 0.05), recover = data.frame("human" = 0.05, "OE" = 0.1)) 

# Fill matrix for pupae
for(i in 1:length(row.pup)){
pop.mat[row.pup[i], col.pup[i]] <- pup.surv
}

# Fill matrix for adults
for(i in 1:length(row.adult)){
pop.mat[row.adult[i], col.adult[i]] <- adult.surv
}

pop.mat[max(row.adult), max(col.adult)] <- adult.surv

# Fill fecundity
for(i in min(breeding):max(breeding)){
pop.mat[1,i] <- fec
}

#print(N0)
#print(pop.mat)
##########################################################################################################################
#################################### Create habitat grid #################################################################
size <- 10 # Number of grid cells - move to initital stages eventually 
days <- 1*(30) # Number of days to simulate - move to initial stages eventually
hab.grid <- matrix(1, ncol=size, nrow=size)

########### Add habitat data ###############

##########################################################################################################################
############################### Prepare intial list of gid cells and empty matrices#######################################

## List to hold population sizes through each iteration #####
cell.popn <- list()

for(i in 1:nrow(hab.grid)){
	cell.popn[[i]] <- list()
	for(j in 1:ncol(hab.grid)){
		cell.popn[[i]][[j]] <- matrix(c(rep(0, (max.age * 3))), ncol=3)
	}
}

# Assign an intitial population to invididual or randomly chosen cells
rand.x <- sample(1:size, 10)
rand.y <- sample(1:size, 10)
for(i in 1:length(rand.x)){
cell.popn[[rand.x[i]]][[rand.y[i]]][,1] <- N0
}
#cell.popn[[1]][[1]][,1] <- N0

# Assign some intial infected flies #

rand.x <- sample(1:size, 10, replace=T)
rand.y <- sample(1:size, 10, replace=T)
rand.z <- sample(8:max.age, 10, replace=T)
rand.fly <- sample(20:200, 10, replace=T)
for(i in 1:length(rand.x)){
cell.popn[[rand.x[i]]][[rand.y[i]]][rand.z[i],3] <- rand.fly[i]
}


### Human population ###

human.popn <- list()

for(i in 1:nrow(hab.grid)){
	human.popn[[i]] <- list()
	for(j in 1:ncol(hab.grid)){
		human.popn[[i]][[j]] <- matrix(c(40,100,0,0,0,0,0,0), ncol=4)
	}
}

#### Ox Equivalent population ####


OE.popn <- list()

for(i in 1:nrow(hab.grid)){
	OE.popn[[i]] <- list()
	for(j in 1:ncol(hab.grid)){
		OE.popn[[i]][[j]] <- matrix(c(100,0,0,0), ncol=4)
	}
}

#### Matrix to store population sizes for each grid cell through the run ######
popn.whole <- matrix(0,nrow = size^2, ncol=days+1) 

detect.whole <- matrix(0,nrow = size^2, ncol=days)

visit.whole <- matrix(0,nrow = size^2, ncol=days)

probe.whole <- matrix(0,nrow = size^2, ncol=days) 

feed.whole <- matrix(0,nrow = size^2, ncol=days)   


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

current.detect <- list()
current.visit <- list()
current.probe <- list()
current.feed <- list()

pop.grid <- list() ### List storing maps of the population during each run

pop.grid[[1]] <- initial.pop ### First map in pop.grid set to be the initial population size



infection.grid <- list() ### List storing maps of the infected population during each run

infection.grid[[1]] <- initial.pop[,2] ### First map in infection.grid set to be the initial infected population size

current.grid <- matrix(NA, nrow=size, ncol=size) ### matrix to store the current population map during each run
current.grid.inf <- matrix(NA, nrow=size, ncol=size) ### matrix to store the current infected population map during each run

human.grid <- matrix(NA, nrow=size, ncol=size)
human.grid.inf <- matrix(NA, nrow=size, ncol=size) 

OE.grid <- matrix(NA, nrow=size, ncol=size)
OE.grid.inf <- matrix(NA, nrow=size, ncol=size) 

##### List to store movements of flies during each run #####
move <- list()
for(i in 1:nrow(hab.grid)){
	move[[i]] <- list()
	for(j in 1:ncol(hab.grid)){
		move[[i]][[j]] <- matrix(c(rep(0, (max.age * 3))), ncol=3)
	}
}

move.un <- list()
for(i in 1:nrow(hab.grid)){
	move.un[[i]] <- list()
	for(j in 1:ncol(hab.grid)){
		move.un[[i]][[j]] <- 0
	}
}

move.inc <- move.un
move.inf <- move.un

move.grid.list <- list() ### List to store maps of movement during each run

move.grid.list[[1]] <- matrix(0, nrow=nrow(hab.grid), ncol=ncol(hab.grid)) ### First movements (i.e. 0) stored

##########################################################################################################################
######################################## Matrix multiplication ###########################################################
source("movement.R")
source("feed.R")
source("infected.R")
source("demog.R")
pb <- txtProgressBar(min = 0, max = days, style = 3)

for(y in 1:days){
	cell.popn <- demog.tsetse(cell.popn, pop.mat, hab.grid, adult=min(row.adult))
	human.popn <- demog.human(human.popn, hab=hab.grid)

	hunger.cycle <- feed_fun(habitat = hab.grid, popn = cell.popn, human = human.popn, OE = OE.popn, feed.cycle = feed.cycle, adult = col.adult)	

	tryp <- infection(popn = hunger.cycle$popn, human = human.popn, OE = OE.popn, probe = hunger.cycle$probe, feed = hunger.cycle$feed,  transmission = infect, adult = row.adult)

	cell.popn <- tryp$popn
	cell.popn <- hunger.cycle$popn
	human.popn <- tryp$human
	OE.popn <- tryp$OE

	move.fun <- HAT_move(popn = cell.popn, move = move, move.prob = move.prob, hab.grid = hab.grid)
	move.grid.list[[y+1]] <- move.fun$move.grid
	
	for(i in 1:nrow(hab.grid)){
		for(j in 1:ncol(hab.grid)){
		cell.popn[[i]][[j]] <- move.fun$new.pop[[i]][[j]] + move.fun$movements[[i]][[j]]
		current.grid[i,j] <- sum(cell.popn[[i]][[j]])
    human.grid[i,j] <- sum(human.popn[[i]][[j]])
    current.grid.inf[i,j] <- sum(cell.popn[[i]][[j]][,3])
		human.grid.inf[i,j] <- sum(human.popn[[i]][[j]][,3])
		OE.grid[i,j] <- sum(OE.popn[[i]][[j]])
    OE.grid.inf[i,j] <- sum(OE.popn[[i]][[j]][,3])
		}
		current.pop[[i]] <- sapply(cell.popn[[i]], sum)
		#current.detect[[i]] <- sapply(hunger.cycle$detect[[i]], sum)
		#current.visit[[i]] <- sapply(hunger.cycle$visit[[i]], sum)
		#current.probe[[i]] <- sapply(hunger.cycle$probe[[i]], sum)
		#current.feed[[i]] <- sapply(hunger.cycle$feed[[i]], sum)
	}

	popn.whole[,y +1] <- unlist(current.pop)
	#detect.whole[,y] <- unlist(current.detect)	
	#visit.whole[,y] <- unlist(current.visit)
	#probe.whole[,y] <- unlist(current.probe)	
	#feed.whole[,y] <- unlist(current.feed)		
	pop.grid[[y+1]] <- current.grid
  oldpar <- par(mfrow=c(2,2), mar=c(4,1,2,1))
  image(current.grid, main="Tsetse popn")
	infection.grid[[y+1]] <- current.grid.inf
  image(current.grid.inf, main="Infected tsetse")
	image(human.grid.inf, main="Infected humans")
	image(OE.grid.inf, main="Infected Ox Equivalents")
  par(oldpar)
	setTxtProgressBar(pb, y)
}

