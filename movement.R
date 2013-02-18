HAT_move <- function(popn, move, move.prob, hab.grid){
	for(i in 1:nrow(hab.grid)){
		popn.un[[i]] <- list()
		popn.inf[[i]] <- list()
		for(j in 1:ncol(hab.grid)){
			popn.un[[i]][[j]] <- matrix(c(popn[[i]][[j]][,1]), ncol=1)
			popn.inf[[i]][[j]] <- matrix(c(popn[[i]][[j]][,2]), ncol=1)
		}
	}
	popn.orig.un <- pop.un
	popn.orig inf <- pop.inf

####### Uninfected #######
	for(i in 1:nrow(hab.grid)){
		for(j in 1:ncol(hab.grid)){
			if((i-1) <= 0 | (j-1) <=0 | i >= nrow(hab.grid) | j >= ncol(hab.grid)){
				print("yes")
				if(i-1 <= 0 & j-1 <= 0){
					move.un[[i]][[j]]	<- move.un[[i]][[j]] + ((popn.un[[(i+1)]][[j]] * move.prob) * (hab.grid[i+1,j]/hab.grid[i,j])  + (pop.orig.un[[(i)]][[j+1]]* move.prob) * (hab.grid[i+1,j]/hab.grid[i,j]))
					move.c <- (pop.orig.un[[i]][[j]] * move.prob) * (hab.grid[i+1,j]/hab.grid[i,j])
					move.d <- (pop.orig.un[[i]][[j]] * move.prob) * (hab.grid[i,j+1]/hab.grid[i,j])
					move.un[[(i+1)]][[j]] <- move.un[[(i+1)]][[j]] + move.c
					move.un[[i]][[(j+1)]] <- move.un[[i]][[(j+1)]] + move.d
					popn.un[[i]][[j]] <- popn.un[[i]][[j]] - (move.c + move.d) * 2
					print("i<=0 & j<=0")
				}
				if(i-1 <= 0 & j >= ncol(hab.grid)){
					move.un[[i]][[j]]	<- move.un[[i]][[j]] + ((pop.orig.un[[(i+1)]][[j]]* move.prob) * (hab.grid[i+1,j]/hab.grid[i,j]) + (pop.orig.un[[(i)]][[j-1]]* move.prob) * (hab.grid[i,j-1]/hab.grid[i,j]))
					move.c <- (pop.orig.un[[i]][[j]] * move.prob) * (hab.grid[i+1,j]/hab.grid[i,j])
					move.a <- (pop.orig.un[[i]][[j]] * move.prob) * (hab.grid[i,j-1]/hab.grid[i,j])
					move.un[[(i+1)]][[j]] <- move.un[[(i+1)]][[j]] + move.c
					move.un[[i]][[(j-1)]] <- move.un[[i]][[(j-1)]] + move.a 
					popn.un[[i]][[j]] <- popn.un[[i]][[j]] - (move.c + move.a) * 2
					print("i<=0 & j>=10")
				}
				if(i >= nrow(hab.grid) & (j-1) <= 0){
					move.un[[i]][[j]]	<- move.un[[i]][[j]] + ((pop.orig.un[[(i-1)]][[j]]* move.prob)* (hab.grid[i-1,j]/hab.grid[i,j]) + (pop.orig.un[[(i)]][[j+1]]* move.prob) * (hab.grid[i,j+1]/hab.grid[i,j]))
					move.b <- (pop.orig.un[[i]][[j]] * move.prob) * (hab.grid[i-1,j]/hab.grid[i,j])	
					move.d <- (pop.orig.un[[i]][[j]] * move.prob) * (hab.grid[i,j+1]/hab.grid[i,j])
					move.un[[(i-1)]][[j]] <- move.un[[(i-1)]][[j]] + move.b
					move.un[[i]][[(j+1)]] <- move.un[[i]][[(j+1)]] + move.d
					popn.un[[i]][[j]] <- popn.un[[i]][[j]] - (move.b + move.d) * 2
					print("i>=10 & j<=0")
				}
				if(i >= nrow(hab.grid) & j >= ncol(hab.grid)){
					move.un[[i]][[j]]	<- move.un[[i]][[j]] + ((pop.orig.un[[(i-1)]][[j]]* move.prob) * (hab.grid[i-1,j]/hab.grid[i,j])+ (pop.orig.un[[(i)]][[j-1]]* move.prob) * (hab.grid[i,j-1]/hab.grid[i,j]))
					move.b <- (pop.orig.un[[i]][[j]] * move.prob) * (hab.grid[i-1,j]/hab.grid[i,j])
					move.a <- (pop.orig.un[[i]][[j]] * move.prob) * (hab.grid[i,j-1]/hab.grid[i,j])
					move.un[[(i-1)]][[j]] <- move.un[[(i-1)]][[j]] + move.b
					move.un[[i]][[(j-1)]] <- move.un[[i]][[(j-1)]] + move.a
					popn.un[[i]][[j]] <- popn.un[[i]][[j]] - (move.b + move.a) * 2
					print("i>=10 & j>=10")
				}
				if(i-1 <= 0 & (j-1) > 0 & j < ncol(hab.grid)){
					move.un[[i]][[j]]	<- move.un[[i]][[j]] + (pop.orig.un[[(i+1)]][[j]]* move.prob) * (hab.grid[i+1,j]/hab.grid[i,j])
					move.a <- (pop.orig.un[[i]][[j]] * move.prob) * (hab.grid[i,j-1]/hab.grid[i,j])
					move.c <- (pop.orig.un[[i]][[j]] * move.prob) * (hab.grid[i+1,j]/hab.grid[i,j])
					move.d <- (pop.orig.un[[i]][[j]] * move.prob) * (hab.grid[i,j+1]/hab.grid[i,j])
	 				move.un[[i]][[(j-1)]] <- move.un[[i]][[(j-1)]] + move.a
					move.un[[(i+1)]][[j]] <- move.un[[(i+1)]][[j]] + move.c
					move.un[[i]][[(j+1)]] <- move.un[[i]][[(j+1)]] + move.d
					popn.un[[i]][[j]] <- popn.un[[i]][[j]] - (move.a + (move.c * 2) + move.d)
				}
				if(i >= nrow(hab.grid) & (j-1) > 0 & j < ncol(hab.grid)){
					move.un[[i]][[j]]	<- move.un[[i]][[j]] + (pop.orig.un[[(i-1)]][[j]] * move.prob)  * (hab.grid[i-1,j]/hab.grid[i,j])
					move.b <- (pop.orig.un[[i]][[j]] * move.prob) * (hab.grid[i-1,j]/hab.grid[i,j])
					move.a <- (pop.orig.un[[i]][[j]] * move.prob) * (hab.grid[i,j-1]/hab.grid[i,j])
					move.d <- (pop.orig.un[[i]][[j]] * move.prob) * (hab.grid[i,j+1]/hab.grid[i,j])
					move.un[[(i-1)]][[j]] <- move.un[[(i-1)]][[j]] + move.b
					move.un[[i]][[(j-1)]] <- move.un[[i]][[(j-1)]] + move.a
					move.un[[i]][[(j+1)]] <- move.un[[i]][[(j+1)]] + move.d
					popn.un[[i]][[j]] <- popn.un[[i]][[j]] - ((move.b *2) + move.a + move.d)
				}
				if(j-1 <= 0 & (i-1) > 0 & i < nrow(hab.grid)){
					move.un[[i]][[j]]	<- move.un[[i]][[j]] + (pop.orig.un[[i]][[(j+1)]] * move.prob) * (hab.grid[i,j+1]/hab.grid[i,j])
					move.b <- (pop.orig.un[[i]][[j]] * move.prob) * (hab.grid[i-1,j]/hab.grid[i,j])
					move.d <- (pop.orig.un[[i]][[j]] * move.prob) * (hab.grid[i,j+1]/hab.grid[i,j])
					move.c <- (pop.orig.un[[i]][[j]] * move.prob) * (hab.grid[i+1,j]/hab.grid[i,j])
					move.un[[(i-1)]][[j]] <- move.un[[(i-1)]][[j]] + move.b
					move.un[[i]][[(j+1)]] <- move.un[[i]][[(j+1)]] + move.d
					move.un[[(i+1)]][[j]] <- move.un[[(i+1)]][[j]] + move.c
					popn.un[[i]][[j]] <- popn.un[[i]][[j]] - (move.b + (move.d * 2) + move.c)
				}
				if(j >= ncol(hab.grid) & (i-1) > 0 & i < nrow(hab.grid)){
					move.un[[i]][[j]]	<- move.un[[i]][[j]] + (pop.orig.un[[i]][[(j-1)]] * move.prob) * (hab.grid[i,j-1]/hab.grid[i,j])
					move.b <- (pop.orig.un[[i]][[j]] * move.prob) * (hab.grid[i-1,j]/hab.grid[i,j])
					move.a <- (pop.orig.un[[i]][[j]] * move.prob) * (hab.grid[i,j-1]/hab.grid[i,j])
					move.c <- (pop.orig.un[[i]][[j]] * move.prob) * (hab.grid[i+1,j]/hab.grid[i,j])
					move.un[[(i-1)]][[j]] <- move.un[[(i-1)]][[j]] + move.b
					move.un[[i]][[(j-1)]] <- move.un[[i]][[(j-1)]] + move.a 
					move.un[[(i+1)]][[j]] <- move.un[[(i+1)]][[j]] + move.c 
					popn.un[[i]][[j]] <- popn.un[[i]][[j]] - (move.b + (move.a * 2) + move.c)
				}
				
			} else {
				move.a <- pop.orig.un[[i]][[j]] * move.prob * (hab.grid[i,j-1]/hab.grid[i,j])
				move.b <- pop.orig.un[[i]][[j]] * move.prob * (hab.grid[i-1,j]/hab.grid[i,j])
				move.c <- pop.orig.un[[i]][[j]] * move.prob * (hab.grid[i+1,j]/hab.grid[i,j])
				move.d <- pop.orig.un[[i]][[j]] * move.prob * (hab.grid[i,j+1]/hab.grid[i,j])
				move.un[[i]][[j-1]] <- move.un[[i]][[(j-1)]] + move.a
	 			move.un[[i-1]][[j]] <- move.un[[(i-1)]][[j]] + move.b
				move.un[[i+1]][[j]] <- move.un[[(i+1)]][[j]] + move.c
				move.un[[i]][[j+1]] <- move.un[[i]][[(j+1)]] + move.d
				popn.un[[i]][[j]] <- popn.un[[i]][[j]] - (move.a + move.b + move.c + move.d)
			}
		print(c(i,j))
		}
	print(i)
	}
	move.grid.un <- matrix(0, nrow=nrow(hab.grid), ncol=ncol(hab.grid))
	for(i in 1:nrow(hab.grid)){
		for(j in 1:ncol(hab.grid)){
			print(c(i,j))
			move.grid.un[i,j] <- sum(move.un[[i]][[j]])
		}
	}

############# Infected ###################
	for(i in 1:nrow(hab.grid)){
		for(j in 1:ncol(hab.grid)){
			if((i-1) <= 0 | (j-1) <=0 | i >= nrow(hab.grid) | j >= ncol(hab.grid)){
				print("yes")
				if(i-1 <= 0 & j-1 <= 0){
					move.inf[[i]][[j]]	<- move.inf[[i]][[j]] + ((popn.inf[[(i+1)]][[j]] * move.prob) * (hab.grid[i+1,j]/hab.grid[i,j])  + (popn.orig.inf[[(i)]][[j+1]]* move.prob) * (hab.grid[i+1,j]/hab.grid[i,j]))
					move.c <- (popn.orig.inf[[i]][[j]] * move.prob) * (hab.grid[i+1,j]/hab.grid[i,j])
					move.d <- (popn.orig.inf[[i]][[j]] * move.prob) * (hab.grid[i,j+1]/hab.grid[i,j])
					move.inf[[(i+1)]][[j]] <- move.inf[[(i+1)]][[j]] + move.c
					move.inf[[i]][[(j+1)]] <- move.inf[[i]][[(j+1)]] + move.d
					popn.inf[[i]][[j]] <- popn.inf[[i]][[j]] - (move.c + move.d) * 2
					print("i<=0 & j<=0")
				}
				if(i-1 <= 0 & j >= ncol(hab.grid)){
					move.inf[[i]][[j]]	<- move.inf[[i]][[j]] + ((popn.orig.inf[[(i+1)]][[j]]* move.prob) * (hab.grid[i+1,j]/hab.grid[i,j]) + (popn.orig.inf[[(i)]][[j-1]]* move.prob) * (hab.grid[i,j-1]/hab.grid[i,j]))
					move.c <- (popn.orig.inf[[i]][[j]] * move.prob) * (hab.grid[i+1,j]/hab.grid[i,j])
					move.a <- (popn.orig.inf[[i]][[j]] * move.prob) * (hab.grid[i,j-1]/hab.grid[i,j])
					move.inf[[(i+1)]][[j]] <- move.inf[[(i+1)]][[j]] + move.c
					move.inf[[i]][[(j-1)]] <- move.inf[[i]][[(j-1)]] + move.a 
					popn.inf[[i]][[j]] <- popn.inf[[i]][[j]] - (move.c + move.a) * 2
					print("i<=0 & j>=10")
				}
				if(i >= nrow(hab.grid) & (j-1) <= 0){
					move.inf[[i]][[j]]	<- move.inf[[i]][[j]] + ((popn.orig.inf[[(i-1)]][[j]]* move.prob)* (hab.grid[i-1,j]/hab.grid[i,j]) + (popn.orig.inf[[(i)]][[j+1]]* move.prob) * (hab.grid[i,j+1]/hab.grid[i,j]))
					move.b <- (popn.orig.inf[[i]][[j]] * move.prob) * (hab.grid[i-1,j]/hab.grid[i,j])	
					move.d <- (popn.orig.inf[[i]][[j]] * move.prob) * (hab.grid[i,j+1]/hab.grid[i,j])
					move.inf[[(i-1)]][[j]] <- move.inf[[(i-1)]][[j]] + move.b
					move.inf[[i]][[(j+1)]] <- move.inf[[i]][[(j+1)]] + move.d
					popn.inf[[i]][[j]] <- popn.inf[[i]][[j]] - (move.b + move.d) * 2
					print("i>=10 & j<=0")
				}
				if(i >= nrow(hab.grid) & j >= ncol(hab.grid)){
					move.inf[[i]][[j]]	<- move.inf[[i]][[j]] + ((popn.orig.inf[[(i-1)]][[j]]* move.prob) * (hab.grid[i-1,j]/hab.grid[i,j])+ (popn.orig.inf[[(i)]][[j-1]]* move.prob) * (hab.grid[i,j-1]/hab.grid[i,j]))
					move.b <- (popn.orig.inf[[i]][[j]] * move.prob) * (hab.grid[i-1,j]/hab.grid[i,j])
					move.a <- (popn.orig.inf[[i]][[j]] * move.prob) * (hab.grid[i,j-1]/hab.grid[i,j])
					move.inf[[(i-1)]][[j]] <- move.inf[[(i-1)]][[j]] + move.b
					move.inf[[i]][[(j-1)]] <- move.inf[[i]][[(j-1)]] + move.a
					popn.inf[[i]][[j]] <- popn.inf[[i]][[j]] - (move.b + move.a) * 2
					print("i>=10 & j>=10")
				}
				if(i-1 <= 0 & (j-1) > 0 & j < ncol(hab.grid)){
					move.inf[[i]][[j]]	<- move.inf[[i]][[j]] + (popn.orig.inf[[(i+1)]][[j]]* move.prob) * (hab.grid[i+1,j]/hab.grid[i,j])
					move.a <- (popn.orig.inf[[i]][[j]] * move.prob) * (hab.grid[i,j-1]/hab.grid[i,j])
					move.c <- (popn.orig.inf[[i]][[j]] * move.prob) * (hab.grid[i+1,j]/hab.grid[i,j])
					move.d <- (popn.orig.inf[[i]][[j]] * move.prob) * (hab.grid[i,j+1]/hab.grid[i,j])
	 				move.inf[[i]][[(j-1)]] <- move.inf[[i]][[(j-1)]] + move.a
					move.inf[[(i+1)]][[j]] <- move.inf[[(i+1)]][[j]] + move.c
					move.inf[[i]][[(j+1)]] <- move.inf[[i]][[(j+1)]] + move.d
					popn.inf[[i]][[j]] <- popn.inf[[i]][[j]] - (move.a + (move.c * 2) + move.d)
				}
				if(i >= nrow(hab.grid) & (j-1) > 0 & j < ncol(hab.grid)){
					move.inf[[i]][[j]]	<- move.inf[[i]][[j]] + (popn.orig.inf[[(i-1)]][[j]] * move.prob)  * (hab.grid[i-1,j]/hab.grid[i,j])
					move.b <- (popn.orig.inf[[i]][[j]] * move.prob) * (hab.grid[i-1,j]/hab.grid[i,j])
					move.a <- (popn.orig.inf[[i]][[j]] * move.prob) * (hab.grid[i,j-1]/hab.grid[i,j])
					move.d <- (popn.orig.inf[[i]][[j]] * move.prob) * (hab.grid[i,j+1]/hab.grid[i,j])
					move.inf[[(i-1)]][[j]] <- move.inf[[(i-1)]][[j]] + move.b
					move.inf[[i]][[(j-1)]] <- move.inf[[i]][[(j-1)]] + move.a
					move.inf[[i]][[(j+1)]] <- move.inf[[i]][[(j+1)]] + move.d
					popn.inf[[i]][[j]] <- popn.inf[[i]][[j]] - ((move.b *2) + move.a + move.d)
				}
				if(j-1 <= 0 & (i-1) > 0 & i < nrow(hab.grid)){
					move.inf[[i]][[j]]	<- move.inf[[i]][[j]] + (popn.orig.inf[[i]][[(j+1)]] * move.prob) * (hab.grid[i,j+1]/hab.grid[i,j])
					move.b <- (popn.orig.inf[[i]][[j]] * move.prob) * (hab.grid[i-1,j]/hab.grid[i,j])
					move.d <- (popn.orig.inf[[i]][[j]] * move.prob) * (hab.grid[i,j+1]/hab.grid[i,j])
					move.c <- (popn.orig.inf[[i]][[j]] * move.prob) * (hab.grid[i+1,j]/hab.grid[i,j])
					move.inf[[(i-1)]][[j]] <- move.inf[[(i-1)]][[j]] + move.b
					move.inf[[i]][[(j+1)]] <- move.inf[[i]][[(j+1)]] + move.d
					move.inf[[(i+1)]][[j]] <- move.inf[[(i+1)]][[j]] + move.c
					popn.inf[[i]][[j]] <- popn.inf[[i]][[j]] - (move.b + (move.d * 2) + move.c)
				}
				if(j >= ncol(hab.grid) & (i-1) > 0 & i < nrow(hab.grid)){
					move.inf[[i]][[j]]	<- move.inf[[i]][[j]] + (popn.orig.inf[[i]][[(j-1)]] * move.prob) * (hab.grid[i,j-1]/hab.grid[i,j])
					move.b <- (popn.orig.inf[[i]][[j]] * move.prob) * (hab.grid[i-1,j]/hab.grid[i,j])
					move.a <- (popn.orig.inf[[i]][[j]] * move.prob) * (hab.grid[i,j-1]/hab.grid[i,j])
					move.c <- (popn.orig.inf[[i]][[j]] * move.prob) * (hab.grid[i+1,j]/hab.grid[i,j])
					move.inf[[(i-1)]][[j]] <- move.inf[[(i-1)]][[j]] + move.b
					move.inf[[i]][[(j-1)]] <- move.inf[[i]][[(j-1)]] + move.a 
					move.inf[[(i+1)]][[j]] <- move.inf[[(i+1)]][[j]] + move.c 
					popn.inf[[i]][[j]] <- popn.inf[[i]][[j]] - (move.b + (move.a * 2) + move.c)
				}
				
			} else {
				move.a <- popn.orig.inf[[i]][[j]] * move.prob * (hab.grid[i,j-1]/hab.grid[i,j])
				move.b <- popn.orig.inf[[i]][[j]] * move.prob * (hab.grid[i-1,j]/hab.grid[i,j])
				move.c <- popn.orig.inf[[i]][[j]] * move.prob * (hab.grid[i+1,j]/hab.grid[i,j])
				move.d <- popn.orig.inf[[i]][[j]] * move.prob * (hab.grid[i,j+1]/hab.grid[i,j])
				move.inf[[i]][[j-1]] <- move.inf[[i]][[(j-1)]] + move.a
	 			move.inf[[i-1]][[j]] <- move.inf[[(i-1)]][[j]] + move.b
				move.inf[[i+1]][[j]] <- move.inf[[(i+1)]][[j]] + move.c
				move.inf[[i]][[j+1]] <- move.inf[[i]][[(j+1)]] + move.d
				popn.inf[[i]][[j]] <- popn.inf[[i]][[j]] - (move.a + move.b + move.c + move.d)
			}
		print(c(i,j))
		}
	print(i)
	}
	move.grid.inf <- matrix(0, nrow=nrow(hab.grid), ncol=ncol(hab.grid))
	for(i in 1:nrow(hab.grid)){
		for(j in 1:ncol(hab.grid)){
			print(c(i,j))
			move.grid.inf[i,j] <- sum(move.inf[[i]][[j]])
		}
	}

###### Combine movements to produce popn list ##############
	for(i in 1:nrow(hab.grid){
		for(j in ncol(hab.grid){
		popn[[i]][[j]][,1] <- popn.un[[i]][[j]]
		popn[[i]][[j]][,2] <- popn.inf[[i]][[j]]
		move[[i]][[j]][,1] <- move.un[[i]][[j]]
		move[[i]][[j]][,2] <- move.inf[[i]][[j]]
		}
	}


	return(list(new.pop = popn, movements = move, move.grid = list("uninfected"=move.grid.un, "infected"=move.grid.inf)))
}
				
			

