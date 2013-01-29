HAT_move <- function(popn, move, move.prob, hab.grid){
	for(i in 1:nrow(hab.grid)){
		for(j in 1:ncol(hab.grid)){
			if((i-1) <= 0 | (j-1) <=0 | (i+1) >= nrow(hab.grid) | (j+1) >= ncol(hab.grid)){
				if(i-1 <= 0 & j-1 <= 0){
					move[[i]][[j]]	<- move[[i]][[j]] + ((popn[[(i+1)]][[j]]* move.prob) + (popn[[(i)]][[j+1]]* move.prob))
					move[[(i+1)]][[j]] <- move.c <- move[[(i+1)]][[j]] + (popn[[i]][[j]] * move.prob)
					move[[i]][[(j+1)]] <- move.d <- move[[i]][[(j+1)]] + (popn[[i]][[j]] * move.prob)
					popn[[i]][[j]] <- popn[[i]][[j]] - sum(c(move.c, move.d))
				}
				if(i-1 <= 0 & j+1 >= ncol(hab.grid)){
					move[[i]][[j]]	<- move[[i]][[j]] + ((popn[[(i+1)]][[j]]* move.prob) + (popn[[(i)]][[j-1]]* move.prob))
					move[[(i+1)]][[j]] <- move.c <- move[[(i+1)]][[j]] + (popn[[i]][[j]] * move.prob)
					move[[i]][[(j-1)]] <- move.d <- move[[i]][[(j-1)]] + (popn[[i]][[j]] * move.prob)
					popn[[i]][[j]] <- popn[[i]][[j]] - sum(c(move.c, move.d))
				}
				if(i+1 >= nrow(hab.grid) & j-1 <= 0){
					move[[i]][[j]]	<- move[[i]][[j]] + ((popn[[(i-1)]][[j]]* move.prob) + (popn[[(i)]][[j+1]]* move.prob))
					move[[(i-1)]][[j]] <- move.c <- move[[(i-1)]][[j]] + (popn[[i]][[j]] * move.prob)
					move[[i]][[(j+1)]] <- move.d <- move[[i]][[(j+1)]] + (popn[[i]][[j]] * move.prob)
					popn[[i]][[j]] <- popn[[i]][[j]] - sum(c(move.c, move.d))
				}
				if(i+1 >= nrow(hab.grid) & j+1 <= 0){
					move[[i]][[j]]	<- move[[i]][[j]] + ((popn[[(i-1)]][[j]]* move.prob) + (popn[[(i)]][[j-1]]* move.prob))
					move[[(i-1)]][[j]] <- move.c <- move[[(i-1)]][[j]] + (popn[[i]][[j]] * move.prob)
					move[[i]][[(j-1)]] <- move.d <- move[[i]][[(j-1)]] + (popn[[i]][[j]] * move.prob)
					popn[[i]][[j]] <- popn[[i]][[j]] - sum(c(move.c, move.d))
				}
				if(i-1 <= 0){
					move[[i]][[j]]	<- move[[i]][[j]] + (popn[[(i+1)]][[j]]* move.prob)
	 				move[[i]][[(j-1)]] <- move.a <-  move[[i]][[(j-1)]] + (popn[[i]][[j]] * move.prob)
					move[[(i+1)]][[j]] <- move.c <- move[[(i+1)]][[j]] + (popn[[i]][[j]] * move.prob)
					move[[i]][[(j+1)]] <- move.d <- move[[i]][[(j+1)]] + (popn[[i]][[j]] * move.prob)
					popn[[i]][[j]] <- popn[[i]][[j]] - sum(c(move.a, move.c, move.d))
				}
				if(i+1 >= nrow(hab.grid)){
					move[[i]][[j]]	<- move[[i]][[j]] + (popn[[(i-1)]][[j]] * move.prob)
					move[[(i-1)]][[j]] <- move.b <- move[[(i-1)]][[j]] + (popn[[i]][[j]] * move.prob)
					move[[i]][[(j-1)]] <- move.a <- move[[i]][[(j-1)]] + (popn[[i]][[j]] * move.prob)
					move[[i]][[(j+1)]] <- move.d <- move[[i]][[(j+1)]] + (popn[[i]][[j]] * move.prob)
					popn[[i]][[j]] <- popn[[i]][[j]] - sum(c(move.b, move.a, move.d))
				}
				if(j-1 <= 0){
					move[[i]][[j]]	<- move[[i]][[j]] + (popn[[i]][[(j+1)]] * move.prob)
					move[[(i-1)]][[j]] <- move.a <- move[[(i-1)]][[j]] + (popn[[i]][[j]] * move.prob)
					move[[i]][[(j+1)]] <- move.d <- move[[i]][[(j+1)]] + (popn[[i]][[j]] * move.prob)
					move[[(i+1)]][[j]] <- move.c <- move[[(i+1)]][[j]] + (popn[[i]][[j]] * move.prob)
					popn[[i]][[j]] <- popn[[i]][[j]] - sum(c(move.a, move.d, move.c))
				}
				if(j+1 >= ncol(hab.grid)){
					move[[i]][[j]]	<- move[[i]][[j]] + (popn[[i]][[(j-1)]] * move.prob)
					move[[(i-1)]][[j]] <- move.b <- move[[(i-1)]][[j]] + (popn[[i]][[j]] * move.prob)
					move[[i]][[(j-1)]] <- move.a <- move[[i]][[(j-1)]] + (popn[[i]][[j]] * move.prob)
					move[[(i+1)]][[j]] <- move.c <- move[[(i+1)]][[j]] + (popn[[i]][[j]] * move.prob)
					popn[[i]][[j]] <- popn[[i]][[j]] - sum(c(move.b, move.a, move.c))
				}
				
			}
			else{
				move[[i]][[j-1]] <- move.a <-  move[[i]][[(j-1)]] + (popn[[i]][[j]] * move.prob)
	 			move[[i-1]][[j]] <- move.b <-  move[[(i-1)]][[j]] + (popn[[i]][[j]] * move.prob)
				move[[i+1]][[j]] <- move.c <- move[[(i+1)]][[j]] + (popn[[i]][[j]] * move.prob)
				move[[i]][[j+1]] <- move.d <- move[[i]][[(j+1)]] + (popn[[i]][[j]] * move.prob)
				popn[[i]][[j]] <- popn[[i]][[j]] - sum(c(move.a, move.b, move.c, move.d))
			}
		}
	}
	move.grid <- matrix(NA, nrow=nrow(hab.grid), ncol=ncol(hab.grid))
	for(i in 1:nrow(hab.grid)){
		for(j in ncol(hab.grid)){
			move.grid[i,j] <- sum(move[[i]][[j]])
		}
	}
	return(new.pop = popn, movements = move, move.grid = move.grid)
}
				
			
move.fun <- HAT_move(cell.popn, move, move.prob, hab.grid)
