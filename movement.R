HAT_move <- function(popn, move, move.prob, hab.grid){
	popn.orig <- popn
	for(i in 1:nrow(hab.grid)){
		for(j in 1:ncol(hab.grid)){
			if((i-1) <= 0 | (j-1) <=0 | i >= nrow(hab.grid) | j >= ncol(hab.grid)){
				print("yes")
				if(i-1 <= 0 & j-1 <= 0){
					move[[i]][[j]]	<- move[[i]][[j]] + ((popn[[(i+1)]][[j]] * move.prob) * (hab.grid[i+1,j]/hab.grid[i,j])  + (popn.orig[[(i)]][[j+1]]* move.prob) * (hab.grid[i+1,j]/hab.grid[i,j]))
					move.c <- (popn.orig[[i]][[j]] * move.prob) * (hab.grid[i+1,j]/hab.grid[i,j])
					move.d <- (popn.orig[[i]][[j]] * move.prob) * (hab.grid[i,j+1]/hab.grid[i,j])
					move[[(i+1)]][[j]] <- move[[(i+1)]][[j]] + move.c
					move[[i]][[(j+1)]] <- move[[i]][[(j+1)]] + move.d
					popn[[i]][[j]] <- popn[[i]][[j]] - (move.c + move.d) * 2
					print("i<=0 & j<=0")
				}
				if(i-1 <= 0 & j >= ncol(hab.grid)){
					move[[i]][[j]]	<- move[[i]][[j]] + ((popn.orig[[(i+1)]][[j]]* move.prob) * (hab.grid[i+1,j]/hab.grid[i,j]) + (popn.orig[[(i)]][[j-1]]* move.prob) * (hab.grid[i,j-1]/hab.grid[i,j]))
					move.c <- (popn.orig[[i]][[j]] * move.prob) * (hab.grid[i+1,j]/hab.grid[i,j])
					move.a <- (popn.orig[[i]][[j]] * move.prob) * (hab.grid[i,j-1]/hab.grid[i,j])
					move[[(i+1)]][[j]] <- move[[(i+1)]][[j]] + move.c
					move[[i]][[(j-1)]] <- move[[i]][[(j-1)]] + move.a 
					popn[[i]][[j]] <- popn[[i]][[j]] - (move.c + move.a) * 2
					print("i<=0 & j>=10")
				}
				if(i >= nrow(hab.grid) & (j-1) <= 0){
					move[[i]][[j]]	<- move[[i]][[j]] + ((popn.orig[[(i-1)]][[j]]* move.prob)* (hab.grid[i-1,j]/hab.grid[i,j]) + (popn.orig[[(i)]][[j+1]]* move.prob) * (hab.grid[i,j+1]/hab.grid[i,j]))
					move.b <- (popn.orig[[i]][[j]] * move.prob) * (hab.grid[i-1,j]/hab.grid[i,j])	
					move.d <- (popn.orig[[i]][[j]] * move.prob) * (hab.grid[i,j+1]/hab.grid[i,j])
					move[[(i-1)]][[j]] <- move[[(i-1)]][[j]] + move.b
					move[[i]][[(j+1)]] <- move[[i]][[(j+1)]] + move.d
					popn[[i]][[j]] <- popn[[i]][[j]] - (move.b + move.d) * 2
					print("i>=10 & j<=0")
				}
				if(i >= nrow(hab.grid) & j >= ncol(hab.grid)){
					move[[i]][[j]]	<- move[[i]][[j]] + ((popn.orig[[(i-1)]][[j]]* move.prob) * (hab.grid[i-1,j]/hab.grid[i,j])+ (popn.orig[[(i)]][[j-1]]* move.prob) * (hab.grid[i,j-1]/hab.grid[i,j]))
					move.b <- (popn.orig[[i]][[j]] * move.prob) * (hab.grid[i-1,j]/hab.grid[i,j])
					move.a <- (popn.orig[[i]][[j]] * move.prob) * (hab.grid[i,j-1]/hab.grid[i,j])
					move[[(i-1)]][[j]] <- move[[(i-1)]][[j]] + move.b
					move[[i]][[(j-1)]] <- move[[i]][[(j-1)]] + move.a
					popn[[i]][[j]] <- popn[[i]][[j]] - (move.b + move.a) * 2
					print("i>=10 & j>=10")
				}
				if(i-1 <= 0 & (j-1) > 0 & j < ncol(hab.grid)){
					move[[i]][[j]]	<- move[[i]][[j]] + (popn.orig[[(i+1)]][[j]]* move.prob) * (hab.grid[i+1,j]/hab.grid[i,j])
					move.a <- (popn.orig[[i]][[j]] * move.prob) * (hab.grid[i,j-1]/hab.grid[i,j])
					move.c <- (popn.orig[[i]][[j]] * move.prob) * (hab.grid[i+1,j]/hab.grid[i,j])
					move.d <- (popn.orig[[i]][[j]] * move.prob) * (hab.grid[i,j+1]/hab.grid[i,j])
	 				move[[i]][[(j-1)]] <- move[[i]][[(j-1)]] + move.a
					move[[(i+1)]][[j]] <- move[[(i+1)]][[j]] + move.c
					move[[i]][[(j+1)]] <- move[[i]][[(j+1)]] + move.d
					popn[[i]][[j]] <- popn[[i]][[j]] - (move.a + (move.c * 2) + move.d)
				}
				if(i >= nrow(hab.grid) & (j-1) > 0 & j < ncol(hab.grid)){
					move[[i]][[j]]	<- move[[i]][[j]] + (popn.orig[[(i-1)]][[j]] * move.prob)  * (hab.grid[i-1,j]/hab.grid[i,j])
					move.b <- (popn.orig[[i]][[j]] * move.prob) * (hab.grid[i-1,j]/hab.grid[i,j])
					move.a <- (popn.orig[[i]][[j]] * move.prob) * (hab.grid[i,j-1]/hab.grid[i,j])
					move.d <- (popn.orig[[i]][[j]] * move.prob) * (hab.grid[i,j+1]/hab.grid[i,j])
					move[[(i-1)]][[j]] <- move[[(i-1)]][[j]] + move.b
					move[[i]][[(j-1)]] <- move[[i]][[(j-1)]] + move.a
					move[[i]][[(j+1)]] <- move[[i]][[(j+1)]] + move.d
					popn[[i]][[j]] <- popn[[i]][[j]] - ((move.b *2) + move.a + move.d)
				}
				if(j-1 <= 0 & (i-1) > 0 & i < nrow(hab.grid)){
					move[[i]][[j]]	<- move[[i]][[j]] + (popn.orig[[i]][[(j+1)]] * move.prob) * (hab.grid[i,j+1]/hab.grid[i,j])
					move.b <- (popn.orig[[i]][[j]] * move.prob) * (hab.grid[i-1,j]/hab.grid[i,j])
					move.d <- (popn.orig[[i]][[j]] * move.prob) * (hab.grid[i,j+1]/hab.grid[i,j])
					move.c <- (popn.orig[[i]][[j]] * move.prob) * (hab.grid[i+1,j]/hab.grid[i,j])
					move[[(i-1)]][[j]] <- move[[(i-1)]][[j]] + move.b
					move[[i]][[(j+1)]] <- move[[i]][[(j+1)]] + move.d
					move[[(i+1)]][[j]] <- move[[(i+1)]][[j]] + move.c
					popn[[i]][[j]] <- popn[[i]][[j]] - (move.b + (move.d * 2) + move.c)
				}
				if(j >= ncol(hab.grid) & (i-1) > 0 & i < nrow(hab.grid)){
					move[[i]][[j]]	<- move[[i]][[j]] + (popn.orig[[i]][[(j-1)]] * move.prob) * (hab.grid[i,j-1]/hab.grid[i,j])
					move.b <- (popn.orig[[i]][[j]] * move.prob) * (hab.grid[i-1,j]/hab.grid[i,j])
					move.a <- (popn.orig[[i]][[j]] * move.prob) * (hab.grid[i,j-1]/hab.grid[i,j])
					move.c <- (popn.orig[[i]][[j]] * move.prob) * (hab.grid[i+1,j]/hab.grid[i,j])
					move[[(i-1)]][[j]] <- move[[(i-1)]][[j]] + move.b
					move[[i]][[(j-1)]] <- move[[i]][[(j-1)]] + move.a 
					move[[(i+1)]][[j]] <- move[[(i+1)]][[j]] + move.c 
					popn[[i]][[j]] <- popn[[i]][[j]] - (move.b + (move.a * 2) + move.c)
				}
				
			} else {
				move.a <- popn.orig[[i]][[j]] * move.prob * (hab.grid[i,j-1]/hab.grid[i,j])
				move.b <- popn.orig[[i]][[j]] * move.prob * (hab.grid[i-1,j]/hab.grid[i,j])
				move.c <- popn.orig[[i]][[j]] * move.prob * (hab.grid[i+1,j]/hab.grid[i,j])
				move.d <- popn.orig[[i]][[j]] * move.prob * (hab.grid[i,j+1]/hab.grid[i,j])
				move[[i]][[j-1]] <- move[[i]][[(j-1)]] + move.a
	 			move[[i-1]][[j]] <- move[[(i-1)]][[j]] + move.b
				move[[i+1]][[j]] <- move[[(i+1)]][[j]] + move.c
				move[[i]][[j+1]] <- move[[i]][[(j+1)]] + move.d
				popn[[i]][[j]] <- popn[[i]][[j]] - (move.a + move.b + move.c + move.d)
			}
		print(c(i,j))
		}
	print(i)
	}
	move.grid <- matrix(0, nrow=nrow(hab.grid), ncol=ncol(hab.grid))
	for(i in 1:nrow(hab.grid)){
		for(j in 1:ncol(hab.grid)){
			print(c(i,j))
			move.grid[i,j] <- sum(move[[i]][[j]])
		}
	}
	return(list(new.pop = popn, movements = move, move.grid = move.grid))
}
				
			

