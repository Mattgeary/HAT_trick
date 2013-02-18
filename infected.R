infect <- funtion(cell.popn, pop.mat, infect)
	for(i in 1:length(cell.popn)){
		for (j in 1:length(cell.popn[[i]])){
			N <- cell.popn[[i]][[j]]
			N <- pop.mat %*% N
			for(p in 1:max(row.pup){
				N[p,1] <- N[p,1] + N[p,2]
				N[p,2] <- 0
			}
			for(q in 8:max(row.adult)){
				N[q,2] <- N[q,2] + (N[q,1] * infect)
				N[q,1] <- N[q,1] - (N[q,1] * infect)
			}
			cell.popn[[i]][[j]] <- N
		}
	}
}
