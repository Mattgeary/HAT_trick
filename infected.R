infection <- function(cell.popn, pop.mat, infect, pupae, adult){
	for(i in 1:length(cell.popn)){
		for (j in 1:length(cell.popn[[i]])){
			N <- cell.popn[[i]][[j]]
			for(p in 1:max(pupae)){
				N[p,1] <- N[p,1] + N[p,2]
				N[p,2] <- 0
			}
			for(q in (max(pupae)+1):max(adult)){
				N[q,2] <- N[q,2] + (N[q,1] * infect)
				N[q,1] <- N[q,1] - (N[q,1] * infect)
			}
			print(c("Infection => ",i,j))
			cell.popn[[i]][[j]] <- N
		}
	}
 	return(list(cell.popn = cell.popn))
}
