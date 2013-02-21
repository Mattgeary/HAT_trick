infection <- function(popn, probe, infect, pupae, adult){
	
		for(i in 1:length(probe[[1]])){
			for(j in 1:length(probe[[1]])){
				probe[[i]][[j]] <- probe[[i]][[j]] * infect
			}
		}
		
	for(i in 1:length(cell.popn)){
		for (j in 1:length(cell.popn[[i]])){
			N <- cell.popn[[i]][[j]]
			for(p in 1:max(pupae)){
				N[p,1] <- N[p,1] + N[p,2]
				N[p,2] <- 0
			}
			for(v in (max(pupae)+1):max(adult)){
				N[v,2] <- N[v,2] + probe[[i]][[j]][v,1]
				N[v,1] <- N[v,1] - probe[[i]][[j]][v,1]
			}
			cell.popn[[i]][[j]] <- N
		}
	}
 	return(list(cell.popn = cell.popn))
}
