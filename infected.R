transmission.default <- list(probe = data.frame("human" = 0.1, "OE" = 0.1), feed = data.frame("human" = 0.1, "OE"= 0.1), mature = data.frame("tsetse" = 0.033, "human" = 0.05, "OE" = 0.05), recover = data.frame("human" = 0.05, "OE" = 0.1))
			
infection <- function(popn, human, OE, probe, feed, transmission = transmission.default, pupae, adult){
		
		#### Create list of infected tsetse in each cell ####
		infect <- list()
		for(i in 1:nrow(max(adult))){
			infect[[i]] <- list()
			for(j in 1:ncol(max(adult))){
				infect[[i]][[j]] <- matrix(c(rep(0, max.age)), ncol=3)
			}
		}

		#### Calculate number of infected flies ####
		for(i in 1:length(probe[[1]])){
			for(j in 1:length(probe[[1]])){
				infect[[i]][[j]] <- ((sum(humans[[i]][[j]])/sum(human[[i]][[j]][,3]) * (sum(probe["human"][[i]][[j]]) - sum(feed[["human"]][[i]][[j]])* transmission[["probe"]]$human) + ((sum(human[[i]][[j]])/sum(human[[i]][[j]][,3]) * sum(feed[["human"]][[i]][[j]]) * transmission[["probe"]]$human) + ((sum(human[[i]][[j]])/sum(OE[[i]][[j]][,3]) * (sum(probe["OE"][[i]][[j]]) - sum(feed[["OE"]][[i]][[j]])* transmission[["probe"]]$OE) + ((sum(OE[[i]][[j]])/sum(OE[[i]][[j]][,3]) * sum(feed[["OE"]][[i]][[j]]) * transmission[["probe"]]$OE) 	
			}
		}

		#### Update infected and incubating tsetse ####
	for(i in 1:length(cell.popn)){
		for (j in 1:length(cell.popn[[i]])){
			N <- cell.popn[[i]][[j]]
			for(p in 1:max(pupae)){
				N[p,1] <- N[p,1] + N[p,2] + N[p,3] 
				N[p,2] <- 0
				N[p,3] <- 0
				N[p,4] <- 0				
			}
			for(v in (max(pupae)+1):max(adult)){
				N[v,2] <- N[v,2] + infect[[i]][[j]][v,1]
				N[v,1] <- N[v,1] - infect[[i]][[j]][v,1]
				N[v,3] <- N[v,3] + (popn[[i]][[j]][v,2] * transmission[["mature"]]$tsetse)
				N[v,2] <- N[v,2] - (popn[[i]][[j]][v,2] * transmission[["mature"]]$tsetse)
			}
			cell.popn[[i]][[j]] <- N
		}
	}

		

		#### Calculate number of infected humans ####
		for(i in 1:length(human[[1]])){
			for(j in length(human[[1]])){
				human[[i]][[j]][,2] <- human[[i]][[j]][,2] + (sum(feed[["human"]][[i]][[j]])/sum(cell.popn[[i]][[j]])[,3]) * transmission[["feed"]]$human) + ((sum(probe[["human"]][[i]][[j]]) - sum(feed[["human"]][[i]][[j]]))/sum(cell.popn[[i]][[j]])[,3]) * transmission[["probe"]]$human)
			}
		}

		#### Update infected/incubating/cured humans ####

		for(i in 1:length(human)){
			for (j in 1:length(human[[i]])){
				N <- human[[i]][[j]]
				for(p in 1:2)){
					N[p,1] <- N[p,1] + N[p,2] + N[p,3] + N[p,4]
					N[p,2] <- 0
					N[p,3] <- 0
					N[p,4] <- 0				
				}
				for(v in (1:2){
					N[v,2] <- N[v,2] + infect[[i]][[j]][v,1]
					N[v,1] <- N[v,1] - infect[[i]][[j]][v,1]
					N[v,4] <- N[v,3] - (human[[i]][[j]][v,3] * transmission[["recover"]]$human)
					N[v,4] <- N[v,4] + (human[[i]][[j]][v,4] * transmission[["recover"]]$human)
					N[v,3] <- N[v,3] + (human[[i]][[j]][v,2] * transmission[["mature"]]$human)
					N[v,2] <- N[v,2] - (human[[i]][[j]][v,2] * transmission[["mature"]]$human)
					
				}
				human[[i]][[j]] <- N
				}
		}


		#### Calculate number of infected OE ####

		for(i in 1:length(OE[[1]])){
			for(j in length(OE[[1]])){
				OE[[i]][[j]][,2] <- OE[[i]][[j]][,2] + (sum(feed[["OE"]][[i]][[j]])/sum(cell.popn[[i]][[j]])[,3]) * transmission[["feed"]]$OE) + ((sum(probe[["OE"]][[i]][[j]]) - sum(feed[["OE"]][[i]][[j]]))/sum(cell.popn[[i]][[j]])[,3]) * transmission[["probe"]]$OE)
			}
		}

		#### Update infected/incubating/recovered OE ####

		for(i in 1:length(OE)){
			for (j in 1:length(OE[[i]])){
				N[v,2] <- N[v,2] + infect[[i]][[j]][v,1]
				N[v,1] <- N[v,1] - infect[[i]][[j]][v,1]
				N[v,4] <- N[v,3] - (OE[[i]][[j]][v,3] * transmission[["recover"]]$OE)
				N[v,4] <- N[v,4] + (OE[[i]][[j]][v,4] * transmission[["recover"]]$OE)
				N[v,3] <- N[v,3] + (OE[[i]][[j]][v,2] * transmission[["mature"]]$OE)
				N[v,2] <- N[v,2] - (OE[[i]][[j]][v,2] * transmission[["mature"]]$OE)
					
			OE[[i]][[j]] <- N
			}
		}


 	return(list(cell.popn = cell.popn, human.popn = human.popn, OE.popn = OE.popn))
}
