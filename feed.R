feeds.default = list("detect.ambush" = data.frame("human" = c(0.06679, 0.08176, 0.09300, 0.10181), "OE" = c(0.26715, 0.32702, 0.37201, 0.40725)), "detect.ranging" = data.frame("human" = c(0.07686, 0.10680, 0.12789, 0.13560), "OE" = c(0.23058, 0.32039, 0.38366, 0.40679)), "visit" = data.frame("human" = c(0.74, 0.79, 0.81, 0.82), "OE" = c(0.74, 0.79, 0.81, 0.82)), "probe" = data.frame("human" = c(0.76, 0.81, 0.84, 0.85), "OE" = c(0.76, 0.81, 0.84, 0.85)), "feed" = data.frame("human" = c(0.92, 0.93, 0.93, 0.93), "OE" = c(0.92, 0.93, 0.93, 0.93)))

##############find detected first then the rest.#############


search_fun <- 


feed_fun <- function(habitat, popn, human, non-human, feed.cycle = 3, feeds=feeds.default, adult){
		for(i in 1:nrow(habitat)){
			for(j in 1:ncol(habitat)){
				popn[[i]][[j]][min(adult),] <- popn[[i]][[j]][min(adult),]
				
				popn[[i]][[j]][min(adult) + feed.cycle,]
				
				popn[[i]][[j]][min(adult) + (feed.cycle * 2),]
				
				popn[[i]][[j]][max(adult),]
				
		
