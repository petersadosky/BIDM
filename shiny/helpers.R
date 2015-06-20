# BIDM S15 Final Project Recommendation Engine

# Goal of code: Map yelp business dataset variables onto 2d clustering space
# and based on this space, make user recommendations for future restaurants
# Clusters are formed in SAS EM, resulting clusters are saved as a csv
# file "yelp-clusters.csv", which is imported here and used to make 
# recommendation

business <- read.csv("data/pittsburgh-restaurants.csv", 
					 stringsAsFactors=FALSE)
yelp.clusters <- read.csv("data/just-pittsburgh-clusters.csv", 
						  stringsAsFactors=FALSE)

# Find, for a given user, what restaurants he/she has been to
# how much they liked them, and what clusters those clusters fall into
restaurant.cluster <- function(restaurants, ratings) {
	indices <- c()
	for (restaurant in restaurants) {
		indices <- c(indices, which(business$name == restaurant))
	}
	ids <- business$id[indices][1]
	id.indices <- c()
	for (id in ids) {
		id.indices <- c(id.indices, which(yelp.clusters$ID == id))
	}
	clusters <- data.frame(yelp.clusters$Cluster[id.indices], ratings)
	names(clusters) <- c("Cluster", "rating")
	return(clusters)
}

# Test case:
#restaurant.cluster(c("BRGR", "PLUM pan asian kitchen", 
# 					"Amazon Cafe"), c(5, 4, 1))

cluster.selector <- function(clusters) {
	if (sum(is.na(clusters)) == 1) {
		return(NA)
	}
	clusters$rating <- as.vector(clusters$rating)
	highest.stars <- max(clusters$rating)
	highest.stars.index <- which(clusters$rating == max(clusters$rating))
	highest.frequency.clusters <- as.integer(names(which(table(
								  clusters$Cluster) == max(table(
								  clusters$Cluster)))))
	clusters.index <- c()
	for (cluster in highest.frequency.clusters) {
		clusters.index <- c(clusters.index, which(clusters$Cluster == cluster))
	}
	highest.clusters.index <- sort(clusters.index)
	best.cluster <- intersect(highest.stars.index, highest.clusters.index)
	if (length(best.cluster) == 1) {
		return(clusters[best.cluster, 1])
	}
	else if (length(best.cluster) > 1) {
		return(clusters[sample(best.cluster, 1), 1])
	}
	else if (length(best.cluster) == 0) {
		if (length(highest.stars.index) > 1) {
			return(clusters[sample(highest.stars.index, 1), 1])
		}
		else if (length(highest.stars.index) == 1) {
			return(clusters[highest.stars.index, 1])
		}
		else {
			return("There are no highest stars to select!")
		}
	}
	else {
		return("Number of clusters < 0!")
	}		
}

#cluster.selector(restaurant.cluster(c("Graziano's Pizzeria", "China Sea Chinese Restaurant"), c(3, 5)))

recommendation <- function(restaurants, ratings) {
	if (0 %in% nchar(restaurants)) { return("") }
	#previous.restaurants <- reviews[which(reviews$user == userID), 3]
	clust <- cluster.selector(restaurant.cluster(restaurants, ratings))
	restaurants <- yelp.clusters[which(yelp.clusters$Cluster == clust), 1:2]
	maxStars = which(restaurants$Stars == max(restaurants$Stars))
	pick = sample(maxStars, 1)
	return(business[which(business$id == restaurants[pick, 1]), 3])
}

#recommendation(c("Graziano's Pizzeria", "China Sea Chinese Restaurant"), 
#               c(3, 5))



