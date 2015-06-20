x = read.csv("~/Downloads/yelp-business-filtered-demo (1).csv", stringsAsFactors=FALSE)

x = x[,c("ID", "PO_NAME")]
pitt = which(x$PO_NAME == "Pittsburgh")
pitt.ids = x[pitt, 1]

y = read.csv("~/Documents/School/94832 - S15/business-with-names.csv", stringsAsFactors=FALSE)

extract.index <- function(id) {
	thing = which(y$id == id)
	if (length(thing) == 0) { thing=NA }
	return(thing)
}

y.ids <- sapply(pitt.ids, extract.index)
names(y.ids)

bad.indices = c()
for (i in 1:length(y.ids)) {
	if (names(y.ids[i]) == "#NAME?") {
		bad.indices = c(bad.indices, i)
	}
}

y.ids = y.ids[-bad.indices]
pitt.indices = unlist(y.ids)

pitt.things = y[pitt.indices,]

write.csv(pitt.things, "pittsburgh-restaurants.csv")