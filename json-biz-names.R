x <- readLines("~/Documents/School/94832 - S15/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_business.json")


extract <- function(business.rownumber) {
	biz1 = regmatches(x[business.rownumber], regexpr("name(.*?)\",", x[business.rownumber]))
	clean = substr(biz1, 9, nchar(biz1)-2)
	return(clean)
}

y = sapply(1:length(x), extract)
write.csv(y, "business-names.csv")

a = read.csv("~/Documents/School/94832 - S15/business-test-names.csv", stringsAsFactors=FALSE)
a = a[,1:7]

b = sapply(a$Category__, function(x) grep("Restaurants", x))
