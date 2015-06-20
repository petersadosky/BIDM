#x = readLines("~/Documents/School/94832 - S15/yelp_academic_dataset_review.json")

x = readLines("~/Downloads/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_review.json")

extract <- function(row) {
	y = gsub("\\{|\\}|\\:", "", x[row])
	funny = regmatches(y, regexpr("funny(.*?),", y))
	funny = substr(funny, 1, nchar(funny)-4)
	useful = regmatches(y, regexpr("useful(.*?),", y))
	useful = gsub("[^0-9]", "", useful)
	cool = gsub("[^0-9]", "", regmatches(y, regexpr("cool(.*?),", y)))
	user_id = regmatches(y, regexpr("user_id(.*?),", y))
	user_id = substr(user_id, 11, nchar(user_id)-2)
	review_id = regmatches(y, regexpr("review_id(.*?),", y))
	review_id = substr(review_id, 13, nchar(review_id)-2)
	stars = gsub("[^0-9]", "", regmatches(y, regexpr("stars(.*?),", y)))
	date = regmatches(y, regexpr("date(.*?),", y))
	date = substr(date, 8, nchar(date)-2)
	text = regmatches(y, regexpr("text(.*?)type\"", y))
	text = substr(text, 8, nchar(text)-9)
	business = regmatches(y, regexpr("business_id\" \"(.*?)\"", y))
	business = substr(business, 15, nchar(business)-1)
	return(c(user_id, review_id, stars, date, text, business, funny, useful, cool))
}

output = as.data.frame(t(sapply(1:length(x), extract)))

write.csv(output, "~/Documents/School/94832 - S15/yelp-reviews-r5.csv")



