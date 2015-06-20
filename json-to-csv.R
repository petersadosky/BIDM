#userYelp <- readLines("~/Documents/School/94832 - S15/yelp_academic_dataset_user.json")

extract <- function(rowNumber) {
	row <- x[rowNumber]
	rowClean <- gsub("\\{|\\}|\\\"", "", row)
	friendSplit1 <- unlist(gregexpr("\\[", rowClean))[1]
	friendSplit2 <- unlist(gregexpr("\\]", rowClean))[1]
	middle <- gsub(",", "", substr(rowClean, friendSplit1+1, friendSplit2-1))
	rowSub <- paste0(substr(rowClean, 1, friendSplit1-1), middle, substr(rowClean, 
					 friendSplit2+1, nchar(rowClean))) 
	result <- unlist(strsplit(rowSub, ","))[c(1:8, 10)]
	return(gsub("(.*?): ", "", result))
}

# userCSV = as.data.frame(t(sapply(1:length(userYelp), extract)))
# names(userCSV) <- c("yelping_since", "funny votes", "useful votes", "cool votes", "review_count", "name", "user_id", "friends", "average_stars")
# write.csv(userCSV, "yelp-user.csv")



#x = readLines("~/Documents/School/94832 - S15/yelp_academic_dataset_business.json")

x = readLines("~/Downloads/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_business.json")

extract <- function(row) {
	y = gsub("\\{|\\}|:|\"", "", x[row])
	y = unlist(strsplit(y, ","))
	return(y)
}

category = c()
for (i in 1:length(x)) {
	y = gsub("\\{|\\}|:|\"", "", x[i])
	category[i] <- regmatches(y, regexpr("\\[(.*?)\\]", y))
}
category.clean <- substr(category, 2, nchar(category)-1)

review_count = c()
for (i in 1:length(x)) {
	y = gsub("\\{|\\}|:|\"", "", x[i])
	review_count[i] = regmatches(y, regexpr("review_count [0-9]{0,20}", y))
}
review_count.clean <- as.numeric(gsub("[^0-9]", "", review_count))

longitude = c()
for (i in 1:length(x)) {
	y = gsub("\\{|\\}|:|\"", "", x[i])
	if (length(regmatches(y, regexpr("longitude (-| )[0-9]{1,30}(\\.| )[0-9]{1,30}", y)))) {
		longitude[i] = regmatches(y, regexpr("longitude (-| )[0-9]{1,30}(\\.| )[0-9]{1,30}", y))
		}
	else {
		longitude[i] = NA
	}
}
		
longitude.clean = substr(longitude, 11, nchar(longitude))
 
latitude = c()
for (i in 1:length(x)) {
	y = gsub("\\{|\\}|:|\"", "", x[i])
	latitude[i] = regmatches(y, regexpr("latitude [0-9]{1,30}(\\.| )[0-9]{1,30}", y))
}
latitude.clean = substr(latitude, 11, nchar(latitude))

stars = c()
for (i in 1:length(x)) {
	y = gsub("\\{|\\}|:|\"", "", x[i])
	stars[i] = regmatches(y, regexpr("stars (.*?),", y))
}
stars.clean = substr(stars, 7, nchar(stars)-1)

takeout = c()
for (i in 1:length(x)) {
	y = gsub("\\{|\\}|:|\"", "", x[i])
	if (length(regmatches(y, regexpr("Take-out (.*?),", y)))) {
		takeout[i] = regmatches(y, regexpr("Take-out (.*?),", y))
	}
	else {
		takeout[i] = NA
	}
}
takeout.clean <- substr(takeout, 10, nchar(takeout)-1)

wifi = c()
for (i in 1:length(x)) {
	y = gsub("\\{|\\}|:|\"", "", x[i])
	if (length(regmatches(y, regexpr("Wi-Fi (.*?),", y)))) {
		wifi[i] = regmatches(y, regexpr("Wi-Fi (.*?),", y))
	}
	else {
		wifi[i] = NA
	}
}
wifi.clean = substr(wifi, 7, nchar(wifi)-1)

reservations = c()
for (i in 1:length(x)) {
	y = gsub("\\{|\\}|:|\"", "", x[i])
	if (length(regmatches(y, regexpr("Reservations (.*?),", y)))) {
		reservations[i] = regmatches(y, regexpr("Reservations (.*?),", y))
	}
	else {
		reservations[i] = NA
	}
}
reservations.clean = substr(reservations, 14, nchar(reservations)-1)

delivery = c()
for (i in 1:length(x)) {
	y = gsub("\\{|\\}|:|\"", "", x[i])
	if (length(regmatches(y, regexpr("Delivery (.*?),", y)))) {
		delivery[i] = regmatches(y, regexpr("Delivery (.*?),", y))
	}
	else {
		delivery[i] = NA
	}
}
delivery.clean = substr(delivery, 10, nchar(delivery)-1)

parking = c()
for (i in 1:length(x)) {
	y = gsub("\\{|\\}|:|\"", "", x[i])
	if (length(regmatches(y, regexpr("Parking garage (.*?),", y)))) {
		parking[i] = regmatches(y, regexpr("Parking garage (.*?),", y))
	}
	else {
		parking[i] = NA
	}
}
parking.clean = substr(parking, 16, nchar(parking)-1)

street = c()
for (i in 1:length(x)) {
	y = gsub("\\{|\\}|:|\"", "", x[i])
	if (length(regmatches(y, regexpr("street (.*?),", y)))) {
		street[i] = regmatches(y, regexpr("street (.*?),", y))
	}
	else {
		street[i] = NA
	}
}
street.clean = substr(street, 8, nchar(street)-1)

validated = c()
for (i in 1:length(x)) {
	y = gsub("\\{|\\}|:|\"", "", x[i])
	if (length(regmatches(y, regexpr("validated (.*?),", y)))) {
		validated[i] = regmatches(y, regexpr("validated (.*?),", y))
	}
	else {
		validated[i] = NA
	}
}
validated.clean = substr(validated, 11, nchar(validated)-1)

lot = c()
for (i in 1:length(x)) {
	y = gsub("\\{|\\}|:|\"", "", x[i])
	if (length(regmatches(y, regexpr("lot (.*?),", y)))) {
		lot[i] = regmatches(y, regexpr("lot (.*?),", y))
	}
	else {
		lot[i] = NA
	}
}
lot.clean = substr(lot, 5, nchar(lot)-1)

valet = c()
for (i in 1:length(x)) {
	y = gsub("\\{|\\}|:|\"", "", x[i])
	if (length(regmatches(y, regexpr("valet (.*?),", y)))) {
		valet[i] = regmatches(y, regexpr("valet (.*?),", y))
	}
	else {
		valet[i] = NA
	}
}
valet.clean = substr(valet, 7, nchar(valet)-1)

wheelchair = c()
for (i in 1:length(x)) {
	y = gsub("\\{|\\}|:|\"", "", x[i])
	if (length(regmatches(y, regexpr("Wheelchair Accessible (.*?),", y)))) {
		wheelchair[i] = regmatches(y, regexpr("Wheelchair Accessible (.*?),", y))
	}
	else {
		wheelchair[i] = NA
	}
}
wheelchair.clean = substr(wheelchair, 23, nchar(wheelchair)-1)

attire = c()
for (i in 1:length(x)) {
	y = gsub("\\{|\\}|:|\"", "", x[i])
	if (length(regmatches(y, regexpr("Attire (.*?),", y)))) {
		attire[i] = regmatches(y, regexpr("Attire (.*?),", y))
	}
	else {
		attire[i] = NA
	}
}
attire.clean = substr(attire, 8, nchar(attire)-1)

credit = c()
for (i in 1:length(x)) {
	y = gsub("\\{|\\}|:|\"", "", x[i])
	if (length(regmatches(y, regexpr("Accepts Credit Cards (.*?),", y)))) {
		credit[i] = regmatches(y, regexpr("Accepts Credit Cards (.*?),", y))
	}
	else {
		credit[i] = NA
	}
}
credit.clean = substr(credit, 22, nchar(credit)-1)

groups = c()
for (i in 1:length(x)) {
	y = gsub("\\{|\\}|:|\"", "", x[i])
	if (length(regmatches(y, regexpr("Good For Groups (.*?),", y)))) {
		groups[i] = regmatches(y, regexpr("Good For Groups (.*?),", y))
	}
	else {
		groups[i] = NA
	}
}
groups.clean = substr(groups, 17, nchar(groups)-1)

price = c()
for (i in 1:length(x)) {
	y = gsub("\\{|\\}|:|\"", "", x[i])
	if (length(regmatches(y, regexpr("Price Range (.*?),", y)))) {
		price[i] = regmatches(y, regexpr("Price Range (.*?),", y))
	}
	else {
		price[i] = NA
	}
}
price.clean = substr(price, 13, nchar(price)-1)

z = t(sapply(1:length(x), extract))
rm(x)

# id = sapply(z, function(x) x[1])
# id = unlist(strsplit(id, " "))
# id = id[seq(2, length(id), 2)]

id = z[,1]

# zip = sapply(z, function(x) x[3])
zip = gsub("[^0-9]", "", z[,3])
# zip = ifelse(99999 > zip & zip > 10000, zip, NA)



business = data.frame(id, zip, "latitude"=latitude.clean, "longitude"=longitude.clean, "category"=category.clean, "review count"=review_count.clean, "stars"=stars.clean, "wifi"=wifi.clean, "reservations"=reservations.clean, "delivery"=delivery.clean, "parking"=parking.clean, "street"=street.clean, "validated"=validated.clean, "lot"=lot.clean, "valet"=valet.clean, "wheelchair"=wheelchair.clean, "attire"=attire.clean, "credit cards"=credit.clean, "groups"=groups.clean, "price"=price.clean)

write.csv(business, "~/Documents/School/94832 - S15/NEW-yelp-business.csv")


