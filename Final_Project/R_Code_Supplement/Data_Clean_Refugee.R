refugee <- read.csv("refugee.csv")
refugee1 <- refugee
cnt_lat_lon <- read.csv("Country_Lat_Long.csv")
country_node <- cnt_lat_lon
country_node$label <- country_node$Country
country_node <- country_node[order(country_node$label),]
colnames(country_node)[1]<-"Id"

#Remove Rows with Unkown Countries and Value less than 300
refugee1 <- refugee1[!grepl("Various/Unknown", refugee1$Country...territory.of.asylum.residence),]
refugee1 <- refugee1[refugee1$Value > 1000,]
#Change Country Names to Match nodelist
refugee1$country <- refugee1$Country...territory.of.asylum.residence
refugee1$newcountry <- gsub("Dem. Rep. of the Congo", "Congo, the Democratic Republic of the", refugee1$country)
refugee1$newcountry <- gsub("Bolivia \\(Plurinational State of\\)", "Bolivia, Plurinational State of", refugee1$newcountry)
refugee1$newcountry <- gsub("Cabo Verde", "Cape Verde", refugee1$newcountry)
refugee1$newcountry <- gsub("Central African Rep.", "Central African Republic", refugee1$newcountry)
refugee1$newcountry <- gsub("C̫te d'Ivoire", "Côte d'Ivoire", refugee1$newcountry)
refugee1$newcountry <- gsub("Czech Rep.", "Czech Republic", refugee1$newcountry)
refugee1$newcountry <- gsub("Dominican Rep.", "Dominican Republic", refugee1$newcountry)
refugee1$newcountry <- gsub("Iran \\(Islamic Rep. of\\)", "Iran, Islamic Republic of", refugee1$newcountry)
refugee1$newcountry <- gsub("Rep. of Korea", "Korea, Republic of", refugee1$newcountry)
refugee1$newcountry <- gsub("Lao People's Dem. Rep.", "Lao People's Democratic Republic", refugee1$newcountry)
refugee1$newcountry <- gsub("Libya", "Libyan Arab Jamahiriya", refugee1$newcountry)
refugee1$newcountry <- gsub("The former Yugoslav Republic of Macedonia", "Macedonia, the former Yugoslav Republic of", refugee1$newcountry)
refugee1$newcountry <- gsub("Rep. of Moldova", "Moldova, Republic of", refugee1$newcountry)
refugee1$newcountry <- gsub("Serbia and Kosovo \\(S/RES/1244 \\(1999\\)\\)", "Serbia", refugee1$newcountry)
refugee1$newcountry <- gsub("Syrian Arab Rep.", "Syrian Arab Republic", refugee1$newcountry)
refugee1$newcountry <- gsub("State of Palestine", "Palestinian", refugee1$newcountry)
refugee1$newcountry <- gsub("United Rep. of Tanzania", "Tanzania, United Republic of", refugee1$newcountry)
refugee1$newcountry <- gsub("United States of America", "United States", refugee1$newcountry)
refugee1$newcountry <- gsub("Sint Maarten (Dutch part)", "Netherlands Antilles", refugee1$newcountry)
refugee1$newcountry <- gsub("Venezuela \\(Bolivarian Republic of\\)", "Venezuela, Bolivarian Republic of", refugee1$newcountry)
refugee1$newcountry <- gsub("China, Hong Kong SAR", "Hong Kong", refugee1$newcountry)
refugee1$newcountry <- gsub("China, Macao SAR", "Macao", refugee1$newcountry)
refugee1$newcountry <- gsub("Micronesia \\(Federated States of\\)", "Micronesia, Federated States of", refugee1$newcountry)
refugee1$newcountry <- gsub("British Virgin Islands", "British Indian Ocean Territory", refugee1$newcountry)
refugee1$newcountry <- gsub("Dem. People's Rep. of Korea", "Korea, Democratic People's Republic of", refugee1$newcountry)
refugee1$newcountry <- gsub("Holy See \\(the\\)", "Holy See \\(Vatican City State\\)", refugee1$newcountry)
#Remove Garbage Rows
refugee1$country <- refugee1$newcountry
refugee1 <- refugee1[c(-2,-4,-7)]

#Change Country Names to Match nodelist
refugee1$neworigin <- refugee1$Origin
refugee1$neworigin <- gsub("Dem. Rep. of the Congo", "Congo, the Democratic Republic of the", refugee1$neworigin)
refugee1$neworigin <- gsub("Bolivia \\(Plurinational State of\\)", "Bolivia, Plurinational State of", refugee1$neworigin)
refugee1$neworigin <- gsub("Cabo Verde", "Cape Verde", refugee1$neworigin)
refugee1$neworigin <- gsub("Central African Rep.", "Central African Republic", refugee1$neworigin)
refugee1$neworigin <- gsub("C̫te d'Ivoire", "Côte d'Ivoire", refugee1$neworigin)
refugee1$neworigin <- gsub("Czech Rep.", "Czech Republic", refugee1$neworigin)
refugee1$neworigin <- gsub("Dominican Rep.", "Dominican Republic", refugee1$neworigin)
refugee1$neworigin <- gsub("Iran \\(Islamic Rep. of\\)", "Iran, Islamic Republic of", refugee1$neworigin)
refugee1$neworigin <- gsub("Rep. of Korea", "Korea, Republic of", refugee1$neworigin)
refugee1$neworigin <- gsub("Lao People's Dem. Rep.", "Lao People's Democratic Republic", refugee1$neworigin)
refugee1$neworigin <- gsub("Libya", "Libyan Arab Jamahiriya", refugee1$neworigin)
refugee1$neworigin <- gsub("The former Yugoslav Republic of Macedonia", "Macedonia, the former Yugoslav Republic of", refugee1$neworigin)
refugee1$neworigin <- gsub("Rep. of Moldova", "Moldova, Republic of", refugee1$neworigin)
refugee1$neworigin <- gsub("Serbia and Kosovo \\(S/RES/1244 \\(1999\\)\\)", "Serbia", refugee1$neworigin)
refugee1$neworigin <- gsub("Syrian Arab Rep.", "Syrian Arab Republic", refugee1$neworigin)
refugee1$neworigin <- gsub("State of Palestine", "Palestinian", refugee1$neworigin)
refugee1$neworigin <- gsub("United Rep. of Tanzania", "Tanzania, United Republic of", refugee1$neworigin)
refugee1$neworigin <- gsub("United States of America", "United States", refugee1$neworigin)
refugee1$neworigin <- gsub("Sint Maarten (Dutch part)", "Netherlands Antilles", refugee1$neworigin)
refugee1$neworigin <- gsub("Venezuela \\(Bolivarian Republic of\\)", "Venezuela, Bolivarian Republic of", refugee1$neworigin)
refugee1$neworigin <- gsub("China, Hong Kong SAR", "Hong Kong", refugee1$neworigin)
refugee1$neworigin <- gsub("China, Macao SAR", "Macao", refugee1$neworigin)
refugee1$neworigin <- gsub("Micronesia \\(Federated States of\\)", "Micronesia, Federated States of", refugee1$neworigin)
refugee1$neworigin <- gsub("British Virgin Islands", "British Indian Ocean Territory", refugee1$neworigin)
refugee1$neworigin <- gsub("Dem. People's Rep. of Korea", "Korea, Democratic People's Republic of", refugee1$neworigin)
refugee1$neworigin <- gsub("Holy See \\(the\\)", "Holy See \\(Vatican City State\\)", refugee1$neworigin)
#Remove Garbage Rows
refugee1$Origin <- refugee1$neworigin
refugee1 <- refugee1[c(-5)]

#Get Longitude and Latitude from Nodelist
refugee1$lat_dest <- cnt_lat_lon[match(refugee1$country,cnt_lat_lon$Country),"Latitude"]
refugee1$lon_dest <- cnt_lat_lon[match(refugee1$country,cnt_lat_lon$Country),"Longitude"]

refugee1$lat_org <- cnt_lat_lon[match(refugee1$Origin,cnt_lat_lon$Country),"Latitude"]
refugee1$lon_org <- cnt_lat_lon[match(refugee1$Origin,cnt_lat_lon$Country),"Longitude"]
#Get list of cases without NA
refugee1 <- refugee1[complete.cases(refugee1),]
#Calculate Edge Weight
refugee1$weight <- log(log(refugee1$Value^5)/4)-1.9
summary(refugee1$weight)
colnames(refugee1)[2]<-"Source"
colnames(refugee1)[4]<-"Target"
#Write Files
write.csv(refugee1,file = "Refugee_EdgeList.csv",row.names = FALSE)
write.csv(country_node,file = "Refugee_Nodes.csv",row.names = FALSE)
