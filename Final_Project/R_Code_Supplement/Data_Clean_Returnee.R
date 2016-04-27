returnee <- read.csv("Returnee.csv")
returnee1 <- returnee
cnt_lat_lon <- read.csv("Country_Lat_Long.csv")
country_node <- cnt_lat_lon
country_node$label <- country_node$Country
country_node <- country_node[order(country_node$label),]
colnames(country_node)[1]<-"Id"

#Remove Rows with Unkown Countries and Value less than 300
returnee1 <- returnee1[!grepl("Various/Unknown", returnee1$Country...territory.of.asylum.residence),]
returnee1 <- returnee1[returnee1$Value > 1000,]
#Change Country Names to Match nodelist
returnee1$country <- returnee1$Country...territory.of.asylum.residence
returnee1$newcountry <- gsub("Dem. Rep. of the Congo", "Congo, the Democratic Republic of the", returnee1$country)
returnee1$newcountry <- gsub("Bolivia \\(Plurinational State of\\)", "Bolivia, Plurinational State of", returnee1$newcountry)
returnee1$newcountry <- gsub("Cabo Verde", "Cape Verde", returnee1$newcountry)
returnee1$newcountry <- gsub("Central African Rep.", "Central African Republic", returnee1$newcountry)
returnee1$newcountry <- gsub("C̫te d'Ivoire", "Côte d'Ivoire", returnee1$newcountry)
returnee1$newcountry <- gsub("Czech Rep.", "Czech Republic", returnee1$newcountry)
returnee1$newcountry <- gsub("Dominican Rep.", "Dominican Republic", returnee1$newcountry)
returnee1$newcountry <- gsub("Iran \\(Islamic Rep. of\\)", "Iran, Islamic Republic of", returnee1$newcountry)
returnee1$newcountry <- gsub("Rep. of Korea", "Korea, Republic of", returnee1$newcountry)
returnee1$newcountry <- gsub("Lao People's Dem. Rep.", "Lao People's Democratic Republic", returnee1$newcountry)
returnee1$newcountry <- gsub("Libya", "Libyan Arab Jamahiriya", returnee1$newcountry)
returnee1$newcountry <- gsub("The former Yugoslav Republic of Macedonia", "Macedonia, the former Yugoslav Republic of", returnee1$newcountry)
returnee1$newcountry <- gsub("Rep. of Moldova", "Moldova, Republic of", returnee1$newcountry)
returnee1$newcountry <- gsub("Serbia and Kosovo \\(S/RES/1244 \\(1999\\)\\)", "Serbia", returnee1$newcountry)
returnee1$newcountry <- gsub("Syrian Arab Rep.", "Syrian Arab Republic", returnee1$newcountry)
returnee1$newcountry <- gsub("State of Palestine", "Palestinian", returnee1$newcountry)
returnee1$newcountry <- gsub("United Rep. of Tanzania", "Tanzania, United Republic of", returnee1$newcountry)
returnee1$newcountry <- gsub("United States of America", "United States", returnee1$newcountry)
returnee1$newcountry <- gsub("Sint Maarten (Dutch part)", "Netherlands Antilles", returnee1$newcountry)
returnee1$newcountry <- gsub("Venezuela \\(Bolivarian Republic of\\)", "Venezuela, Bolivarian Republic of", returnee1$newcountry)
returnee1$newcountry <- gsub("China, Hong Kong SAR", "Hong Kong", returnee1$newcountry)
returnee1$newcountry <- gsub("China, Macao SAR", "Macao", returnee1$newcountry)
returnee1$newcountry <- gsub("Micronesia \\(Federated States of\\)", "Micronesia, Federated States of", returnee1$newcountry)
returnee1$newcountry <- gsub("British Virgin Islands", "British Indian Ocean Territory", returnee1$newcountry)
returnee1$newcountry <- gsub("Dem. People's Rep. of Korea", "Korea, Democratic People's Republic of", returnee1$newcountry)
returnee1$newcountry <- gsub("Holy See \\(the\\)", "Holy See \\(Vatican City State\\)", returnee1$newcountry)

#Remove Garbage Rows
returnee1$country <- returnee1$newcountry
returnee1 <- returnee1[c(-2,-4,-7)]

#Change Country Names to Match nodelist
returnee1$neworigin <- returnee1$Origin
returnee1$neworigin <- gsub("Dem. Rep. of the Congo", "Congo, the Democratic Republic of the", returnee1$neworigin)
returnee1$neworigin <- gsub("Bolivia \\(Plurinational State of\\)", "Bolivia, Plurinational State of", returnee1$neworigin)
returnee1$neworigin <- gsub("Cabo Verde", "Cape Verde", returnee1$neworigin)
returnee1$neworigin <- gsub("Central African Rep.", "Central African Republic", returnee1$neworigin)
returnee1$neworigin <- gsub("C̫te d'Ivoire", "Côte d'Ivoire", returnee1$neworigin)
returnee1$neworigin <- gsub("Czech Rep.", "Czech Republic", returnee1$neworigin)
returnee1$neworigin <- gsub("Dominican Rep.", "Dominican Republic", returnee1$neworigin)
returnee1$neworigin <- gsub("Iran \\(Islamic Rep. of\\)", "Iran, Islamic Republic of", returnee1$neworigin)
returnee1$neworigin <- gsub("Rep. of Korea", "Korea, Republic of", returnee1$neworigin)
returnee1$neworigin <- gsub("Lao People's Dem. Rep.", "Lao People's Democratic Republic", returnee1$neworigin)
returnee1$neworigin <- gsub("Libya", "Libyan Arab Jamahiriya", returnee1$neworigin)
returnee1$neworigin <- gsub("The former Yugoslav Republic of Macedonia", "Macedonia, the former Yugoslav Republic of", returnee1$neworigin)
returnee1$neworigin <- gsub("Rep. of Moldova", "Moldova, Republic of", returnee1$neworigin)
returnee1$neworigin <- gsub("Serbia and Kosovo \\(S/RES/1244 \\(1999\\)\\)", "Serbia", returnee1$neworigin)
returnee1$neworigin <- gsub("Syrian Arab Rep.", "Syrian Arab Republic", returnee1$neworigin)
returnee1$neworigin <- gsub("State of Palestine", "Palestinian", returnee1$neworigin)
returnee1$neworigin <- gsub("United Rep. of Tanzania", "Tanzania, United Republic of", returnee1$neworigin)
returnee1$neworigin <- gsub("United States of America", "United States", returnee1$neworigin)
returnee1$neworigin <- gsub("Sint Maarten (Dutch part)", "Netherlands Antilles", returnee1$neworigin)
returnee1$neworigin <- gsub("Venezuela \\(Bolivarian Republic of\\)", "Venezuela, Bolivarian Republic of", returnee1$neworigin)
returnee1$neworigin <- gsub("China, Hong Kong SAR", "Hong Kong", returnee1$neworigin)
returnee1$neworigin <- gsub("China, Macao SAR", "Macao", returnee1$neworigin)
returnee1$neworigin <- gsub("Micronesia \\(Federated States of\\)", "Micronesia, Federated States of", returnee1$neworigin)
returnee1$neworigin <- gsub("British Virgin Islands", "British Indian Ocean Territory", returnee1$neworigin)
returnee1$neworigin <- gsub("Dem. People's Rep. of Korea", "Korea, Democratic People's Republic of", returnee1$neworigin)
returnee1$neworigin <- gsub("Holy See \\(the\\)", "Holy See \\(Vatican City State\\)", returnee1$neworigin)
#Remove Garbage Rows
returnee1$Origin <- returnee1$neworigin
returnee1 <- returnee1[c(-5)]

#Get Longitude and Latitude from Nodelist
returnee1$lat_dest <- cnt_lat_lon[match(returnee1$country,cnt_lat_lon$Country),"Latitude"]
returnee1$lon_dest <- cnt_lat_lon[match(returnee1$country,cnt_lat_lon$Country),"Longitude"]

returnee1$lat_org <- cnt_lat_lon[match(returnee1$Origin,cnt_lat_lon$Country),"Latitude"]
returnee1$lon_org <- cnt_lat_lon[match(returnee1$Origin,cnt_lat_lon$Country),"Longitude"]
#Get list of cases without NA
returnee1 <- returnee1[complete.cases(returnee1),]
#Calculate Edge Weight
returnee1$weight <- log(log(returnee1$Value^5)/4)-2
summary(returnee1$weight)
colnames(returnee1)[2]<-"Source"
colnames(returnee1)[4]<-"Target"
#Write Files
write.csv(returnee1,file = "Returnee_EdgeList.csv",row.names = FALSE)
write.csv(country_node,file = "Returnee_Nodes.csv",row.names = FALSE)
