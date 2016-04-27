library(maps)
library(geosphere)
#Get File
refugee2 <- read.csv("Refugee_EdgeList.csv")
#Plot Map
map("world", col="#191919", fill=TRUE, bg="#000000",myborder = 0.01, lwd=0.001,wrap=TRUE,resolution=0)
#Create Color Pallete to be used according to edge weights
pal <- colorRampPalette(c("#FA6363", "#FF0101"))
colors <- pal(830)
#Order Edges per weight
refugee2 <- refugee2[order(refugee2$weight),]

for (j in 1:length(refugee2$Source)) {
    #Get Line between 2 countries in edge
    inter <- gcIntermediate(c(refugee2[j,]$lon_org, refugee2[j,]$lat_org), c(refugee2[j,]$lon_dest, refugee2[j,]$lat_dest), n=100, addStartEnd=TRUE)
    #Calculate Color Index to be used with color pallete
    colindex <- round( refugee2[j,]$weight * 800 )
    #Plot the Line
    lines(inter, col=colors[colindex], lwd=0.005)
}





