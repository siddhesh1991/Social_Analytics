library(maps)
library(geosphere)
#Get File
returnee2 <- read.csv("Returnee_EdgeList.csv")
#Plot Map
map("world", col="#191919", fill=TRUE, bg="#000000",myborder = 0.01, lwd=0.001,wrap=TRUE,resolution=0)
#Create Color Pallete to be used according to edge weights
pal <- colorRampPalette(c("#7682FC", "#041AFC"))
colors <- pal(881)
#Order Edges per weight
returnee2 <- returnee2[order(returnee2$weight),]

for (j in 1:length(returnee2$Source)) {
    #Get Line between 2 countries in edge
    inter <- gcIntermediate(c(returnee2[j,]$lon_org, returnee2[j,]$lat_org), c(returnee2[j,]$lon_dest, returnee2[j,]$lat_dest), n=100, addStartEnd=TRUE)
    #Calculate Color Index to be used with color pallete
    colindex <- round( returnee2[j,]$weight * 1000 )
    #Plot the Line
    lines(inter, col=colors[colindex], lwd=0.005)
}





