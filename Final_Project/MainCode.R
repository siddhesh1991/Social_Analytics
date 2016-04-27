getwd()
dirpath <- "D:/Sid_Documents/Knowledge/Syllabus And Lecture/Social Analytics/Project/DATA/CleanedDataWithLatLong"
setwd(dirpath)

rm(list=ls())  
infile<-"Refugee_EdgeList.csv"

library(igraph)
refugee <- read.csv(infile, header = TRUE, sep = ",")
#Refine Data for Igraph
graph.data <- data.frame(refugee$Source,refugee$Target,refugee$Year,refugee$weight)
colnames(graph.data)[1]<-"source"
colnames(graph.data)[2]<-"target"
colnames(graph.data)[3]<-"year"
colnames(graph.data)[4]<-"weight"

#Create Graph
g_refugee=graph.data.frame(graph.data, directed = TRUE, vertices= NULL)
#USe the Largest Connected Component
g.decompose <- decompose(g_refugee)
g.refugee <- g.decompose[[1]]
ecount.full <- c("Edge Count Full",ecount(g.refugee))
vcount.full <- c("Vertex Count Full",vcount(g.refugee))
is.simple(g.refugee)
#Simplify Graph
g.refugee.simplify <- simplify(g.refugee)
graph.simplify <- get.data.frame(g.refugee.simplify)
colnames(graph.simplify)[1] <- "Source"
colnames(graph.simplify)[2] <- "Target"
write.csv(graph.simplify,file="Graph_Simplify.csv",row.names = FALSE)
is.simple(g.refugee.simplify)
ecount.simplify <- c("Edge Count Simplify",ecount(g.refugee.simplify))
vcount.simplify <- c("Vertex Count Simplify",vcount(g.refugee.simplify))

#Initial Network
is.connected(g.refugee.simplify, mode = c("weak"))
is.connected(g.refugee.simplify, mode = c("strong"))
is.connected(g.refugee.simplify)

diameter(g.refugee.simplify)


#Betweeness
betweeness.data <- data.frame(betweenness(g.refugee.simplify,directed = TRUE))
names <- rownames(betweeness.data)
betweeness.data$Names <- names
colnames(betweeness.data)[1] <- "Betweeness"    
betweeness.data <- betweeness.data[order(betweeness.data$Betweeness,decreasing = TRUE),]
write.csv(betweeness.data,file="Betweeness.csv",row.names = FALSE)

#Closeness Centrality
diameter(g.refugee.simplify)

closeness.out.data <- data.frame(closeness(g.refugee.simplify,mode = "out"))
clo.out.names <- rownames(closeness.out.data)
closeness.out.data$Names <- clo.out.names
colnames(closeness.out.data)[1] <- "Closeness_Out"     
closeness.out.data <- closeness.out.data[order(closeness.out.data$Closeness,decreasing = TRUE),]
write.csv(closeness.out.data,file="Closeness_Out.csv",row.names = FALSE)

closeness.in.data <- data.frame(closeness(g.refugee.simplify,mode = "in"))
clo.in.names <- rownames(closeness.in.data)
closeness.in.data$Names <- clo.in.names
colnames(closeness.in.data)[1] <- "Closeness_In"     
closeness.in.data <- closeness.in.data[order(closeness.in.data$Closeness,decreasing = TRUE),]
write.csv(closeness.in.data,file="Closeness_In.csv",row.names = FALSE)
closeness_merged <- merge(closeness.in.data,closeness.out.data, by = "Names")
write.csv(closeness_merged,file="Closeness_Merged.csv",row.names = FALSE)

#Cluster Coefficient
transitivity <- c("Transitivity",transitivity(g.refugee.simplify))
#Shortest Path
shortest.path.out.data <- data.frame(shortest.paths(g.refugee.simplify,mode = "out"))
shortest.path.out.data.names <- rownames(shortest.path.out.data)
shortest.path.out.data <- cbind(shortest.path.out.data.names, shortest.path.out.data)
colnames(shortest.path.out.data)[1] <- "Country" 
write.csv(shortest.path.out.data,file="Shortestpath_out.csv",row.names = FALSE)

library(reshape2)
shortest.path.out.data.melt <- melt(shortest.path.out.data)
colnames(shortest.path.out.data.melt)[[1]] <- "From"
colnames(shortest.path.out.data.melt)[[2]] <- "To"
shortest.path.out.data.melt <- shortest.path.out.data.melt[shortest.path.out.data.melt$value !="Inf",]
shortest.path.out.data.melt <- shortest.path.out.data.melt[shortest.path.out.data.melt$value !=0,]
shortest.path.out.data.melt <- shortest.path.out.data.melt[order(shortest.path.out.data.melt$value),]
write.csv(shortest.path.out.data.melt,file="Shortestpath_out_Melt.csv",row.names = FALSE)


#EigenCentrality
eigen.data <- data.frame(eigen_centrality(g.refugee.simplify,directed = TRUE, scale = TRUE)$vector)
eigen.names <- rownames(eigen.data)
eigen.data$Names <- eigen.names
colnames(eigen.data)[1] <- "Eigen"    
eigen.data <- eigen.data[order(eigen.data$Eigen,decreasing = TRUE),]
write.csv(eigen.data,file="Eigen.csv",row.names = FALSE)

#Embeddedness
graph.data$Value <- refugee$Value
weight <- graph.data$weight
graph.data$weight <- graph.data$Value/10000
g.embed=graph.data.frame(graph.data, directed = TRUE, vertices= NULL)
g.embed.simple<-simplify(g.embed, edge.attr.comb="sum")
is.simple(g.embed.simple)
embeddeness.data <- data.frame(round(constraint(g.embed.simple, nodes=V(g.embed.simple)), digits=4))
embeddeness.names <- rownames(embeddeness.data)
embeddeness.data$Names <- embeddeness.names
colnames(embeddeness.data)[1] <- "Embeddeness"    
embeddeness.data <- embeddeness.data[order(embeddeness.data$Embeddeness,decreasing = TRUE),]
write.csv(embeddeness.data,file="Embeddeness.csv",row.names = FALSE)
reciprocity.ref <- c("Transitivity",reciprocity(g.refugee.simplify))
network.properties <- rbind(ecount.full,vcount.full,ecount.simplify,vcount.simplify,transitivity,reciprocity.ref)
write.csv(network.properties,file = "Network_Prop.csv",row.names = FALSE)
#Degree Centrality
degree.data.in <- data.frame(degree(g.refugee.simplify,mode = "in"))
degree.names <- rownames(degree.data.in)
degree.data.in$Names <- degree.names
colnames(degree.data.in)[1] <- "DegreeIn"    
degree.data.in <- degree.data.in[order(degree.data.in$DegreeIn,decreasing = TRUE),]
write.csv(degree.data.in,file="DegreeIn.csv",row.names = FALSE)
#Degree Centrality
degree.data.out <- data.frame(degree(g.refugee.simplify,mode = "out"))
degree.names <- rownames(degree.data.out)
degree.data.out$Names <- degree.names
colnames(degree.data.out)[1] <- "DegreeOut"    
degree.data.out <- degree.data.out[order(degree.data.out$DegreeOut,decreasing = TRUE),]
write.csv(degree.data.out,file="DegreeOut.csv",row.names = FALSE)

par(mfrow=c(1,1))
V(g.refugee.simplify)$label.cex= 0.3
igraph.options(vertex.size=3,edge.arrow.size=0.05)
plot(g.refugee.simplify, vertex.color="purple", vertex.frame.color="#ffffff",vertex.size = 3,
     layout = layout.gem,edge.width = E(g.refugee.simplify)$weight/5,
     vertex.label.color="black",edge.color = adjustcolor("purple",alpha.f =0.4 ))
title("Sub Network In")


#Subnet
subnet.graph <- function(con){
neighbours <- neighbors(g.refugee.simplify, v=c(con))
sub.net.india<-induced.subgraph(g.refugee.simplify, v=append(neighbours$name, con))
V(sub.net.india)$label.cex= 0.3
igraph.options(vertex.size=3,edge.arrow.size=0.05)
plot(sub.net.india, vertex.color="purple", vertex.frame.color="#ffffff",vertex.size = 3,
     layout = layout.gem,edge.width = E(sub.net.india)$weight/5,
     vertex.label.color="black",edge.color = adjustcolor("purple",alpha.f =0.4 ))
title(c(con, "Sub Network Out"))
}
subnet.graph.in <- function(con){
    neighbours <- neighbors(g.refugee.simplify, v=c(con),mode = "in")
    sub.net.india<-induced.subgraph(g.refugee.simplify, v=append(neighbours$name, con))
    V(sub.net.india)$label.cex= 0.3
    igraph.options(vertex.size=3,edge.arrow.size=0.05)
    plot(sub.net.india, vertex.color="purple", vertex.frame.color="#ffffff",vertex.size = 3,
         layout = layout.gem,edge.width = E(sub.net.india)$weight/5,
         vertex.label.color="black",edge.color = adjustcolor("purple",alpha.f =0.4 ))
    title(c(con, "Sub Network In"))
}
subnet.graph(con = "Afghanistan")
subnet.graph(con = "Congo, the Democratic Republic of the")
subnet.graph(con = "Lao People's Democratic Republic")
subnet.graph(con = "Iran, Islamic Republic of")
subnet.graph(con = "Viet Nam")
subnet.graph(con = "India")
subnet.graph.in(con = "India")
subnet.graph.in(con = "United States")
subnet.graph.in(con = "Brazil")
subnet.graph.in(con = "Germany")
subnet.graph.in(con = "Canada")
subnet.graph.in(con = "Russian Federation")
subnet.graph(con = "Russian Federation")
subnet.graph.in(con = "France")
subnet.graph.(con = "France")
subnet.graph.in(con = "Italy")
subnet.graph.in(con = "South Africa")
subnet.graph(con = "South Africa")
subnet.graph.in(con = "China")
subnet.graph(con = "China")
subnet.graph.in(con = "Japan")
subnet.graph.in(con = "Saudi Arabia")
subnet.graph.in(con = "Saudi Arabia")
#Community detection
g_refugee_commun=graph.data.frame(graph.data, directed = FALSE, vertices= NULL)
#USe the Largest Connected Component
g.decompose <- decompose(g_refugee_commun)
g.refugee.commun.undir <- g.decompose[[1]]
ecount(g.refugee.commun.undir)
g.refugee.commun.undir <- simplify(g.refugee.commun.undir)
ecount(g.refugee.commun.undir)

g.refugee.commun.dir <- g.refugee.simplify

#Fast Greedy
g.refugee.fast <- fastgreedy.community(g.refugee.commun.undir, weights=E(g.refugee.commun.undir)$weight)
V(g.refugee.commun.undir)$label.cex= 0.3
plot(g.refugee.fast,g.refugee.commun.undir, vertex.color="purple", vertex.frame.color="#ffffff",
     vertex.size = 3,edge.width = E(g.refugee.commun.undir)$weight/5,edge.arrow.size = 0.3,
     vertex.label.color="black",edge.color = adjustcolor("purple",alpha.f =0.4 ))
title("Fast greedy Algorithm")
c.m.fast <- membership(g.refugee.fast)
table(c.m.fast)
data.frame(c.m.fast)
#Community Detection using Walktrap
g.refugee.walktrap <- walktrap.community(g.refugee.commun.dir,step = 6, weights=E(g.refugee.commun.dir)$weight)
length(g.refugee.walktrap)
c.m.walktrap <- membership(g.refugee.walktrap)
V(g.refugee.commun.dir)$label.cex= 0.3
plot(g.refugee.walktrap,g.refugee.commun.dir, vertex.color="purple", vertex.frame.color="#ffffff",
     vertex.size = 3,edge.width = E(g.refugee.commun.undir)$weight/5,edge.arrow.size = 0.3,
     vertex.label.color="black",edge.color = adjustcolor("purple",alpha.f =0.4 ))
title("WalkTrap Algorithm")

#Community Detection using Spinglass
g.refugee.spinglass<- spinglass.community(g.refugee.commun.dir,spins = 60, weights=E(g.refugee.commun.dir)$weight)
length(g.refugee.spinglass)
c.m.spinglass <- membership(g.refugee.spinglass)
V(g.refugee.commun.dir)$label.cex= 0.3
plot(g.refugee.spinglass,g.refugee.commun.dir, vertex.color="purple", vertex.frame.color="#ffffff",
     vertex.size = 3,edge.width = E(g.refugee.commun.undir)$weight/5,edge.arrow.size = 0.3,
     vertex.label.color="black",edge.color = adjustcolor("purple",alpha.f =0.4 ))
title("Spinglass Algorithm")

#Community Detection using Label PRopogation
g.refugee.label<- label.propagation.community(g.refugee.commun.dir,weights=E(g.refugee.commun.dir)$weight)
length(g.refugee.label)
c.m.label <- membership(g.refugee.label)
V(g.refugee.commun.dir)$label.cex= 0.3
plot(g.refugee.label,g.refugee.commun.dir, vertex.color="purple", vertex.frame.color="#ffffff",
     vertex.size = 3,edge.width = E(g.refugee.commun.undir)$weight/5,edge.arrow.size = 0.3,
     vertex.label.color="black",edge.color = adjustcolor("purple",alpha.f =0.4 ))
title("Label Propogation Algorithm")

#Community Detection using Girvan-Newman
g.refugee.gn<- edge.betweenness.community(g.refugee.commun.dir,weights=E(g.refugee.commun.dir)$weight)
length(g.refugee.gn)
c.m.gn <- membership(g.refugee.gn)
V(g.refugee.commun.dir)$label.cex= 0.3
plot(g.refugee.gn,g.refugee.commun.dir, vertex.color="purple", vertex.frame.color="#ffffff",
     vertex.size = 3,edge.width = E(g.refugee.commun.undir)$weight/5,edge.arrow.size = 0.3,
     vertex.label.color="black",edge.color = adjustcolor("purple",alpha.f =0.4 ))
title("Label Propogation Algorithm")



#Get Communities based on Fast
country_final <- get.vertex.attribute(g.refugee.commun.undir, "name")
country_table_final.fast = data.frame(table(c.m.fast,country_final, useNA = c("no")))
country_table_final.fast <- country_table_final.fast[country_table_final.fast$Freq>0,]
country_table_final.fast <- country_table_final.fast[c(-3)]
colnames(country_table_final.fast)[1] <- "Communities"
colnames(country_table_final.fast)[2] <- "Country"
write.csv(country_table_final.fast,file = "Communities_Fast.csv",row.names = FALSE)

#Get Communities based on Walktrap
country_final <- get.vertex.attribute(g.refugee.commun.dir, "name")
country_table_final.walktrap = data.frame(table(c.m.walktrap,country_final, useNA = c("no")))
country_table_final.walktrap <- country_table_final.walktrap[country_table_final.walktrap$Freq>0,]
sum(country_table_final.walktrap$Freq)
country_table_final.walktrap <- country_table_final.walktrap[c(-3)]
colnames(country_table_final.walktrap)[1] <- "Communities"
colnames(country_table_final.walktrap)[2] <- "Country"
summary(country_table_final.walktrap$Communities)
write.csv(country_table_final.walktrap,file = "Communities_Walktrap.csv",row.names = FALSE)

#Spinglass
country_table_final.spinglass= data.frame(table(c.m.spinglass,country_final, useNA = c("no")))
country_table_final.spinglass <- country_table_final.spinglass[country_table_final.spinglass$Freq>0,]
sum(country_table_final.spinglass$Freq)
country_table_final.spinglass <- country_table_final.spinglass[c(-3)]
colnames(country_table_final.spinglass)[1] <- "Communities"
colnames(country_table_final.spinglass)[2] <- "Country"
write.csv(country_table_final.spinglass,file = "Communities_spinglass.csv",row.names = FALSE)

#Label Prop
country_table_final.labelProp= data.frame(table(c.m.label,country_final, useNA = c("no")))
country_table_final.labelProp <- country_table_final.labelProp[country_table_final.labelProp$Freq>0,]
sum(country_table_final.labelProp$Freq)
country_table_final.labelProp <- country_table_final.labelProp[c(-3)]
colnames(country_table_final.labelProp)[1] <- "Communities"
colnames(country_table_final.labelProp)[2] <- "Country"
write.csv(country_table_final.labelProp,file = "Communities_labelProp.csv",row.names = FALSE)

#Label Prop
country_table_final.gn= data.frame(table(c.m.gn,country_final, useNA = c("no")))
country_table_final.gn <- country_table_final.gn[country_table_final.gn$Freq>0,]
sum(country_table_final.gn$Freq)
country_table_final.gn <- country_table_final.gn[c(-3)]
colnames(country_table_final.gn)[1] <- "Communities"
colnames(country_table_final.gn)[2] <- "Country"
write.csv(country_table_final.gn,file = "Communities_gn.csv",row.names = FALSE)






