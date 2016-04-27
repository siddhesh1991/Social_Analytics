
# clear everything out of memory
rm(list=ls())  

# Load primary school data, contact data
infile_edges<-"CollabNetEdgeListFilteredDec7_2012.csv"
infile_nodes<-"NodesNetList_corrected_Feb19_2016.csv"

## Load package
library(igraph)
edge_frame=read.csv(infile_edges, header = TRUE, sep = ",")
node_frame=read.csv(infile_nodes, header = TRUE, sep = ",")

#Community detection on full dataset
g_sap=graph.data.frame(edge_frame, directed = FALSE, vertices= node_frame)
E(g_sap)$weight <-1
g_simple<-simplify(g_sap, edge.attr.comb="sum")
vcount(g_simple)
ecount(g_simple)
sap_fast_raw <- fastgreedy.community(g_simple, weights=E(g_simple)$weight)
length(sap_fast_raw)

#Function to preprocess graph data and return graph
generate.graph <- function(edge_frame,node_frame,dir = FALSE){
    g_sapcomm=graph.data.frame(edge_frame, directed = FALSE, vertices= node_frame)
    E(g_sapcomm)$weight <-1
    g_sap_simple<-simplify(g_sapcomm, edge.attr.comb="sum")
    g_sap_sub <- subgraph.edges(graph = g_sap_simple,eids = E(g_sap_simple),delete.vertices = TRUE)
    g_sap_sub <- delete_vertex_attr(g_sap_sub,"ln_fdi")
    g_sap_sub <- delete_vertex_attr(g_sap_sub,"fdi_pergdp")
    g_sap_sub <- delete_vertex_attr(g_sap_sub,"ICT_goods_import_percent")
    g_sap_sub <- delete_vertex_attr(g_sap_sub,"internet_users_percent")
    g_sap_sub <- delete_vertex_attr(g_sap_sub,"immigration_pct")
    g_sap_sub <- delete_vertex_attr(g_sap_sub,"lat")
    g_sap_sub <- delete_vertex_attr(g_sap_sub,"lng")
    g_sap_decompose <- decompose.graph(g_sap_sub)
    g_sap_final <- g_sap_decompose[[1]] #every other connected network had only 2-3 nodes
    return(g_sap_final)
}

#Function call to get processed graph
g_sap_dir <- generate.graph(edge_frame,node_frame,dir = TRUE) 
g_sap_undir <- generate.graph(edge_frame,node_frame,dir = FALSE) 

#Stats for processed graph
vcount(g_sap_dir)
vcount(g_sap_undir)
is.connected(g_sap_undir)
is.connected(g_sap_dir)
par(mfrow=c(1,1))

# Community detection using the Fast Greedy Algorithm
sap_fast <- fastgreedy.community(g_sap_undir, weights=E(g_sap_undir)$weight)
length(sap_fast)
plot(sap_fast,g_sap_undir, vertex.label= NA, vertex.size = 1)
title("Fast greedy Algorithm")
vcount(g_sap_undir)
ecount(g_sap_undir)
length(sap_fast)
c.m.fast <- membership(sap_fast)
table(c.m.fast,country_final, useNA = c("no"))
#Get country list and frequency
country_final <- get.vertex.attribute(g_sap_undir, "country")
table
country_table_final = data.frame(table(country_final, useNA = c("no")))

#Get Country list with frequency greater than 20
List_Country <- country_table_final$country_final[country_table_final$Freq>20]
List_Country <- as.array(List_Country)

library(dplyr)
library(ggplot2)

#Create data frame for Community versus Nationality Analysis
fast_table <- data.frame(table(c.m.fast,country_final, useNA = c("no")))
fast_table <- filter(fast_table, fast_table$Freq >0)
fast_table <- fast_table[fast_table$country_final%in% List_Country,]
fast_table <- fast_table[56:1648,]

#create plot of Community vs Country
fast_plot <- ggplot(fast_table, aes( country_final,c.m.fast),legend = FALSE) + geom_point(alpha = 0.5)
fast_plot <- fast_plot + theme(text= element_text(size=7),axis.text.x = element_text(angle=90, vjust=0.3),axis.text.y = element_blank())
fast_plot <- fast_plot + ylab("Communities")+xlab("Country")
fast_plot
##create bubble chart of Community vs Country
fast_plot <- ggplot(fast_table, aes( country_final,c.m.fast,size = log(log(Freq))),legend = FALSE) + geom_point(alpha = 0.3)
fast_plot <- fast_plot + theme(text= element_text(size=7),axis.text.x = element_text(angle=90, vjust=0.3),axis.text.y = element_blank())
fast_plot <- fast_plot + ylab("Communities")+xlab("Country")
fast_plot

#Write data to csv file
write.csv(fast_table,"Fast_table.csv")

#create data frame to Communities and their award point
award_table<- data.frame(sap_fast$names,sap_fast$membership)
ln_point_table <- data.frame(node_frame$Id,node_frame$ln_points)
merge_award <- merge(award_table,ln_point_table, by.x = "sap_fast.names", by.y = "node_frame.Id", all.x = TRUE, all.y = FALSE, sort = TRUE)

#Aggregate award point at community level
com_award_score <- aggregate(node_frame.ln_points ~ sap_fast.membership, merge_award, sum)

#Plot Community versus Award points
award_plot <- ggplot(com_award_score,aes(sap_fast.membership,node_frame.ln_points)) + geom_line()
award_plot <- award_plot + xlab("Communities")+ylab("Aggregated Award Score") +theme(text= element_text(size=7))
award_plot
award_plot <- ggplot(com_award_score,aes(sap_fast.membership,log(node_frame.ln_points))) + geom_line()
award_plot <- award_plot + xlab("Communities")+ylab("log(Aggregated Award Score)") +theme(text= element_text(size=7))
award_plot


#Community Detection using Walktrap
sap_walktrap <- walktrap.community(g_sap_dir,step = 6, weights=E(g_sap_dir)$weight)
length(sap_walktrap)
c.m.walktrap <- membership(sap_walktrap)
plot(sap_walktrap,g_sap_dir, vertex.label= NA, vertex.size=2)
title("WalkTrap Algorithm")

#Community Detection using Spinglass
sap_spinglass <- spinglass.community(g_sap_dir, weights=E(g_sap_dir)$weight,spins = 60)
c.m.spinglass <- membership(sap_spinglass)
table(c.m.spinglass, stud.class, useNA = c("no"))
plot(sap_spinglass,g_sap_sub, vertex.label= NA)
title("Spinglass Algorithm")

#Community Detection using Label PRopogation
sap_labelprop <- label.propagation.community(g_sap_dir, weights=E(g_sap_dir)$weight)
length(sap_labelprop)
c.m.labelprop <- membership(sap_fast_labelprop)
table(c.m.labelprop, stud.class, useNA = c("no"))
plot(sap_fast_labelprop,g_primschool, vertex.label= NA, vertex.size=2)
title("Label Propogation Algorithm")

#Community Detection using Girvan-Newman
sap_girvan<- edge.betweenness.community(g_sap_dir, weights=E(g_sap_dir)$weight)
c.m.gn <- membership(sap_fast_girvan)
table(c.m.gn, stud.class, useNA = c("no"))
par(mfrow = c(1,1))
plot(sap_fast_girvan,g_primschool, vertex.label= NA, vertex.size=2)
title("Girvan-Newman Algorithm")


