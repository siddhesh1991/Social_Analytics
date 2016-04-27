
rm(list=ls()) 

# This is a 10% random sample for class exercises
infile_sub<-"Refugee_EdgeList.csv"

## Load package
library(igraph)
refugee=read.csv(infile_sub, header = TRUE, sep = ",")
el <- data.frame(refugee$Source,refugee$Target,refugee$Value)
colnames(el)[1]<-"source"
colnames(el)[2]<-"target"
colnames(el)[3]<-"Value"
el$weight <- el$Value/10000

g_SAPSub=graph.data.frame(el, directed = TRUE, vertices= NULL)

g_SAPSub_simpl<-simplify(g_SAPSub, edge.attr.comb="sum")
is.simple(g_SAPSub_simpl)

# Use the inverse of log weight for some of the network measure calculations
inv_weight<-1/log(E(g_SAPSub_simpl)$weight  + 1)
num_weight<-E(g_SAPSub_simpl)$weight 
length(inv_weight)
E(g_SAPSub_simpl)$weight <-inv_weight

## For example, there is only a path to 814 from 511; not from it. So there are outpaths from 511 to 814, but no in paths. (or the other vectors)
E(g_SAPSub_simpl)$weight <- inv_weight

reciprocity(g_SAPSub_simpl)
is.connected(g_SAPSub_simpl)
is.connected(g_SAPSub_simpl, mode="strong")
is.connected(g_SAPSub_simpl, mode="weak")

# Diameter with both kinds of weights

# Clustering
transitivity(g_SAPSub_simpl, weights = inv_weight)
# Avg. path length and diameter
average.path.length(g_SAPSub_simpl, directed=TRUE)
diameter(g_SAPSub_simpl)
diameter(g_SAPSub_simpl, weights= num_weight)
diameter(g_SAPSub_simpl, weights= inv_weight)
# Summarize the graph structure
summary(g_SAPSub_simpl)

# Clique structure: 5 cliques of size 5, 39 cliques of size 4, 335 triangles
table(sapply(maximal.cliques(g_SAPSub_simpl), length))
#A <- get.adjacency(g_SAPSub_simpl, sparse=FALSE)

# Can try either of these weighting schemes for various measures; they change the interpretation of the measures
# Inverse weight
E(g_SAPSub_simpl)$weight <- inv_weight
# Regular weight
E(g_SAPSub_simpl)$weight <- num_weight

# Embeddedness/ inverse of structural hole access (see Burt 2004)
constraints_SAP <- round(constraint(g_SAPSub_simpl, nodes=V(g_SAPSub_simpl)), digits=4)
# Degree centrality
degree_sap <- degree(g_SAPSub_simpl)
# Node betweenness
betweens_SAP <- round(betweenness(g_SAPSub_simpl, v=V(g_SAPSub_simpl), directed = TRUE, nobigint =TRUE, normalized = FALSE))
# Edge betwenness
edgebetweens_SAP<-edge.betweenness(g_SAPSub_simpl, e=E(g_SAPSub_simpl), directed = TRUE)
# Local clustering coefficients
clustering_SAP <- transitivity(g_SAPSub_simpl, type="local", vids=V(g_SAPSub_simpl)) 


# Plot set 2: Four plots 
library(ggplot2)
a_node<-aggregate(betweens_SAP ~ degree_sap, data=node_frame, mean)
plot <- ggplot(a_node, aes(y=log(betweens_SAP),x=degree_sap,legend = FALSE)) + geom_point(alpha = 0.7,color = "red1")
plot <- plot + theme(text= element_text(size=10))+ theme(plot.margin = unit(c(2,10,1,1), "cm"))
plot <- plot + ylab("Average Betweenness")+xlab("Degree")
plot 

a_node<-aggregate(clustering_SAP ~ degree_sap, data=node_frame, mean)
plot <- ggplot(a_node, aes(y=log(clustering_SAP),x=degree_sap,legend = FALSE)) + geom_point(alpha = 0.7,color = "red1")
plot <- plot + theme(text= element_text(size=10))+ theme(plot.margin = unit(c(2,10,1,1), "cm"))
plot <- plot + ylab("Average Clustering")+xlab("Degree")
plot
plot(a_node, col="blue", log="xy", xlab="Degree", ylab="Average Clustering")

a_node<-aggregate(constraints_SAP ~ degree_sap, data=node_frame, mean)
plot <- ggplot(a_node, aes(y=log(constraints_SAP),x=degree_sap,legend = FALSE)) + geom_point(alpha = 0.7,color = "red1")
plot <- plot + theme(text= element_text(size=10))+ theme(plot.margin = unit(c(2,10,1,1), "cm"))
plot <- plot + ylab("Average Constraint (Embeddedness)")+xlab("Degree")
plot
plot(a_node, col="blue", log="xy", xlab="Degree", ylab="Average Constraint (Embeddedness)")

