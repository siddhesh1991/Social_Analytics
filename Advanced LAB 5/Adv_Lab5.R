
library(igraph)
rm(list=ls())
filename <- "wide_twitter_daily_lab5.csv"

#Read Data
nasdaq <- read.csv(filename, header = TRUE, sep = ",")
nasdaq.matrix <- nasdaq[,-1]
nasdaq.matrix <- apply(nasdaq.matrix,2,as.numeric)
rownames(nasdaq.matrix) <- nasdaq[,1]

#Get Correlation Matrix
mycorr <- round(cor(nasdaq.matrix),2)
library(ggplot2)
library("plotly")
library(reshape2)
melted_cormat <- melt(mycorr)
head(melted_cormat)
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
    cormat[lower.tri(cormat)]<- NA
    return(cormat)
}
upper_tri <- get_upper_tri(mycorr)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

#Correlation Matrix
corr_plot <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+geom_tile(color = "white")
corr_plot <- corr_plot +scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Pearson\nCorrelation") 
corr_plot <- corr_plot+  theme(text= element_text(size=7),axis.text.y = element_blank(),axis.text.x = element_blank())
corr_plot <- corr_plot + ylab("Firms")+xlab("Firms")
corr_plot <- corr_plot +coord_fixed()
corr_plot

interactive_plot <- ggplotly(corr_plot)
interactive_plot

#Correlation Plot for Firms with Strong Positive Correlation
h_corr <- melted_cormat[(melted_cormat$value < 1) & (melted_cormat$value >0.4),]
h_corr_plot <- ggplot(h_corr, aes( Var1,Var2,fill =value,size = value),legend = FALSE) + geom_point(alpha = 0.7,color = "red1")
h_corr_plot <- h_corr_plot + theme(text= element_text(size=7))
h_corr_plot <- h_corr_plot + ylab("Firms")+xlab("Firms")
h_corr_plot
ggplotly(h_corr_plot)

#Link Prediction Using Partial Correlation
n <- dim(nasdaq.matrix)[1]
pcorr.pvals <- matrix(0, dim(mycorr)[1], 
                      dim(mycorr)[2])
for(i in seq(1, 92)){
    for(j in seq(1, 92)){
        rowi <- mycorr[i, -c(i, j)]
        rowj <- mycorr[j, -c(i, j)]
        tmp <- (mycorr[i, j] - 
                    rowi*rowj)/sqrt((1-rowi^2) * (1-rowj^2))
        tmp.zvals <- (0.5) * log((1+tmp) / (1-tmp))
        tmp.s.zvals <- sqrt(n-4) * tmp.zvals
        tmp.pvals <- 2 * pnorm(abs(tmp.s.zvals), 
                               0, 1, lower.tail=FALSE)
        pcorr.pvals[i, j] <- max(tmp.pvals)
    }
}

pcorr.pvals.vec <- pcorr.pvals[lower.tri(pcorr.pvals)]
# Benjamini-Hochberg adjustment to control for the false discovery rate
pcorr.pvals.adj <- p.adjust(pcorr.pvals.vec, "BH")
summary(pcorr.pvals.vec)
summary(pcorr.pvals.adj)

#Link Prediction at 0.05 Singnificance Level
pcorr.edges <- (pcorr.pvals.adj < 0.05)
pcorr.edges
length(pcorr.pvals.adj[pcorr.edges])

pcorr.A <- matrix(0, 92, 92)
rownames(pcorr.A) <- colnames(nasdaq.matrix)
colnames(pcorr.A) <- colnames(nasdaq.matrix)
pcorr.A[lower.tri(pcorr.A)] <- as.numeric(pcorr.edges)
g.pcorr <- graph.adjacency(pcorr.A, "undirected")
ecount(g.pcorr)
vcount(g.pcorr)

V(g.pcorr)$shape <- "circle"
V(g.pcorr)$size <- 1.20*sqrt(igraph::betweenness(g.pcorr))
V(g.pcorr)$label.cex= 0.27

g.pcorr1 <- delete.vertices(g.pcorr, V(g.pcorr)[ degree(g.pcorr)==0 ])

plot(g.pcorr1, edge.color="purple",
     vertex.color="purple", vertex.frame.color="#ffffff",
     vertex.label.color="black")

#Link Prediction at 0.01 Singnificance Level
pcorr.edges <- (pcorr.pvals.adj < 0.01)
pcorr.edges
length(pcorr.pvals.adj[pcorr.edges])

pcorr.A <- matrix(0, 92, 92)
rownames(pcorr.A) <- colnames(nasdaq.matrix)
colnames(pcorr.A) <- colnames(nasdaq.matrix)
pcorr.A[lower.tri(pcorr.A)] <- as.numeric(pcorr.edges)
g.pcorr <- graph.adjacency(pcorr.A, "undirected")

V(g.pcorr)$shape <- "circle"
V(g.pcorr)$size <- 2*sqrt(igraph::betweenness(g.pcorr))
V(g.pcorr)$label.cex= 0.27

g.pcorr1 <- delete.vertices(g.pcorr, V(g.pcorr)[ degree(g.pcorr)==0 ])

plot(g.pcorr1, edge.color="purple",
     vertex.color="purple", vertex.frame.color="#ffffff",
     vertex.label.color="black")


#Link Prediction Using Overall Correlation
z <- 0.5 * log((1 + mycorr) / (1 - mycorr))
z.vec <- z[upper.tri(z)]
pcorr.pvals.vec <- pcorr.pvals[lower.tri(pcorr.pvals)]
n <- dim(nasdaq.matrix)[1]
corr.pvals <- 2 * pnorm(abs(z.vec), 0, 
                        sqrt(1 / (n-3)), lower.tail=FALSE)


length(corr.pvals)
corr.pvals.adj <- p.adjust(corr.pvals, "BH")
corr.edges <- (corr.pvals.adj < 0.01)
length(corr.edges)
length(corr.pvals.adj[corr.edges])

corr.A <- matrix(0, 92, 92)
rownames(corr.A) <- colnames(nasdaq.matrix)
colnames(corr.A) <- colnames(nasdaq.matrix)
corr.A[lower.tri(corr.A)] <- as.numeric(corr.edges)
g.corr <- graph.adjacency(corr.A, "undirected")

V(g.corr)$shape <- "circle"
V(g.corr)$size <- 0.75*sqrt(igraph::betweenness(g.corr))
V(g.corr)$label.cex= 0.27

g.corr1 <- delete.vertices(g.corr, V(g.corr)[ degree(g.corr)==0 ])

plot(g.corr1, edge.color="purple",
     vertex.color="purple", vertex.frame.color="#ffffff",
     vertex.label.color="black")


#Link Prediction Using Huge

library(huge)
set.seed(42)
huge.out <- huge(nasdaq.matrix)

huge.opt <- huge.select(huge.out, criterion="stars")
h.adj <- as.matrix(huge.opt$refit)

rownames(h.adj) <- colnames(nasdaq.matrix)
colnames(h.adj) <- colnames(nasdaq.matrix)

g.huge <- graph.adjacency(h.adj, "undirected")
V(g.huge)$shape <- "circle"
V(g.huge)$size <- 2*sqrt(igraph::betweenness(g.pcorr))
V(g.huge)$label.cex= 0.27

g.pcorr1 <- delete.vertices(g.huge, V(g.huge)[ degree(g.huge)==0 ])

plot(g.pcorr1, edge.color="purple",
     vertex.color="purple", vertex.frame.color="#ffffff",
     vertex.label.color="black")




