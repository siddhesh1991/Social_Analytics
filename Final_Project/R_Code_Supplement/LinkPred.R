getwd()
dirpath <- "D:/Sid_Documents/Knowledge/Syllabus And Lecture/Social Analytics/Project/DATA/CleanedDataWithLatLong"
setwd(dirpath)

rm(list=ls()) 
infile<-"Refugee_EdgeList.csv"
refugee <- read.csv(infile, header = TRUE, sep = ",")
graph.data.source <- data.frame(refugee$Source,refugee$Year,refugee$Value)
colnames(graph.data.source)[1]<-"source"
colnames(graph.data.source)[2]<-"year"
colnames(graph.data.source)[3]<-"value"
graph.data.source <- graph.data.source[order(graph.data.source$year,graph.data.source$source),]
head(graph.data.source)
graph.data.source$year <- as.factor(graph.data.source$year)
class(graph.data.source$year)
aggregate.data.source <- aggregate.data.frame(graph.data.source$value,by = list(graph.data.source$source,graph.data.source$year),FUN = sum)
colnames(aggregate.data.source)[1]<-"source"
colnames(aggregate.data.source)[2]<-"year"
colnames(aggregate.data.source)[3]<-"value"
library(reshape2)
casted = dcast(aggregate.data.source, year~source )
casted[is.na(casted)]<-0
write.csv(casted,file="matrix.csv",row.names = FALSE)

#link.pred.source <- data.frame(table(aggregate.data.source$year,aggregate.data.source$source))
#Read Data
nasdaq <- casted
nasdaq.matrix <- nasdaq[,-1]
nasdaq.matrix <- apply(nasdaq.matrix,2,as.numeric)
rownames(nasdaq.matrix) <- nasdaq[,1]

#Link Prediction Using Partial Correlation
mycorr <- round(cor(nasdaq.matrix),2)
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
library(igraph)
pcorr.A <- matrix(0, 111, 111)
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

pcorr.A <- matrix(0, 111,111)
rownames(pcorr.A) <- colnames(nasdaq.matrix)
colnames(pcorr.A) <- colnames(nasdaq.matrix)
pcorr.A[lower.tri(pcorr.A)] <- as.numeric(pcorr.edges)
g.pcorr <- graph.adjacency(pcorr.A, "undirected")

V(g.pcorr)$shape <- "circle"
V(g.pcorr)$label.cex= 0.27
igraph.options(vertex.size=3,edge.arrow.size=0.05)
plot(g.pcorr1, vertex.color="purple", vertex.frame.color="#ffffff",vertex.size = 3,
     layout=layout.kamada.kawai,edge.width = 0.01,
     vertex.label.color="black",edge.color = adjustcolor("purple",alpha.f =0.4 ))

g.pcorr1 <- delete.vertices(g.pcorr, V(g.pcorr)[ degree(g.pcorr)==0 ])

plot(g.pcorr1, edge.color="purple",
     vertex.color="purple", vertex.frame.color="#ffffff",
     vertex.label.color="black")

library(huge)
set.seed(42)
huge.out <- huge(nasdaq.matrix)

huge.opt <- huge.select(huge.out, criterion="ric")
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

