#Name : SIDDHESH TIWARI
#UIN : 657796780
#email :stiwar3@uic.edu

# clear everything out of memory
getwd()
dirpath <- "D:/Sid_Documents/Knowledge/Syllabus And Lecture/Social Analytics/Project/DATA/CleanedDataWithLatLong"
setwd(dirpath)


rm(list=ls())  #Read in the hs0 data over the internet using the read.table() function.

## Load package
library(igraph)
library(ggplot2)

#Function to get degree distribution
get.distribution <- function(graph){
    d.count <- hist(degree(graph), breaks = -1:max(degree(graph)))
    num.of.nodes <- d.count$counts
    Degree <- d.count$breaks[2:length(d.count$breaks)]
    Degree.Dist <- data.frame(Degree,num.of.nodes)
    return(Degree.Dist)
}

#Function to fit Hybrid Model
Hybrid.fit <- function(alpha0,m,i_index,data){
    f.x <- 2/(1 - alpha0)
    X <- log(data$Degree + (alpha0*m*f.x))
    X[i_index] <- NA 
    na.omit(X)
    Y <- data$Y
    Y[i_index] <- NA
    na.omit(Y)
    fit <- lm(Y ~ X)
    return (fit$coefficients[2])
}

#Function to calculate Alpha1
Alpha.1 <- function(beta1){
    alpha1 <- (beta1+2)/beta1
    return(alpha1)
}

#Generate intial guess for alpha 
alpha_0 <- vector()
alpha_1 <- vector()
len <- seq(from=1,to=9, by = 0.001)

for(i in 1:length(len)) {
    alpha_0[i]<-len[i]/10
}
alpha_0[length(alpha_0)+1]<-0.99
alpha_0[length(alpha_0)+1]<-0.999
alpha_0[length(alpha_0)+1]<-0.9999

#Function to perform analysis on random growth on graph
get.analysis <- function(graph){
    Degree.Dist <- get.distribution(graph)
    Degree.Dist$DxP <- Degree.Dist$Degree * Degree.Dist$num.of.nodes
    avg.degree <- sum(Degree.Dist$DxP)/sum(Degree.Dist$num.of.nodes)
    F_d <- cumsum(Degree.Dist$num.of.nodes)/sum(Degree.Dist$num.of.nodes)
    Degree.Dist$CDF <- F_d
    m<- avg.degree/2
    Degree.Dist$Y <- log(1- F_d)
    i_index <- which(Degree.Dist$Y==-Inf)
    
    for( i in 1:length(alpha_0)){
        beta_1 <- Hybrid.fit(alpha0 = alpha_0[i], m = m,i_index = i_index ,data = Degree.Dist)
        alpha_1[i] <- Alpha.1(beta_1)
    }
    
    diff<- abs(alpha_1 - alpha_0)
    alphas<- data.frame(alpha_0,alpha_1,diff)
    plot<-  ggplot(alphas,aes(alpha_0,alpha_1)) + geom_line()
    plot <- plot + xlab("alpha_0")+ylab("alpha_1") +theme(text= element_text(size=10))
    plot.diff <-  ggplot(alphas,aes(alpha_0,diff)) + geom_line()
    plot.diff <- plot.diff + xlab("alpha_0")+ylab("Diff") +theme(text= element_text(size=10))
    analysis <- list(Degree.Dist,alphas,plot,plot.diff)
    return(analysis)
}


infile<-"Refugee_EdgeList.csv"
refugee <- read.csv(infile, header = TRUE, sep = ",")
graph.data <- data.frame(refugee$Source,refugee$Target,refugee$Year,refugee$weight)
colnames(graph.data)[1]<-"source"
colnames(graph.data)[2]<-"target"
colnames(graph.data)[3]<-"year"
colnames(graph.data)[4]<-"weight"
#1980-1988
graph.data.8088 <- graph.data[graph.data$year < 1990,]
graph.data.8088 <- graph.data.8088[c(-3,-4)]
graph.8088 <- graph.data.frame(graph.data.8088, directed = FALSE, vertices= NULL)
vcount(graph.8088)
analysis.8088 <- get.analysis(graph.8088)
Degree.Dist.8088 <- analysis.8088[[1]]
alphas.8088 <- analysis.8088[[2]]
plot.8088 <- analysis.8088[[3]]
plot.diff.8088 <- analysis.8088[[4]]
#Alpha value plots
plot.8088 + geom_line(colour="#000099")
plot.diff.8088 + geom_line(colour="#CC0000")

decompose.8088<- decompose.graph(graph.8088)
table(sapply(decompose.8088, vcount))
max <- which.max(sapply(decompose.8088, vcount))
graph.8088.sub <- decompose.8088[[max]]

igraph.options(vertex.size=3)
V(graph.8088.sub)$shape <- "circle"
V(graph.8088.sub)$color <- "red"
V(graph.8088.sub)["594918"]$color <- "green"
E(graph.8088.sub)$color <-"lightblue"
#Graph Plot
V(graph.8088.sub)$label.cex= 0.3
igraph.options(vertex.size=3,edge.arrow.size=0.05)

plot(graph.8088.sub,vertex.color="purple", vertex.frame.color="#ffffff",vertex.size = 3,
     layout = layout.gem,edge.width = 1,
     vertex.label.color="black",edge.color = adjustcolor("purple",alpha.f =0.4 ))
title("Largest Connected Component 2007")

analysis.8088.sub <- get.analysis(graph.8088.sub)
Degree.Dist.8088.sub <- analysis.8088.sub[[1]]
alphas.8088.sub <- analysis.8088.sub[[2]]
plot.8088.sub <- analysis.8088.sub[[3]]
plot.diff.8088.sub <- analysis.8088.sub[[4]]
#Alpha value plots
plot.8088.sub + geom_line(colour="#000099")
plot.diff.8088.sub + geom_line(colour="#CC0000")


#1990-2000
graph.data.9020 <- graph.data[(graph.data$year > 1989) & (graph.data$year < 2001),]
graph.data.9020 <- graph.data.9020[c(-3,-4)]
graph.9020 <- graph.data.frame(graph.data.9020, directed = FALSE, vertices= NULL)
vcount(graph.9020)
analysis.9020 <- get.analysis(graph.9020)
Degree.Dist.9020 <- analysis.9020[[1]]
alphas.9020 <- analysis.9020[[2]]
plot.9020 <- analysis.9020[[3]]
plot.diff.9020 <- analysis.9020[[4]]
#Alpha value plots
plot.9020 + geom_line(colour="#000099")
plot.diff.9020 + geom_line(colour="#CC0000")

decompose.9020<- decompose.graph(graph.9020)
table(sapply(decompose.9020, vcount))
max <- which.max(sapply(decompose.9020, vcount))
graph.9020.sub <- decompose.9020[[max]]


igraph.options(vertex.size=3)
V(graph.9020.sub)$shape <- "circle"
V(graph.9020.sub)$color <- "purple"
E(graph.9020.sub)$color <-"lightblue"
V(graph.9020.sub)["606769"]$color <- "blue"
#Graph Plot
plot(graph.9020.sub,vertex.label=NA)
title("Largest Connected Component 2014")

plot(graph.9020.sub)

analysis.9020.sub <- get.analysis(graph.9020.sub)
Degree.Dist.9020.sub <- analysis.9020.sub[[1]]
alphas.9020.sub <- analysis.9020.sub[[2]]
plot.9020.sub <- analysis.9020.sub[[3]]
plot.diff.9020.sub <- analysis.9020.sub[[4]]
#Alpha value plots
plot.9020.sub + geom_line(colour="#000099")
plot.diff.9020.sub + geom_line(colour="#CC0000")


#2001
graph.data.2001 <- graph.data[(graph.data$year > 2000),]
graph.data.2001 <- graph.data.2001[c(-3,-4)]
graph.2001 <- graph.data.frame(graph.data.2001, directed = FALSE, vertices= NULL)
vcount(graph.2001)
analysis.2001 <- get.analysis(graph.2001)
Degree.Dist.2001 <- analysis.2001[[1]]
alphas.2001 <- analysis.2001[[2]]
plot.2001 <- analysis.2001[[3]]
plot.diff.2001 <- analysis.2001[[4]]
#Alpha value plots
plot.2001 + geom_line(colour="#000099")
plot.diff.2001 + geom_line(colour="#CC0000")

decompose.2001<- decompose.graph(graph.2001)
table(sapply(decompose.2001, vcount))
max <- which.max(sapply(decompose.2001, vcount))
graph.2001.sub <- decompose.2001[[max]]


analysis.2001.sub <- get.analysis(graph.2001.sub)
Degree.Dist.2001.sub <- analysis.2001.sub[[1]]
alphas.2001.sub <- analysis.2001.sub[[2]]
plot.2001.sub <- analysis.2001.sub[[3]]
plot.diff.2001.sub <- analysis.2001.sub[[4]]
#Alpha value plots
plot.2001.sub + geom_line(colour="#000099")
plot.diff.2001.sub + geom_line(colour="#CC0000")
