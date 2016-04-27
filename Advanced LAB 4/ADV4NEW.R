#Name : SIDDHESH TIWARI
#UIN : 657796780
#email :stiwar3@uic.edu

# clear everything out of memory
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

infile <- "Share_corp_alliance_EdgeList_2007.csv"
Corp.2007 <- read.csv(infile, header = TRUE, sep = ",")
graph.2007 <- graph.data.frame(Corp.2007, directed = FALSE, vertices= NULL)
vcount(graph.2007)
analysis.2007 <- get.analysis(graph.2007)
Degree.Dist.2007 <- analysis.2007[[1]]
alphas.2007 <- analysis.2007[[2]]
plot.2007 <- analysis.2007[[3]]
plot.diff.2007 <- analysis.2007[[4]]
#Alpha value plots
plot.2007 + geom_line(colour="#000099")
plot.diff.2007 + geom_line(colour="#CC0000")

decompose.2007<- decompose.graph(graph.2007)
table(sapply(decompose.2007, vcount))
max <- which.max(sapply(decompose.2007, vcount))
graph.2007.sub <- decompose.2007[[max]]

igraph.options(vertex.size=20)
V(graph.2007.sub)$shape <- "circle"
V(graph.2007.sub)$color <- "red"
V(graph.2007.sub)["594918"]$color <- "green"
E(graph.2007.sub)$color <-"lightblue"
#Graph Plot
plot(graph.2007.sub,vertex.label = NA)
title("Largest Connected Component 2007")

analysis.2007.sub <- get.analysis(graph.2007.sub)
Degree.Dist.2007.sub <- analysis.2007.sub[[1]]
alphas.2007.sub <- analysis.2007.sub[[2]]
plot.2007.sub <- analysis.2007.sub[[3]]
plot.diff.2007.sub <- analysis.2007.sub[[4]]
#Alpha value plots
plot.2007.sub + geom_line(colour="#000099")
plot.diff.2007.sub + geom_line(colour="#CC0000")

infile <- "Share_corp_alliance_EdgeList_2014.csv"
Corp.2014 <- read.csv(infile, header = TRUE, sep = ",")
graph.2014 <- graph.data.frame(Corp.2014, directed = FALSE, vertices= NULL)
vcount(graph.2014)
analysis.2014 <- get.analysis(graph.2014)
Degree.Dist.2014 <- analysis.2014[[1]]
alphas.2014 <- analysis.2014[[2]]
plot.2014 <- analysis.2014[[3]]
plot.diff.2014 <- analysis.2014[[4]]
#Alpha value plots
plot.2014 + geom_line(colour="#000099")
plot.diff.2014 + geom_line(colour="#CC0000")

decompose.2014<- decompose.graph(graph.2014)
table(sapply(decompose.2014, vcount))
max <- which.max(sapply(decompose.2014, vcount))
graph.2014.sub <- decompose.2014[[max]]

igraph.options(vertex.size=20)
V(graph.2014.sub)$shape <- "circle"
V(graph.2014.sub)$color <- "purple"
E(graph.2014.sub)$color <-"lightblue"
V(graph.2014.sub)["606769"]$color <- "blue"
#Graph Plot
plot(graph.2014.sub,vertex.label=NA)
title("Largest Connected Component 2014")

plot(graph.2014.sub)

analysis.2014.sub <- get.analysis(graph.2014.sub)
Degree.Dist.2014.sub <- analysis.2014.sub[[1]]
alphas.2014.sub <- analysis.2014.sub[[2]]
plot.2014.sub <- analysis.2014.sub[[3]]
plot.diff.2014.sub <- analysis.2014.sub[[4]]
#Alpha value plots
plot.2014.sub + geom_line(colour="#000099")
plot.diff.2014.sub + geom_line(colour="#CC0000")

merged.data <- merge(Degree.Dist.2014,Degree.Dist.2007,by="Degree",all.x = TRUE,all.y = TRUE)

plot.comp <- ggplot(merged.data, aes(x = Degree)) + geom_line(aes(y = num.of.nodes.x), colour="blue") + geom_line(aes(y = num.of.nodes.y), colour = "red")
plot.comp
plot <- plot + xlab("alpha_0")+ylab("alpha_1") +theme(text= element_text(size=10))


#Network Measures
transitivity(graph.2007)
diameter(graph.2007,directed = FALSE)
average.path.length(graph.2007,directed = FALSE)

transitivity(graph.2014)
diameter(graph.2014,directed = FALSE)
average.path.length(graph.2014,directed = FALSE)

