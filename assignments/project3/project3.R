## ----setup, include=FALSE-----------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## -----------------------------------------------------------------------
# Loading the CO2 data
co2_data<-CO2
str(co2_data)


## -----------------------------------------------------------------------
variables<-c('Plant','Type','Treatment')
for (var in variables){
barplot(table(co2_data[var]),main = paste("Bar Graph of ",var),
        xlab = var, ylab = "Frequency")  
}



## -----------------------------------------------------------------------
# Histogram of conc variable
hist(co2_data$conc,main = "Histogram of conc",xlab = "conc",ylab = "Frequency")


## -----------------------------------------------------------------------
# Histogram of uptake variable
hist(co2_data$uptake,main = "Histogram of uptake",xlab = "conc",ylab = "Frequency")


## -----------------------------------------------------------------------
plot(co2_data$conc,co2_data$uptake,main = "Scatter Plot of conc and uptake",
     xlab = "conc",ylab = "uptake")


## -----------------------------------------------------------------------
cor(co2_data$conc,co2_data$uptake,method  = c("spearman"))


## -----------------------------------------------------------------------
library(ggplot2)


## -----------------------------------------------------------------------
variables<-c('Plant','Type','Treatment')
ggplot(data = co2_data) + geom_bar(mapping = aes(x = Plant))+ggtitle("Bar Graph of Plant")
ggplot(data = co2_data) + geom_bar(mapping = aes(x = Type))+ggtitle("Bar Graph of Type")
ggplot(data = co2_data) + geom_bar(mapping = aes(x = Treatment))+ggtitle("Bar Graph of Treatment")


## -----------------------------------------------------------------------
ggplot(data = co2_data)+geom_histogram(mapping = aes(x=conc))+ggtitle("Histogram of conc")
ggplot(data = co2_data)+geom_histogram(mapping = aes(x=uptake))+ggtitle("Histogram of uptake")


## -----------------------------------------------------------------------
ggplot(data=co2_data)+geom_point(mapping = aes(x=conc,y=uptake))+ggtitle('Scatter Plot of conc and uptake')


## -----------------------------------------------------------------------
file_path = 'data/termDocMatrix.rdata'
term_matrix_data<-load(file = file_path)


## -----------------------------------------------------------------------
library(tm)
term_matrix_data<-as.DocumentTermMatrix(termDocMatrix,weighting=weightBin)
term_matrix_data<-as.matrix(term_matrix_data)


## -----------------------------------------------------------------------
freq <- sort(rowSums(term_matrix_data), decreasing=T)
freq


## -----------------------------------------------------------------------
barplot(freq,main = "Frequency of Each Term",xlab = "Term",ylab = "Frequency")

hist(freq,main = "Histogram of Frequency",xlab = "Term Frequency")


## -----------------------------------------------------------------------
freq_1<-subset(freq,freq>=5)
barplot(freq_1,main = "Frequency of Each Term with Freq>=5",xlab = "Term",ylab = "Frequency")

hist(freq-1,main = "Histogram of Frequency",xlab = "Term Frequency")


## -----------------------------------------------------------------------
library(wordcloud)
freq <- sort(rowSums(term_matrix_data), decreasing=T)
wordcloud(words=names(freq), freq=freq, min.freq=5,
random.order=F)


## -----------------------------------------------------------------------
library(igraph)
#Transform Data into an Adjacency Matrix
termDocMatrix[termDocMatrix>=1] <- 1
# Transformation into term-term adjacency matrix
termMatrix <- termDocMatrix %*% t(termDocMatrix)
# Checking few terms in the adjacency matrix
termMatrix[1:5,1:5]
# Creating a undirected graph
g <- graph.adjacency(termMatrix, weighted=T, mode = "undirected")
# Removing the loop in same term 
g<-simplify(g)
plot(g)

