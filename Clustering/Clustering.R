install.packages("dendextend")
library(dendextend)
library(ggplot2)

setwd("C:\\Sudhanshu\\SU\\Semester 2\\707\\Assignment 4")
fedPaperDS <- read.csv("fedPapers85.csv")
View(fedPaperDS)
str(fedPaperDS)

fedPapersCleanedDS <- fedPaperDS[,c(3:72)]

set.seed(100)
modelKMeans <- kmeans(fedPapersCleanedDS,4, nstart = 20)
modelKMeans

table(modelKMeans$cluster, fedPaperDS$author)

modelKMeans$cluster <- as.factor(modelKMeans$cluster)
ggplot(fedPapersCleanedDS, aes(fedPaperDS$author, fedPaperDS$filename, color = modelKMeans$cluster)) + geom_point()

#Building HAC Model
distance = dist(as.matrix(fedPapersCleanedDS))
hacModel <- as.dendrogram(hclust(distance, method = "ward.D2"))
labels(hacModel) <- fedPaperDS[,1][order.dendrogram(hacModel)]
plot(hacModel, main="Dendrogram using HAC algorithm",xlab = "", ylab = "Euclidean Distance")