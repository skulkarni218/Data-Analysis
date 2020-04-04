library(dplyr)
library(tree)

setwd("C:\\Sudhanshu\\SU\\Semester 2\\707\\Assignment 5")
fedPapersDS <- read.csv("fedPapers85.csv")
View(fedPapersDS)
str(fedPapersDS)

fedPapersDS$AuthorCode <- dplyr::recode(fedPapersDS$author, dispt = 1, Hamilton = 2, HM = 3, Jay = 4, Madison = 5)
fedPapersDS <- fedPapersDS[,-2]

fedTrainingData <- subset(fedPapersDS, AuthorCode == 2 | AuthorCode == 3 | AuthorCode == 4 | AuthorCode == 5)
str(fedTrainingData)

fedTestData <- subset(fedPapersDS, AuthorCode == 1)
str(fedTestData)

tree.fedPapers = tree(AuthorCode~.-author, data=fedTrainingData)
summary(tree.fedPapers)
plot(tree.fedPapers)
text(tree.fedPapers, splits = TRUE, all = FALSE ,pretty = 0)
tree.fedPapers

tree.pred = predict(tree.fedPapers, fedTestData)
summary(tree.pred)
plot(tree.pred)
with(fedTestData, table(tree.pred, AuthorCode))