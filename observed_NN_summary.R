#load data
data <- read.csv("observed_NN.csv", header = T)

#load relevant libraries
library(ggplot2)

#rename size categories and order correctly
levels(data$size_class)
levels <- c("s5", "s2", "s3", "s4", "s1")
levels(data$size_class) <- levels
data$size_class <- factor(data$size_class,levels(data$size_class)[c(5,2,3,4,1)])

#summarize (mean, sd) nearest neighbor measurements by size class
aggregate(data$NN, list(data$size_class), mean)
aggregate(data$NN, list(data$size_class), sd)

round(aggregate(data$NN, list(data$size_class), mean)$x, digits = 1)
round(aggregate(data$NN, list(data$size_class), sd)$x/sqrt(100), digits = 1)

#n for each size class
aggregate(data$NN, list(data$size_class), length)


