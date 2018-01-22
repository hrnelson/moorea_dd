#load relevant libraries
	library(ggplot2)
	library(tidyr)
	
#import simulation output
data <- read.csv("simulation_output.csv", stringsAsFactors = FALSE)

#calculate means for each size class, using simulated data

	round(rowMeans(data[,-(1:2)], na.rm = TRUE), digits = 1)

#calculate 5th and 95th percentiles for each size class, using simulated data

	#delete everything but data columns
	data <- data[,-1]
	data2 <- data[,-1]

	#size class 0-4 cm
		#subset data
		subset1 <- as.numeric(data2[1,])
		#calculate percentiles
		quantile(subset1, c(0.05, 0.95), na.rm = TRUE)
	
	#size class 4-8 cm
		#subset data
		subset2 <- as.numeric(data2[2,])
		#calculate percentiles
		quantile(subset2, c(0.05, 0.95), na.rm = TRUE)
	
	#size class 8-12 cm
		#subset data
		subset3 <- as.numeric(data2[3,])
		#calculate percentiles
		quantile(subset3, c(0.05, 0.95), na.rm = TRUE)

	#size class 12-16 cm
		#subset data
		subset4 <- as.numeric(data2[4,])
		#calculate percentiles
		quantile(subset4, c(0.05, 0.95), na.rm = TRUE)
	
	#size class 16+ cm
		#subset data
		subset5 <- as.numeric(data2[5,])
		#calculate percentiles
		quantile(subset5, c(0.05, 0.95), na.rm = TRUE)

#calculate % of replications with simulated average distance >= observed average distance

	#calculate observed averages
	
		#load data
		observed <- read.csv("observed_NN.csv", header = T) #load data
		
		#rename size categories and order correctly
		levels(observed$size_class)
		levels <- c("s5", "s2", "s3", "s4", "s1")
		levels(observed$size_class) <- levels
		observed$size_class <- factor(observed$size_class,levels(observed$size_class)[c(5,2,3,4,1)])

		#mean nearest neighbor measurements by size class
		observed_average <- aggregate(observed$NN, list(observed$size_class), mean)$x
	
	#size class 0-4 cm
		sum(subset1>observed_average[1], na.rm = TRUE)/sum(!is.na(subset1))*100
	#size class 4-8 cm
		sum(subset2>observed_average[2], na.rm = TRUE)/sum(!is.na(subset2))*100
	#size class 8-12 cm
		sum(subset3>observed_average[3], na.rm = TRUE)/sum(!is.na(subset3))*100
	#size class 12-16 cm
		sum(subset4>observed_average[4], na.rm = TRUE)/sum(!is.na(subset4))*100
	#size class 16+ cm
		sum(subset5>observed_average[5], na.rm = TRUE)/sum(!is.na(subset5))*100