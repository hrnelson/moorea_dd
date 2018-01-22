#load relevant libraries
library(plotrix)

#set seed for reproducibility
set.seed(4)

#load data
data <- read.csv("observed_sizes.csv", header = T)

#create a directory to store simulation visualizations
dir.create("simulation_visualizations")

#change working directory to subfolder
setwd(paste(getwd(),"/simulation_visualizations",sep=""))

#calculate average of two diameters
data$mean_diameter <- rowMeans(subset(data, select = c(diameter1, diameter2)))

#make df to store output
size <- c("0 to 4", "4 to 8", "8 to 12", "12 to 16", "16+")
output <- as.data.frame(size)

#repeat simulation 500 times
for (a in 1:500) {
	
	#randomly select 9 quadrats (out of 16) with replacement
	numbers <- sample(1:16,9, replace = TRUE)
	
	#combine coral sizes from selected quadrats into a single vector
	sizes <- subset(data, quadrat == numbers[1], select= c(quadrat, mean_diameter))
	for (b in 2:length(numbers)) {
		add <- subset(data, quadrat == numbers[b], select= c(quadrat, mean_diameter))
		sizes <- rbind (sizes, add)
	}
	
	#make data frame to store sizes and coordinates of circles
	df <- as.data.frame(list(sizes$mean_diameter, NA, NA))
	names(df) <- c("size", "x", "y")
	
	#randomize circle order
	df <- df[sample(nrow(df)),]

	#randomly assign coordinates of first circle
	df[1,2] <- runif(1,-100,200) #x-coordinate
	df[1,3] <- runif(1,-100,200) #y-coordinate
	
	##generate coordinates for rest of circles
	
	#loop through remaining circles
	for (n in 2:nrow(df)) {
		
		#get radius of circle
		r1 <- df[n,1]/2 #radius = diameter/2
		
		overlap <- TRUE #starting condition
		
		#generate coordinates until not overlapping
		while (overlap == TRUE) {
			
			#randomly generate coordinates
			x1 <- runif(1,-100,200) 
			y1 <- runif(1,-100,200) 
			
			##test for overlap
			
			#set overlap as false
			overlap <- FALSE
			
			#loop through all circles and test for overlap
			for (i in 1:nrow(df)) {
				
				#if circle has coordinates already (not NA) then test for overlap
				if (is.na(df[i,2]) == FALSE) {
					
					#calculate distance (d) between centers
					x2 <- df[i,2]
					y2 <- df[i,3]
					d <- sqrt((x2-x1)^2 + (y2-y1)^2)
					
					#get radius of other circle
					r2 <- df[i,1]/2
					
					#if d < sum of radii then two circles are overlapping
					if (d < (r1 + r2)) {
						overlap <- TRUE
					}
				}
			}
		}
		
		#save coordinates of circle in data frame
		df[n,2] <- x1
		df[n,3] <- y1
	}
	
	#plot circles in 2D space and save plot
	pdf(file = paste(a,".pdf", sep = ""))
	plot(1, asp = 1, type = "n", xlab = "x", ylab = "y", xlim = c(-100,200), ylim = c(-100,200))
	for (j in 1:nrow(df)) {
		draw.circle(df[j,2], df[j,3], df[j,1]/2, border = "red")
	}
	rect(0,0,100,100,border="blue") #focal quadrat
	dev.off()
	
	##calculate nearest neighbor (NN) distance by size class
	
	#make new column to store size class and NN distance
	df$size_class <- NA
	df$NN_dist <- NA
	
	#assign circles size class
	for (k in 1:nrow(df)) {
		if (df[k,1] <= 4) {
			df[k,4] <- "0 to 4"
		} else if (df[k,1] <= 8) {
			df[k,4] <- "4 to 8"
		} else if (df[k,1] <= 12) {
			df[k,4] <- "8 to 12"
		} else if (df[k,1] <= 16) {
			df[k,4] <- "12 to 16"
		} else {
			df[k,4] <- "16+"
		}
	}
	
	#subset focal circles in center square (3 x 3 grid)
	focal <- df[df$x > 0 & df$x <= 100, ]
	focal <- focal[focal$y > 0 & focal$y <= 100, ]
	
	#calculate NN distance for focal circles based on size class
	for (l in 1:nrow(focal)) {
		
		#store coordinates of point
		x1 <- focal[l,2]
		y1 <- focal[l,3]
		
		#get subset of circles of same size class
		size <- focal[l,4]
		df2 <- df[df$size_class == size,]
		
		#calculate minimum distance to all other points in size class
		old_dist <- 1000000 #initialize with impossibly big distance
		for (m in 1:nrow(df2)) {
			if (focal[l,2] != df2[m,2]) { #ignore same point (NN = 0)
				x2 <- df2[m,2]
				y2 <- df2[m,3]
				new_dist <- sqrt((x2-x1)^2 + (y2-y1)^2)
				
				if (new_dist < old_dist) {
					old_dist <- new_dist
				}
			}
			
			focal[l,5] <- old_dist
		}
	}
	
	#average NN distance by size class and add to output
	summary <- aggregate(focal$NN_dist,list(focal$size_class), mean)
	
	#add missing categories
	if (!("0 to 4" %in% summary$Group.1)) {
		add <- data.frame(Group.1="0 to 4",x=NA)
		summary <- rbind(summary,add)
	}
	if (!("4 to 8" %in% summary$Group.1)) {
		add <- data.frame(Group.1="4 to 8",x=NA)
		summary <- rbind(summary,add)
	}
	if (!("8 to 12" %in% summary$Group.1)) {
		add <- data.frame(Group.1="8 to 12",x=NA)
		summary <- rbind(summary,add)
	}
	if (!("12 to 16" %in% summary$Group.1)) {
		add <- data.frame(Group.1="12 to 16",x=NA)
		summary <- rbind(summary,add)
	}	
	if (!("16+" %in% summary$Group.1)) {
		add <- data.frame(Group.1="16+",x=NA)
		summary <- rbind(summary,add)
	}
	
	output <- cbind(output, trial = summary[,2])	
}

##save output as csv

#change working directory to parent folder
setwd("../")

#save data
write.csv(output, file = "simulation_output.csv")
