#load relevant libraries
	library(ggplot2)
	
#import observed data
	observed <- read.csv("observed_NN.csv", header = T)
	
	#rename levels of size classes
	levels(observed$size_class)
	levels <- c("s5", "s2", "s3", "s4", "s1")
	levels(observed$size_class) <- levels

#import simulated data
	simulated <- read.csv("simulation_output.csv", stringsAsFactors = FALSE)
	
	#subset data
	s1 <- as.numeric(simulated[1,-(1:2)])
	s1 <- s1[!is.na(s1)]
	
	s2 <- as.numeric(simulated[2,-(1:2)])
	s2 <- s2[!is.na(s2)]
	
	s3 <- as.numeric(simulated[3,-(1:2)])
	s3 <- s3[!is.na(s3)]
	
	s4 <- as.numeric(simulated[4,-(1:2)])
	s4 <- s4[!is.na(s4)]
	
	s5 <- as.numeric(simulated[5,-(1:2)])
	s5 <- s5[!is.na(s5)]

#create a directory to store figures
dir.create("figures")

#change working directory to subfolder
setwd(paste(getwd(),"/figures",sep=""))

#plot data

	#0-4 cm plot
	observed_1 <- data.frame(x = observed[which(observed$size_class == "s1"),2], group = "observed")
	simulated_1 <- data.frame(x = s1, group = "simulated")
	combined_1 <- rbind(observed_1, simulated_1)
	means_1 <- aggregate(combined_1$x, by = list(combined_1$group), FUN = mean)
	
	ggplot(combined_1, aes(x, fill=group, colour = group)) +
		geom_density(alpha = 0.4) +
	  scale_fill_manual(values = c("gray80","gray20")) +
		geom_vline(data = means_1, aes(xintercept = x, color = Group.1)) +
	  scale_color_manual(values = c("gray80","gray20")) +
		scale_x_continuous(limits = c(0,100), expand = c(0,0)) +
		scale_y_continuous(limits = c(0,0.06), expand = c(0,0)) +
		xlab("Nearest size class neighbor (cm)") +
		ylab("Density") +
		theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
		      panel.background = element_blank(), axis.line = element_line(colour = "black"),
		      axis.text = element_text(colour="black"), text = element_text(size=12),
		      legend.position = "none", plot.margin=unit(c(0.3,0.4,0.3,0.3),"cm"))
	ggsave("plot_1.pdf", width = 3.19, height = 2.88, units = "in")
	
	#4-8 cm plot
	observed_2 <- data.frame(x = observed[which(observed$size_class == "s2"),2], group = "observed")
	simulated_2 <- data.frame(x = s2, group = "simulated")
	combined_2 <- rbind(observed_2, simulated_2)
	means_2 <- aggregate(combined_2$x, by = list(combined_2$group), FUN = mean)
	
	ggplot(combined_2, aes(x, fill=group, colour = group)) +
		geom_density(alpha = 0.4) +
	  scale_fill_manual(values = c("gray80","gray20")) +
		geom_vline(data = means_2, aes(xintercept = x, color = Group.1)) +
	  scale_color_manual(values = c("gray80","gray20")) +
		scale_x_continuous(limits = c(0,100), expand = c(0,0)) +
		scale_y_continuous(limits = c(0,0.1), expand = c(0,0)) +
		xlab("Nearest size class neighbor (cm)") +
		ylab("Density") +
	  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
	        panel.background = element_blank(), axis.line = element_line(colour = "black"),
	        axis.text = element_text(colour="black"), text = element_text(size=12),
	        legend.position = "none", plot.margin=unit(c(0.3,0.4,0.3,0.3),"cm"))
	ggsave("plot_2.pdf", width = 3.19, height = 2.88, units = "in")
	
	#8-12 cm plot
	observed_3 <- data.frame(x = observed[which(observed$size_class == "s3"),2], group = "observed")
	simulated_3 <- data.frame(x = s3, group = "simulated")
	combined_3 <- rbind(observed_3, simulated_3)
	means_3 <- aggregate(combined_3$x, by = list(combined_3$group), FUN = mean)
	
	ggplot(combined_3, aes(x, fill=group, colour = group)) +
		geom_density(alpha = 0.4) +
	  scale_fill_manual(values = c("gray80","gray20")) +
		geom_vline(data = means_3, aes(xintercept = x, color = Group.1)) +
	  scale_color_manual(values = c("gray80","gray20")) +
		scale_x_continuous(limits = c(0,100), expand = c(0,0)) +
		scale_y_continuous(limits = c(0,0.085), expand = c(0,0)) +
		xlab("Nearest size class neighbor (cm)") +
		ylab("Density") +
	  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
	        panel.background = element_blank(), axis.line = element_line(colour = "black"),
	        axis.text = element_text(colour="black"), text = element_text(size=12),
	        legend.position = "none", plot.margin=unit(c(0.3,0.4,0.3,0.3),"cm"))
	ggsave("plot_3.pdf", width = 3.19, height = 2.88, units = "in")
	
	#12-16 cm plot
	observed_4 <- data.frame(x = observed[which(observed$size_class == "s4"),2], group = "observed")
	simulated_4 <- data.frame(x = s4, group = "simulated")
	combined_4 <- rbind(observed_4, simulated_4)
	means_4 <- aggregate(combined_4$x, by = list(combined_4$group), FUN = mean)
	
	ggplot(combined_4, aes(x, fill=group, colour = group)) +
		geom_density(alpha = 0.4) +
	  scale_fill_manual(values = c("gray80","gray20")) +
		geom_vline(data = means_4, aes(xintercept = x, color = Group.1)) +
	  scale_color_manual(values = c("gray80","gray20")) +
		scale_x_continuous(limits = c(0,100), expand = c(0,0)) +
		scale_y_continuous(limits = c(0,0.1), expand = c(0,0)) +
		xlab("Nearest size class neighbor (cm)") +
		ylab("Density") +
	  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
	        panel.background = element_blank(), axis.line = element_line(colour = "black"),
	        axis.text = element_text(colour="black"), text = element_text(size=12),
	        legend.position = "none", plot.margin=unit(c(0.3,0.4,0.3,0.3),"cm"))
	ggsave("plot_4.pdf", width = 3.19, height = 2.88, units = "in")
	
	#16+ cm plot
	observed_5 <- data.frame(x = observed[which(observed$size_class == "s5"),2], group = "observed")
	simulated_5 <- data.frame(x = s5, group = "simulated")
	combined_5 <- rbind(observed_5, simulated_5)
	means_5 <- aggregate(combined_5$x, by = list(combined_5$group), FUN = mean)
	
	ggplot(combined_5, aes(x, fill=group, colour = group)) +
		geom_density(alpha = 0.4) +
	  scale_fill_manual(values = c("gray80","gray20")) +
		geom_vline(data = means_5, aes(xintercept = x, color = Group.1)) +
	  scale_color_manual(values = c("gray80","gray20")) +
		scale_x_continuous(limits = c(0,100), expand = c(0,0)) +
		scale_y_continuous(limits = c(0,0.13), expand = c(0,0)) +
		xlab("Nearest size class neighbor (cm)") +
		ylab("Density") +
	  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
	        panel.background = element_blank(), axis.line = element_line(colour = "black"),
	        axis.text = element_text(colour="black"), text = element_text(size=12),
	        legend.position = "none", plot.margin=unit(c(0.3,0.4,0.3,0.3),"cm"))
	ggsave("plot_5.pdf", width = 3.19, height = 2.88, units = "in")