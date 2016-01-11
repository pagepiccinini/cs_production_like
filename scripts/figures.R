## LOAD PACKAGES
library(lme4)
library(ggplot2)
library(RColorBrewer)


## SET COLORS
colors = brewer.pal(5, "PRGn")
eng_col = colors[5]
sp_col = colors[1]


## READ IN DATA AND CLEAN
data = read.table("data/duration.txt", header=T, sep="\t")

# Get rid of 'verb' tokens
data = subset(data, gram_cat!="V")
	data$gram_cat = factor(data$gram_cat)
	
# Get rid of 'CS3' tokens
data = subset(data, context_cat!="CS3")
	data$context_cat = factor(data$context_cat)
	data$context_start_lg = factor(data$context_start_lg)
	data$context_specific = factor(data$context_specific)
	
# Subset out data with main code-switching tokens
data_sub = subset(data, context_specific=="E" | context_specific=="S" | context_specific=="CS_ES" | context_specific=="CS_SE")
	data_sub$context_specific = factor(data_sub$context_specific)
	data_sub$context_cat = factor(data_sub$context_cat)
	data_sub$context_start_lg = factor(data_sub$context_start_lg)
	
# Reorder and rename variables
data_sub$context_cat = factor(data_sub$context_cat, levels=c("ML", "CS"), labels=c("monolingual", "code-switching"))
data_sub$context_start_lg = factor(data_sub$context_start_lg, levels=c("E", "S"), labels=c("English", "Spanish"))

# Organize data for boxplot of burst production
data_burst = aggregate(data_sub$burst_presence, by=list(data_sub$context_cat, data_sub$context_start_lg), FUN=mean)
	colnames(data_burst) = c("context_cat", "context_start_lg", "burst_percentage")
	

## MAKE PLOTS
# Boxplot of like duration
duration_plot <- ggplot(data_sub, aes(x=context_cat, y=like_duration_ms)) +
					geom_boxplot(aes(fill=context_start_lg)) +
					scale_fill_manual(values=c(eng_col, sp_col)) +
					#scale_fill_manual(values=c("white", "grey")) +
					ggtitle("/la\u026A/ Duration by\nContext and Start Language") +
					xlab("Context") +
					ylab("Duration in ms") +
					guides(fill=guide_legend(title="Language preceeding \'like\'")) +
					theme_bw() +
					theme(text=element_text(size=18), title=element_text(size=18), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
					      legend.position="top", legend.key=element_blank())

cairo_pdf("figures/like_duration.pdf")
duration_plot
dev.off()
	
# Boxplot of closure duration
data_clos = subset(data_sub, closure_duration > 0)
closure_duration_plot <- ggplot(data_clos, aes(x=context_cat, y=closure_duration)) +
					geom_boxplot(aes(fill=context_start_lg)) +
					scale_fill_manual(values=c("white", "grey")) +
					ggtitle("'like' Closure Duration by\nContext and Start Language") +
					xlab("Context") +
					ylab("Duration in ms") +
					guides(fill=guide_legend(title="")) +
					theme_bw() +
					theme(text=element_text(size=18), title=element_text(size=18), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position=c(0.15, 0.9))
	#pdf("../figures/closure_duration.pdf")
closure_duration_plot
	#dev.off()
	
# Boxplot of burst duration
data_burst = subset(data_sub, burst_duration > 0)
burst_duration_plot <- ggplot(data_burst, aes(x=context_cat, y=burst_duration)) +
					geom_boxplot(aes(fill=context_start_lg)) +
					scale_fill_manual(values=c("white", "grey")) +
					ggtitle("'like' Burst Duration by\nContext and Start Language") +
					xlab("Context") +
					ylab("Duration in ms") +
					guides(fill=guide_legend(title="")) +
					theme_bw() +
					theme(text=element_text(size=18), title=element_text(size=18), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position=c(0.15, 0.9))
	#pdf("../figures/burst_duration.pdf")
burst_duration_plot
	#dev.off()
