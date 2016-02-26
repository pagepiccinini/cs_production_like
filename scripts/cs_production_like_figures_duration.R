## READ IN DATA ####
source("scripts/cs_production_like_cleaning_duration.R")


## LOAD PACKAGES ####
library(ggplot2)
library(RColorBrewer)


## ORGANIZE DATA ####
dur_figs = dur_clean %>%
  mutate(context_specific = factor(context_specific, levels = c("E", "CS_ES", "S", "CS_SE"),
                                   labels = c("Eng. ML", "Eng. CS", "Sp. ML", "Sp. CS"))) %>%
  mutate(context_start_lg = factor(context_start_lg, levels = c("E", "S"), labels = c("English", "Spanish")))
	
# Make data for closure duration
dur_clos_figs = dur_figs %>%
  filter(closure_duration > 0)

# Make data for burst duration
dur_burst_figs = dur_figs %>%
  filter(burst_duration > 0)


## SET COLORS ####
colors = brewer.pal(5, "PRGn")
col_eng = colors[5]
col_sp = colors[1]
col_cses = colors[4]
col_csse = colors[2]


## MAKE PLOTS ####
# Boxplot of like duration
duration_lai.plot = ggplot(dur_figs, aes(x=context_start_lg, y=like_duration_ms)) +
					geom_boxplot(aes(fill=context_specific)) +
					#scale_fill_manual(values=c("white", "grey")) +
          scale_fill_manual(values=c(col_eng, col_cses, col_sp, col_csse)) +
					ggtitle("/la\u026A/ Duration by\nContext and Start Language") +
					xlab("Context") +
					ylab("Duration in ms") +
					guides(fill=guide_legend(title="")) +
					theme_bw() +
					theme(text=element_text(size=18), title=element_text(size=18), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
					      legend.position="top", legend.key=element_blank())

cairo_pdf("figures/like_duration.pdf")
duration_lai.plot
dev.off()

# Boxplot of closure duration
duration_closure.plot = ggplot(dur_clos_figs, aes(x=context_start_lg, y=closure_duration)) +
					geom_boxplot(aes(fill=context_specific)) +
					#scale_fill_manual(values=c("white", "grey")) +
          scale_fill_manual(values=c(col_eng, col_cses, col_sp, col_csse)) +
					ggtitle("/k/ Closure Duration by\nContext and Start Language") +
					xlab("Context") +
					ylab("Duration in ms") +
          guides(fill=guide_legend(title="")) +
					theme_bw() +
          theme(text=element_text(size=18), title=element_text(size=18), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
            legend.position="top", legend.key=element_blank())

pdf("figures/closure_duration.pdf")
duration_closure.plot
dev.off()
	
# Boxplot of burst duration
duration_burst.plot = ggplot(dur_burst_figs, aes(x=context_start_lg, y=burst_duration)) +
					geom_boxplot(aes(fill=context_specific)) +
					#scale_fill_manual(values=c("white", "grey")) +
          scale_fill_manual(values=c(col_eng, col_cses, col_sp, col_csse)) +
					ggtitle("/k/ Burst Duration by\nContext and Start Language") +
					xlab("Context") +
					ylab("Duration in ms") +
          guides(fill=guide_legend(title="")) +
					theme_bw() +
          theme(text=element_text(size=18), title=element_text(size=18), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
              legend.position="top", legend.key=element_blank())

pdf("figures/burst_duration.pdf")
duration_burst.plot
dev.off()
