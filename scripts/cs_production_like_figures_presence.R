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

# Make summary data for closure realization
pres_closure_figs = dur_figs %>%
  group_by(speaker, context_specific, context_start_lg) %>%
  summarise(perc_prod = mean(closure_presence) * 100) %>%
  ungroup()

# Make summary data for burst realization
pres_burst_figs = dur_figs %>%
  group_by(speaker, context_specific, context_start_lg) %>%
  summarise(perc_prod = mean(burst_presence) * 100) %>%
  ungroup()


## SET COLORS ####
colors = brewer.pal(5, "PRGn")
col_eng = colors[5]
col_sp = colors[1]
col_cses = colors[4]
col_csse = colors[2]


## MAKE PLOTS ####
# Boxplot of closure realization
presence_closure.plot = ggplot(pres_closure_figs, aes(x=context_start_lg, y=perc_prod)) +
  geom_boxplot(aes(fill = context_specific)) +
  #scale_fill_manual(values=c("white", "grey")) +
  scale_fill_manual(values=c(col_eng, col_cses, col_sp, col_csse)) +
  ggtitle("/k/ Closure Realization by\nContext and Start Language") +
  xlab("Context") +
  ylab("Percentage of time realized") +
  guides(fill=guide_legend(title="")) +
  theme_bw() +
  theme(text=element_text(size=18), title=element_text(size=18), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="top", legend.key=element_blank())

pdf("figures/closure_realization.pdf")
presence_closure.plot
dev.off()

# Boxplot of burst realization
presence_burst.plot = ggplot(pres_burst_figs, aes(x=context_start_lg, y=perc_prod)) +
  geom_boxplot(aes(fill = context_specific)) +
  #scale_fill_manual(values=c("white", "grey")) +
  scale_fill_manual(values=c(col_eng, col_cses, col_sp, col_csse)) +
  ggtitle("/k/ Burst Realization by\nContext and Start Language") +
  xlab("Context") +
  ylab("Percentage of time realized") +
  guides(fill=guide_legend(title="")) +
  theme_bw() +
  theme(text=element_text(size=18), title=element_text(size=18), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="top", legend.key=element_blank())

pdf("figures/burst_realization.pdf")
presence_burst.plot
dev.off()