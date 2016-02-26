## READ IN DATA ####
source("scripts/cs_production_like_cleaning_formants.R")


## LOAD PACKAGES ####
library(ggplot2)
library(RColorBrewer)


## ORGANIZE DATA ####
formant_figs = formant_noout %>%
  mutate(context_specific = factor(context_specific, levels = c("E", "CS_ES", "S", "CS_SE"),
                                   labels=c("ML English", "CS English", "ML Spanish", "CS Spanish")))

# Make data with means for percentage
formant_sum_figs = formant_figs %>%
  group_by(context_specific, percentage) %>%
  summarise(f1_mean = mean(f1_norm_bark, na.rm=T), f2_mean = mean(f2_norm_bark, na.rm=T)) %>%
  ungroup()


## SET COLORS ####
cols = brewer.pal(5, "PRGn")
col_eng = cols[5]
col_sp = cols[1]
col_cses = cols[4]
col_csse = cols[2]


## MAKE PLOTS ####
# Raw formants plot
formants.plot = ggplot(formant_figs, aes(x=f2_norm_bark, y=f1_norm_bark, group=context_specific, linetype=context_specific)) +
	geom_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3), size = 1, color="black") +
	ggtitle("/la\u026A/ Formant Values by\nContext and Language Preceding Token") +
	xlab("F2 Bark Normalized") +
	ylab("F1 Bark Normalized") +
	scale_linetype_manual(values=c("solid", "dashed", "dotted", "dotdash")) +
	annotate("text", x = 7, y = 12.75, label = "0% into /la\u026A/") +
	annotate("text", x = 0.75, y = 11.9, label = "100% into /la\u026A/") +
	theme_bw() +
	theme(legend.title=element_blank(), text=element_text(size=18), title=element_text(size=18), legend.position=c(0.15, 0.85), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

cairo_pdf("figures/like_formants.pdf")
formants.plot
dev.off()
	
# Mean formants plot
formants_sum.plot = ggplot(formant_sum_figs, aes(x=f2_mean, y=f1_mean, group=context_specific, linetype=context_specific, color=context_specific)) +
	geom_smooth(size=1) +
	ggtitle("/la\u026A/ Formant Values by\nContext and Language Preceding Token") +
	xlab("F2 Bark Normalized") +
	ylab("F1 Bark Normalized") +
  scale_color_manual(values=c("black", "white", "black", "white")) +
	scale_linetype_manual(values=c(1, 1, 2, 2)) +
	annotate("text", x = 4.5, y = 10.9, label = "0% into /la\u026A/") +
	annotate("text", x = 1.5, y = 11.2, label = "100% into /la\u026A/") +
  guides(color = guide_legend(title = ""), linetype = guide_legend(title = "")) +
	theme_bw() +
  theme(text=element_text(size=18), title=element_text(size=18), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="top", legend.key=element_blank())

cairo_pdf("figures/like_formants_means.pdf")
formants_sum.plot
dev.off()
