## SET WORKING DIRECTORY
setwd("~/Desktop/Experiments/CS E-S Production - like/Results/data/")


## LOAD REQUIRED PACKAGES
library(lme4)
library(dplyr)


## READ IN DATA AND ORGANIZE
data = read.table("formants.txt", header=T, sep="\t")

# Add column for gender
data = data %>%
	mutate(sex = ifelse(speaker == "BESM_07" | speaker == "BESM_08", "M", "F"))
data$sex = factor(data$sex)

# Get rid of 'verb' and 'grammatical' tokens
data = subset(data, gram_cat!="V" & gram_cat!="G")
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


## REMOVE OUTLIERS
# Compute F1, F2, and F3 means means and standard deviations
formant_summ = data_sub %>%
	group_by(speaker, percentage) %>%
	summarize(mean_f1=mean(f1, na.rm=TRUE),
				mean_f2=mean(f2, na.rm=TRUE),
				mean_f3=mean(f3, na.rm=TRUE),
				sd_f1=sd(f1, na.rm=TRUE),
				sd_f2=sd(f2, na.rm=TRUE),
				sd_f3=sd(f3, na.rm=TRUE)) %>%
	ungroup() %>%
	mutate(high_f1 = mean_f1 + 2*sd_f1) %>%
	mutate(low_f1 = mean_f1 - 2*sd_f1) %>%
	mutate(high_f2 = mean_f2 + 2*sd_f2) %>%
	mutate(low_f2 = mean_f2 - 2*sd_f2) %>%
	mutate(high_f3 = mean_f3 + 2*sd_f3) %>%
	mutate(low_f3 = mean_f3 - 2*sd_f3)
	
# Remove outliers for a given participant (does not remove lines, turns into NA)
data_noout = data %>%
	inner_join(formant_summ) %>%
	mutate(f1 = ifelse(f1 > high_f1, NA, f1)) %>%
	mutate(f1 = ifelse(f1 < low_f1, NA, f1)) %>%
	mutate(f2 = ifelse(f2 > high_f2, NA, f2)) %>%
	mutate(f2 = ifelse(f2 < low_f2, NA, f2)) %>%
	mutate(f3 = ifelse(f3 > high_f3, NA, f3)) %>%
	mutate(f3 = ifelse(f3 < low_f3, NA, f3))	
	
# Normalize F1 and F2 based on F3
data_noout$f1_bark = 26.81 / (1+1960/data_noout$f1) - 0.53
data_noout$f2_bark = 26.81 / (1+1960/data_noout$f2) - 0.53
data_noout$f3_bark = 26.81 / (1+1960/data_noout$f3) - 0.53

data_noout$f1_norm_bark = data_noout$f3_bark - data_noout$f1_bark
data_noout$f2_norm_bark = data_noout$f3_bark - data_noout$f2_bark

# Remove final outlier
data_noout = data_noout %>%
	mutate(f1_norm_bark = ifelse(f1_norm_bark < 6, NA, f1_norm_bark))

# Change format of data	
f1_values = data_noout %>%
	select(speaker, sex, task, time_stamp, context_cat, context_start_lg, context_specific, duration_ms, percentage, f1_norm_bark) %>%
	spread(percentage, f1_norm_bark) %>%
	select(speaker, sex, task, time_stamp, context_cat, context_start_lg, context_specific, duration_ms, f1_0=`0`, f1_5=`5`, f1_10=`10`, f1_15=`15`, f1_20=`20`, f1_25=`25`, f1_30=`30`, f1_35=`35`, f1_40=`40`, f1_45=`45`, f1_50=`50`, f1_55=`55`, f1_60=`60`, f1_65=`65`, f1_70=`70`, f1_75=`75`, f1_80=`80`, f1_85=`85`, f1_90=`90`, f1_95=`95`, f1_100=`100`)
	
f2_values = data_noout %>%
	select(speaker, sex, task, time_stamp, context_cat, context_start_lg, context_specific, duration_ms, percentage, f2_norm_bark) %>%
	spread(percentage, f2_norm_bark) %>%
	select(speaker, sex, task, time_stamp, context_cat, context_start_lg, context_specific, duration_ms, f2_0=`0`, f2_5=`5`, f2_10=`10`, f2_15=`15`, f2_20=`20`, f2_25=`25`, f2_30=`30`, f2_35=`35`, f2_40=`40`, f2_45=`45`, f2_50=`50`, f2_55=`55`, f2_60=`60`, f2_65=`65`, f2_70=`70`, f2_75=`75`, f2_80=`80`, f2_85=`85`, f2_90=`90`, f2_95=`95`, f2_100=`100`)

data_morph = f1_values %>%
	inner_join(f2_values)


