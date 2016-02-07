## LOAD PACKAGES ####
library(lme4)
library(dplyr)


## READ IN DATA AND ORGANIZE ####
data = read.table("data/formants_segments.txt", header=T, sep="\t")

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
	
# Make factor for phoneme
data_sub$phoneme = factor(data_sub$phoneme)


## REMOVE OUTLIERS ####
# Compute F1, F2, and F3 means and standard deviations
formant_summ = data_sub %>%
	group_by(speaker, percentage, phoneme) %>%
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
data_noout = data_sub %>%
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
	
	
## COMPUTE VARAIBLES FOR TESTING ####
# [l]
data_l = filter(data_noout, phoneme == "l")
	data_l$phoneme = factor(data_l$phoneme)
	
data_l_summ = data_l %>%
	group_by(speaker, task, time_stamp, gram_cat, context_cat, context_start_lg, sex) %>%
	summarize(mean_f1_norm_bark = mean(f1_norm_bark, na.rm=T), mean_f2_norm_bark = mean(f2_norm_bark, na.rm=T)) %>%
	ungroup()
	
# [ai] - nucleous
data_ai_nuc = data_noout %>%
	filter(phoneme == "ai")	 %>%
	filter(percentage >=25, percentage <= 75)
data_ai_nuc$phoneme = factor(data_ai_nuc$phoneme)

data_ai_nuc_maxf1 = data_ai_nuc %>%
	group_by(speaker, task, time_stamp, context_cat, context_start_lg, sex) %>%
	summarize(f1_norm_bark = max(f1_norm_bark, na.rm=T)) %>%
	ungroup()
	
data_ai_nuc_summ = inner_join(data_ai_nuc_maxf1, data_ai_nuc) %>%
	group_by(speaker, task, time_stamp, context_cat, context_start_lg, sex, f1_norm_bark) %>%
	summarize(percentage = round(mean(percentage))) %>%
	ungroup() %>%
	inner_join(data_ai_nuc)

# [ai] - offglide
data_ai_off = data_noout %>%
	filter(phoneme == "ai")	 %>%
	filter(percentage > 75)
data_ai_off$phoneme = factor(data_ai_off$phoneme)

data_ai_off_maxf2 = data_ai_off %>%
	group_by(speaker, task, time_stamp, context_cat, context_start_lg, sex) %>%
	summarize(f2_norm_bark = max(f2_norm_bark, na.rm=T)) %>%
	ungroup()
	
data_ai_off_summ = inner_join(data_ai_off_maxf2, data_ai_off) %>%
	group_by(speaker, task, time_stamp, context_cat, context_start_lg, sex, f2_norm_bark) %>%
	summarize(percentage = round(mean(percentage))) %>%
	ungroup() %>%
	inner_join(data_ai_off)
	
	

	



