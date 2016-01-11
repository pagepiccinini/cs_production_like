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
f1_data = subset(data_sub, select=-c(f2, f3))
f2_data = subset(data_sub, select=-c(f1, f3))
f3_data = subset(data_sub, select=-c(f1, f2))

f1_noout = f1_data %>%
	inner_join(formant_summ) %>%
	filter(f1 < high_f1) %>%
	filter(f1 > low_f1)
	
f1_na = f1_data %>%
	inner_join(formant_sum) %>%
	mutate(f1=ifelse(f1>high_f1, NA, f1)) %>%
	mutate(f1=ifelse(f1<low_f1, NA, f1))
	
f2_noout = f2_data %>%
	inner_join(formant_summ) %>%
	filter(f2 < high_f2) %>%
	filter(f2 > low_f2)
	
f3_noout = f3_data %>%
	inner_join(formant_summ) %>%
	filter(f3 < high_f3) %>%
	filter(f3 > low_f3)
	
	



