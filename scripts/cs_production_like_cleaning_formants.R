## LOAD PACKAGES ####
library(dplyr)


## READ IN DATA ####
formant = read.table("data/formants.txt", header=T, sep="\t")


## CLEAN DATA ####
formant_clean = formant %>%
  # Add column for gender
	mutate(sex = ifelse(speaker == "BESM_07" | speaker == "BESM_08", "M", "F")) %>%
  mutate(sex = factor(sex)) %>%
  # Get rid of 'verb' and 'grammatical' tokens
  filter(gram_cat != "V" & gram_cat != "G") %>%
	mutate(gram_cat = factor(gram_cat)) %>%
  # Get rid of 'CS3' tokens
  filter(context_cat != "CS3") %>%
	mutate(context_cat = factor(context_cat)) %>%
  # Subset out data with main code-switching tokens
  filter(context_specific=="E" | context_specific=="S" |
           context_specific=="CS_ES" | context_specific=="CS_SE") %>%
  mutate(context_specific = factor(context_specific)) %>%
  # Clean up other variables
  mutate(context_start_lg = factor(context_start_lg)) %>%
  mutate(context_cat = factor(context_cat))
  
  
## REMOVE OUTLIERS ####
# Compute F1, F2, and F3 means means and standard deviations
formant_sum = formant_clean %>%
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
formant_noout = formant_clean %>%
	inner_join(formant_sum) %>%
	mutate(f1 = ifelse(f1 > high_f1, NA, f1)) %>%
	mutate(f1 = ifelse(f1 < low_f1, NA, f1)) %>%
	mutate(f2 = ifelse(f2 > high_f2, NA, f2)) %>%
	mutate(f2 = ifelse(f2 < low_f2, NA, f2)) %>%
	mutate(f3 = ifelse(f3 > high_f3, NA, f3)) %>%
	mutate(f3 = ifelse(f3 < low_f3, NA, f3)) %>%
  # Bark transform F1, F2, F3
  mutate(f1_bark = 26.81 / (1+1960/f1) - 0.53) %>%
  mutate(f2_bark = 26.81 / (1+1960/f2) - 0.53) %>%
  mutate(f3_bark = 26.81 / (1+1960/f3) - 0.53) %>%
  # Normalize F1 and F2 based on F3
  mutate(f1_norm_bark = f3_bark - f1_bark) %>%
  mutate(f2_norm_bark = f3_bark - f2_bark) %>%
  # Remove final outlier
	mutate(f1_norm_bark = ifelse(f1_norm_bark < 6, NA, f1_norm_bark))

	



