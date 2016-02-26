## LOAD PACKAGES ####
library(dplyr)


## READ IN DATA ####
dur = read.table("data/duration.txt", header=T, sep="\t")


## CLEAN DATA ####
dur_clean = dur %>%
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