## SET WORKING DIRECTORY
setwd("~/Desktop/Experiments/CS E-S Production - like/Results/data/")


## LOAD PACKAGES
library(lme4)
library(ggplot2)


## READ IN DATA AND CLEAN
data = read.table("duration.txt", header=T, sep="\t")

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
	
# Subset out data where the duration of the closure is 0
data_burst = subset(data_sub, burst_duration > 0)
	
	
## PREPARE VARAIBLES FOR LMER
# Context (baseline set to ML)
contrasts(data_burst$context_cat) = c(0.5, -0.5)
data_burst$context_catContrast = contrasts(data_burst$context_cat)[,1][as.numeric(data_burst$context_cat)]

# Language at start of 'like' (baseline set to English)
contrasts(data_burst$context_start_lg) = c(-0.5, 0.5)
data_burst$context_start_lgContrast = contrasts(data_burst$context_start_lg)[,1][as.numeric(data_burst$context_start_lg)]

# Task category (baseline set to c)
contrasts(data_burst$task) = c(-0.5, 0.5)
data_burst$taskContrast = contrasts(data_burst$task)[,1][as.numeric(data_burst$task)]

# Grammatical category (baseline set to D)
contrasts(data_burst$gram_cat) = c(-0.5, 0.5)
data_burst$gram_catContrast = contrasts(data_burst$gram_cat)[,1][as.numeric(data_burst$gram_cat)]


## RUN LMERS ON DURATION OF CLOSURE
# Full model
data_burst.lmer = lmer(burst_duration ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast + (1+context_catContrast+context_start_lgContrast+taskContrast+gram_catContrast|speaker), data=data_burst, REML=F)

summary(data_burst.lmer)

# Test for effect of context
data_burst_context.lmer = lmer(burst_duration ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - context_catContrast + (1+context_catContrast+context_start_lgContrast+taskContrast+gram_catContrast|speaker), data=data_burst, REML=F)

anova(data_burst.lmer, data_burst_context.lmer)

# Test for effect of starting language
data_burst_startlg.lmer = lmer(burst_duration ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - context_start_lgContrast + (1+context_catContrast+context_start_lgContrast+taskContrast+gram_catContrast|speaker), data=data_burst, REML=F)

anova(data_burst.lmer, data_burst_startlg.lmer)

# Test for effect of task
data_burst_task.lmer = lmer(burst_duration ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - taskContrast + (1+context_catContrast+context_start_lgContrast+taskContrast+gram_catContrast|speaker), data=data_burst, REML=F)

anova(data_burst.lmer, data_burst_task.lmer)

# Test for effect of grammatical category
data_burst_gramcat.lmer = lmer(burst_duration ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - gram_catContrast + (1+context_catContrast+context_start_lgContrast+taskContrast+gram_catContrast|speaker), data=data_burst, REML=F)

anova(data_burst.lmer, data_burst_gramcat.lmer)

# Test for interaction of context and starting language
data_burst_contextxstartlg.lmer = lmer(burst_duration ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - context_catContrast:context_start_lgContrast + (1+context_catContrast+context_start_lgContrast+taskContrast+gram_catContrast|speaker), data=data_burst, REML=F)

anova(data_burst.lmer, data_burst_contextxstartlg.lmer)

# Test for interaction of context and task
data_burst_contextxtask.lmer = lmer(burst_duration ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - context_catContrast:taskContrast + (1+context_catContrast+context_start_lgContrast+taskContrast+gram_catContrast|speaker), data=data_burst, REML=F)

anova(data_burst.lmer, data_burst_contextxtask.lmer)

# Test for interaction of starting language and task
data_burst_startlgxtask.lmer = lmer(burst_duration ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - context_start_lgContrast:taskContrast + (1+context_catContrast+context_start_lgContrast+taskContrast+gram_catContrast|speaker), data=data_burst, REML=F)

anova(data_burst.lmer, data_burst_startlgxtask.lmer)

# Test for interaction of context, starting language, and task
data_burst_contextxstartlgxtask.lmer = lmer(burst_duration ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - context_catContrast:context_start_lgContrast:taskContrast + (1+context_catContrast+context_start_lgContrast+taskContrast+gram_catContrast|speaker), data=data_burst, REML=F)

anova(data_burst.lmer, data_burst_contextxstartlgxtask.lmer)

# Follow-up regressions for interaction
data_burst_ml = subset(data_burst, context_cat=="ML")
	data_burst_ml$context_cat = factor(data_burst_ml$context_cat)
data_burst_cs = subset(data_burst, context_cat=="CS")
	data_burst_cs$context_cat = factor(data_burst_cs$context_cat)
summary(lm(burst_duration ~ context_start_lg, data_burst_ml))
summary(lm(burst_duration ~ context_start_lg, data_burst_cs))

data_burst_e = subset(data_burst, context_start_lg=="E")
	data_burst_e$context_start_lg = factor(data_burst_e$context_start_lg)
data_burst_s = subset(data_burst, context_start_lg=="S")
	data_burst_s$context_start_lg = factor(data_burst_s$context_start_lg)
summary(lm(burst_duration ~ context_cat, data_burst_e))
summary(lm(burst_duration ~ context_cat, data_burst_s))











