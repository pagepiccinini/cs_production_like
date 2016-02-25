## LOAD PACKAGES ####
library(lme4)


## READ IN DATA ####
source("scripts/cs_production_like_cleaning_duration.R")

data_burst = data_sub	
	
## PREPARE VARAIBLES FOR glmer ####
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


## RUN GLMERS ON PRESENCE OF BURST
# Full model
data_burst.glmer = glmer(burst_presence ~ context_catContrast * context_start_lgContrast + taskContrast + gram_catContrast +
                           (1+context_catContrast+taskContrast|speaker), data=data_burst, family="binomial")

summary(data_burst.glmer)

# Test for effect of context
data_burst_context.glmer = glmer(burst_presence ~ context_catContrast * context_start_lgContrast + taskContrast + gram_catContrast - context_catContrast + (1+ context_catContrast+taskContrast|speaker), data=data_burst, family="binomial")

anova(data_burst.glmer, data_burst_context.glmer)

# Test for effect of starting language
data_burst_startlg.glmer = glmer(burst_presence ~ context_catContrast * context_start_lgContrast + taskContrast + gram_catContrast - context_start_lgContrast + (1+context_catContrast+taskContrast|speaker), data=data_burst, family="binomial")

anova(data_burst.glmer, data_burst_startlg.glmer)

# Test for effect of task
data_burst_task.glmer = glmer(burst_presence ~ context_catContrast * context_start_lgContrast + taskContrast + gram_catContrast - taskContrast + (1+context_catContrast+taskContrast|speaker), data=data_burst, family="binomial")

anova(data_burst.glmer, data_burst_task.glmer)

# Test for effect of grammatical category
data_burst_gramcat.glmer = glmer(burst_presence ~ context_catContrast * context_start_lgContrast + taskContrast + gram_catContrast - gram_catContrast + (1+context_catContrast+taskContrast|speaker), data=data_burst, family="binomial")

anova(data_burst.glmer, data_burst_gramcat.glmer)

# Test for interaction of context and starting language
data_burst_contextxstartlg.glmer = glmer(burst_presence ~ context_catContrast * context_start_lgContrast + taskContrast + gram_catContrast - context_catContrast:context_start_lgContrast + (1+context_catContrast+taskContrast|speaker), data=data_burst, family="binomial")

anova(data_burst.glmer, data_burst_contextxstartlg.glmer)

# Follow-up regressions for interaction
data_burst_ml = subset(data_burst, context_cat=="ML")
	data_burst_ml$context_cat = factor(data_burst_ml$context_cat)
data_burst_cs = subset(data_burst, context_cat=="CS")
	data_burst_cs$context_cat = factor(data_burst_cs$context_cat)
summary(glm(burst_presence ~ context_start_lg, data_burst_ml, family="binomial"))
summary(glm(burst_presence ~ context_start_lg, data_burst_cs, family="binomial"))

data_burst_e = subset(data_burst, context_start_lg=="E")
	data_burst_e$context_start_lg = factor(data_burst_e$context_start_lg)
data_burst_s = subset(data_burst, context_start_lg=="S")
	data_burst_s$context_start_lg = factor(data_burst_s$context_start_lg)
summary(glm(burst_presence ~ context_cat, data_burst_e, family="binomial"))
summary(glm(burst_presence ~ context_cat, data_burst_s, family="binomial"))











