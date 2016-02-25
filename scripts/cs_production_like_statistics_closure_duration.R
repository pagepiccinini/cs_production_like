## LOAD PACKAGES ####
library(lme4)
library(ggplot2)


## LOAD PACKAGES ####
library(lme4)


## READ IN DATA ####
source("scripts/cs_production_like_cleaning_duration.R")
	
# Subset out data where the duration of the closure is 0
data_clos = subset(data_sub, closure_duration > 0)
	
	
## PREPARE VARAIBLES FOR LMER ####
# Context (baseline set to ML)
contrasts(data_clos$context_cat) = c(0.5, -0.5)
data_clos$context_catContrast = contrasts(data_clos$context_cat)[,1][as.numeric(data_clos$context_cat)]

# Language at start of 'like' (baseline set to English)
contrasts(data_clos$context_start_lg) = c(-0.5, 0.5)
data_clos$context_start_lgContrast = contrasts(data_clos$context_start_lg)[,1][as.numeric(data_clos$context_start_lg)]

# Task category (baseline set to c)
contrasts(data_clos$task) = c(-0.5, 0.5)
data_clos$taskContrast = contrasts(data_clos$task)[,1][as.numeric(data_clos$task)]

# Grammatical category (baseline set to D)
contrasts(data_clos$gram_cat) = c(-0.5, 0.5)
data_clos$gram_catContrast = contrasts(data_clos$gram_cat)[,1][as.numeric(data_clos$gram_cat)]


## RUN LMERS ON DURATION OF CLOSURE ####
# Full model
data_clos.lmer = lmer(closure_duration ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast + (1+context_catContrast+context_start_lgContrast+taskContrast+gram_catContrast|speaker), data=data_clos, REML=F)

summary(data_clos.lmer)

# Test for effect of context
data_clos_context.lmer = lmer(closure_duration ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - context_catContrast + (1+context_catContrast+context_start_lgContrast+taskContrast+gram_catContrast|speaker), data=data_clos, REML=F)

anova(data_clos.lmer, data_clos_context.lmer)

# Test for effect of starting language
data_clos_startlg.lmer = lmer(closure_duration ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - context_start_lgContrast + (1+context_catContrast+context_start_lgContrast+taskContrast+gram_catContrast|speaker), data=data_clos, REML=F)

anova(data_clos.lmer, data_clos_startlg.lmer)

# Test for effect of task
data_clos_task.lmer = lmer(closure_duration ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - taskContrast + (1+context_catContrast+context_start_lgContrast+taskContrast+gram_catContrast|speaker), data=data_clos, REML=F)

anova(data_clos.lmer, data_clos_task.lmer)

# Test for effect of grammatical category
data_clos_gramcat.lmer = lmer(closure_duration ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - gram_catContrast + (1+context_catContrast+context_start_lgContrast+taskContrast+gram_catContrast|speaker), data=data_clos, REML=F)

anova(data_clos.lmer, data_clos_gramcat.lmer)

# Test for interaction of context and starting language
data_clos_contextxstartlg.lmer = lmer(closure_duration ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - context_catContrast:context_start_lgContrast + (1+context_catContrast+context_start_lgContrast+taskContrast+gram_catContrast|speaker), data=data_clos, REML=F)

anova(data_clos.lmer, data_clos_contextxstartlg.lmer)

# Test for interaction of context and task
data_clos_contextxtask.lmer = lmer(closure_duration ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - context_catContrast:taskContrast + (1+context_catContrast+context_start_lgContrast+taskContrast+gram_catContrast|speaker), data=data_clos, REML=F)

anova(data_clos.lmer, data_clos_contextxtask.lmer)

# Test for interaction of starting language and task
data_clos_startlgxtask.lmer = lmer(closure_duration ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - context_start_lgContrast:taskContrast + (1+context_catContrast+context_start_lgContrast+taskContrast+gram_catContrast|speaker), data=data_clos, REML=F)

anova(data_clos.lmer, data_clos_startlgxtask.lmer)

# Test for interaction of context, starting language, and task
data_clos_contextxstartlgxtask.lmer = lmer(closure_duration ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - context_catContrast:context_start_lgContrast:taskContrast + (1+context_catContrast+context_start_lgContrast+taskContrast+gram_catContrast|speaker), data=data_clos, REML=F)

anova(data_clos.lmer, data_clos_contextxstartlgxtask.lmer)

# Follow-up regressions for interaction
data_clos_ml = subset(data_clos, context_cat=="ML")
	data_clos_ml$context_cat = factor(data_clos_ml$context_cat)
data_clos_cs = subset(data_clos, context_cat=="CS")
	data_clos_cs$context_cat = factor(data_clos_cs$context_cat)
summary(lm(closure_duration ~ context_start_lg, data_clos_ml))
summary(lm(closure_duration ~ context_start_lg, data_clos_cs))

data_clos_e = subset(data_clos, context_start_lg=="E")
	data_clos_e$context_start_lg = factor(data_clos_e$context_start_lg)
data_clos_s = subset(data_clos, context_start_lg=="S")
	data_clos_s$context_start_lg = factor(data_clos_s$context_start_lg)
summary(lm(closure_duration ~ context_cat, data_clos_e))
summary(lm(closure_duration ~ context_cat, data_clos_s))











