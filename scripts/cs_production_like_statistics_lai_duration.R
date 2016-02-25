## LOAD PACKAGES ####
library(lme4)


## READ IN DATA ####
source("scripts/cs_production_like_cleaning_duration.R")
	
	
## ORGANIZE DATA ####
# Context (baseline set to ML)
contrasts(data_sub$context_cat) = c(0.5, -0.5)
data_sub$context_catContrast = contrasts(data_sub$context_cat)[,1][as.numeric(data_sub$context_cat)]

# Language at start of 'like' (baseline set to English)
contrasts(data_sub$context_start_lg) = c(-0.5, 0.5)
data_sub$context_start_lgContrast = contrasts(data_sub$context_start_lg)[,1][as.numeric(data_sub$context_start_lg)]

# Task category (baseline set to c)
contrasts(data_sub$task) = c(-0.5, 0.5)
data_sub$taskContrast = contrasts(data_sub$task)[,1][as.numeric(data_sub$task)]

# Grammatical category (baseline set to D)
contrasts(data_sub$gram_cat) = c(-0.5, 0.5)
data_sub$gram_catContrast = contrasts(data_sub$gram_cat)[,1][as.numeric(data_sub$gram_cat)]


## RUN LMERS ON DURATION OF 'LIKE' ####
# Full model
data_sub.lmer = lmer(like_duration_ms ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast +
                       (1+context_catContrast+context_start_lgContrast+taskContrast|speaker), data=data_sub, REML=F)

summary(data_sub.lmer)

# Test for effect of context
data_sub_context.lmer = lmer(like_duration_ms ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - context_catContrast + (1+context_catContrast+context_start_lgContrast+taskContrast|speaker), data=data_sub, REML=F)

anova(data_sub.lmer, data_sub_context.lmer)

# Test for effect of starting language
data_sub_startlg.lmer = lmer(like_duration_ms ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - context_start_lgContrast + (1+context_catContrast+context_start_lgContrast+taskContrast|speaker), data=data_sub, REML=F)

anova(data_sub.lmer, data_sub_startlg.lmer)

# Test for effect of task
data_sub_task.lmer = lmer(like_duration_ms ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - taskContrast + (1+context_catContrast+context_start_lgContrast+taskContrast|speaker), data=data_sub, REML=F)

anova(data_sub.lmer, data_sub_task.lmer)

# Test for effect of grammatical category
data_sub_gramcat.lmer = lmer(like_duration_ms ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - gram_catContrast + (1+context_catContrast+context_start_lgContrast+taskContrast|speaker), data=data_sub, REML=F)

anova(data_sub.lmer, data_sub_gramcat.lmer)

# Test for interaction of context and starting language
data_sub_contextxstartlg.lmer = lmer(like_duration_ms ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - context_catContrast:context_start_lgContrast + (1+context_catContrast+context_start_lgContrast+taskContrast|speaker), data=data_sub, REML=F)

anova(data_sub.lmer, data_sub_contextxstartlg.lmer)

# Test for interaction of context and task
data_sub_contextxtask.lmer = lmer(like_duration_ms ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - context_catContrast:taskContrast + (1+context_catContrast+context_start_lgContrast+taskContrast|speaker), data=data_sub, REML=F)

anova(data_sub.lmer, data_sub_contextxtask.lmer)

# Test for interaction of starting language and task
data_sub_startlgxtask.lmer = lmer(like_duration_ms ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - context_start_lgContrast:taskContrast + (1+context_catContrast+context_start_lgContrast+taskContrast|speaker), data=data_sub, REML=F)

anova(data_sub.lmer, data_sub_startlgxtask.lmer)

# Test for interaction of context, starting language, and task
data_sub_contextxstartlgxtask.lmer = lmer(like_duration_ms ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - context_catContrast:context_start_lgContrast:taskContrast + (1+context_catContrast+context_start_lgContrast+taskContrast|speaker), data=data_sub, REML=F)

anova(data_sub.lmer, data_sub_contextxstartlgxtask.lmer)

# Follow-up regressions for interaction
data_ml = subset(data_sub, context_cat=="ML")
	data_ml$context_cat = factor(data_ml$context_cat)
data_cs = subset(data_sub, context_cat=="CS")
	data_cs$context_cat = factor(data_cs$context_cat)
summary(lm(like_duration_ms ~ context_start_lg, data_ml))
summary(lm(like_duration_ms ~ context_start_lg, data_cs))

data_e = subset(data_sub, context_start_lg=="E")
	data_e$context_start_lg = factor(data_e$context_start_lg)
data_s = subset(data_sub, context_start_lg=="S")
	data_s$context_start_lg = factor(data_s$context_start_lg)
summary(lm(like_duration_ms ~ context_cat, data_e))
summary(lm(like_duration_ms ~ context_cat, data_s))











