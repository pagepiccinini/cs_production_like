## READ IN DATA ####
source("scripts/data_organization_formants_bark.R")


## LOAD PACKAGES ####
library(lme4)


## ORGANIZE DATA ####
# Context (baseline set to ML)
contrasts(data_noout$context_cat) = c(0.5, -0.5)
data_noout$context_catContrast = contrasts(data_noout$context_cat)[,1][as.numeric(data_noout$context_cat)]

# Language at start of 'like' (baseline set to English)
contrasts(data_noout$context_start_lg) = c(-0.5, 0.5)
data_noout$context_start_lgContrast = contrasts(data_noout$context_start_lg)[,1][as.numeric(data_noout$context_start_lg)]

# Task category (baseline set to c)
contrasts(data_noout$task) = c(-0.5, 0.5)
data_noout$taskContrast = contrasts(data_noout$task)[,1][as.numeric(data_noout$task)]

# Grammatical category (baseline set to D)
contrasts(data_noout$gram_cat) = c(-0.5, 0.5)
data_noout$gram_catContrast = contrasts(data_noout$gram_cat)[,1][as.numeric(data_noout$gram_cat)]


## RUN LMERS ####
# Full model
f1.lmer = lmer(f1_norm_bark ~ context_catContrast * context_start_lgContrast * percentage +
                 (1+context_catContrast+context_start_lgContrast|speaker), data=data_noout, REML=F)

f1.lmer_sum = summary(f1.lmer)

# Test for effect of context
f1_context.lmer = lmer(f1_norm_bark ~ context_catContrast * context_start_lgContrast * percentage - context_catContrast +
                         (1+context_catContrast+context_start_lgContrast|speaker), data=data_noout, REML=F)

f1_context.anova = anova(f1.lmer, f1_context.lmer)

# Test for effect of starting language
f1_startlg.lmer = lmer(f1_norm_bark ~ context_catContrast * context_start_lgContrast * percentage - context_start_lgContrast +
                         (1+context_catContrast+context_start_lgContrast|speaker), data=data_noout, REML=F)

f1_startlg.anova = anova(f1.lmer, f1_startlg.lmer)

# Test for effect of percentage
f1_percentage.lmer = lmer(f1_norm_bark ~ context_catContrast * context_start_lgContrast * percentage - percentage +
                            (1+context_catContrast+context_start_lgContrast|speaker), data=data_noout, REML=F)

f1_percentage.anova = anova(f1.lmer, f1_percentage.lmer)

# Test for interaction of context and starting language
f1_contextxstartlg.lmer = lmer(f1_norm_bark ~ context_catContrast * context_start_lgContrast * percentage - context_catContrast:context_start_lgContrast +
                                 (1+context_catContrast+context_start_lgContrast|speaker), data=data_noout, REML=F)

f1_contextxstartlg.anova = anova(f1.lmer, f1_contextxstartlg.lmer)

# Test for interaction of context and percentage
f1_contextxpercentage.lmer = lmer(f1_norm_bark ~ context_catContrast * context_start_lgContrast * percentage - context_catContrast:percentage +
                                    (1+context_catContrast+context_start_lgContrast|speaker), data=data_noout, REML=F)

f1_contextxpercentage.anova = anova(f1.lmer, f1_contextxpercentage.lmer)

# Test for interaction of starting language and percentage
f1_startlgxpercentage.lmer = lmer(f1_norm_bark ~ context_catContrast * context_start_lgContrast * percentage - context_start_lgContrast:percentage +
                                    (1+context_catContrast+context_start_lgContrast|speaker), data=data_noout, REML=F)

f1_startlgxpercentage.anova = anova(f1.lmer, f1_startlgxpercentage.lmer)

# Test for interaction of context, starting language, and percentage
f1_contextxstartlgxpercentage.lmer = lmer(f1_norm_bark ~ context_catContrast * context_start_lgContrast * percentage - context_catContrast:context_start_lgContrast:percentage +
                                            (1+context_catContrast+context_start_lgContrast|speaker), data=data_noout, REML=F)

f1_contextxstartlgxpercentage.anova = anova(f1.lmer, f1_contextxstartlgxpercentage.lmer)


## FOLLOW UP REGRESSIONS FOR CONTEXT X STARTING LANGUAGE x PERCENTAGE INTERACTION ####
# Organize data
f1_eng = data_noout %>%
  filter(context_start_lg == "E") %>%
	mutate(context_start_lg = factor(context_start_lg))
	       
f1_eng_ml = f1_eng %>%
  filter(context_cat == "ML") %>%
	mutate(context_cat = factor(context_cat))

f1_eng_cs = f1_eng %>%
  filter(context_cat == "CS") %>%
	mutate(context_cat = factor(context_cat))

f1_sp = data_noout %>%
  filter(context_start_lg == "S") %>%
	mutate(context_start_lg = factor(context_start_lg))

f1_sp_ml = f1_sp %>%
  filter(context_cat == "ML") %>%
	mutate(context_cat = factor(context_cat))

f1_sp_cs = f1_sp %>%
  filter(context_cat == "CS") %>%
	mutate(context_cat = factor(context_cat))

# Run regressions	
summary(lm(f1_norm_bark ~ percentage, f1_eng_ml))
summary(lm(f1_norm_bark ~ percentage, f1_eng_cs))

summary(lm(f1_norm_bark ~ percentage, f1_sp_ml))
summary(lm(f1_norm_bark ~ percentage, f1_sp_cs))









