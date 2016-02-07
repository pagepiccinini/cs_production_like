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
f2.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast * percentage +
                 (1+context_catContrast*context_start_lgContrast|speaker), data=data_noout, REML=F)

f2.lmer_sum = summary(f2.lmer)

# Test for effect of context
f2_context.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast * percentage - context_catContrast +
                         (1+context_catContrast*context_start_lgContrast|speaker), data=data_noout, REML=F)

f2_context.anova = anova(f2.lmer, f2_context.lmer)

# Test for effect of starting language
f2_startlg.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast * percentage - context_start_lgContrast +
                         (1+context_catContrast*context_start_lgContrast|speaker), data=data_noout, REML=F)

f2_startlg.anova = anova(f2.lmer, f2_startlg.lmer)

# Test for effect of percentage
f2_percentage.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast * percentage - percentage +
                            (1+context_catContrast*context_start_lgContrast|speaker), data=data_noout, REML=F)

f2_percentage.anova = anova(f2.lmer, f2_percentage.lmer)

# Test for interaction of context and starting language
f2_contextxstartlg.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast * percentage - context_catContrast:context_start_lgContrast +
                                 (1+context_catContrast*context_start_lgContrast|speaker), data=data_noout, REML=F)

f2_contextxstartlg.anova = anova(f2.lmer, f2_contextxstartlg.lmer)

# Test for interaction of context and percentage
f2_contextxpercentage.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast * percentage - context_catContrast:percentage +
                                    (1+context_catContrast*context_start_lgContrast|speaker), data=data_noout, REML=F)

f2_contextxpercentage.anova = anova(f2.lmer, f2_contextxpercentage.lmer)

# Test for interaction of starting language and percentage
f2_startlgxpercentage.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast * percentage - context_start_lgContrast:percentage +
                                    (1+context_catContrast*context_start_lgContrast|speaker), data=data_noout, REML=F)

f2_startlgxpercentage.anova = anova(f2.lmer, f2_startlgxpercentage.lmer)

# Test for interaction of context, starting language, and percentage
f2_contextxstartlgxpercentage.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast * percentage - context_catContrast:context_start_lgContrast:percentage +
                                            (1+context_catContrast*context_start_lgContrast|speaker), data=data_noout, REML=F)

f2_contextxstartlgxpercentage.anova = anova(f2.lmer, f2_contextxstartlgxpercentage.lmer)


## FOLLOW UP REGRESSIONS FOR CONTEXT X STARTING LANGUAGE x PERCENTAGE INTERACTION ####
f2_eng = subset(data_noout, context_start_lg == "E")
	f2_eng$context_start_lg = factor(f2_eng$context_start_lg)
	f2_eng_ml = subset(f2_eng, context_cat=="ML")
		f2_eng_ml$context_cat = factor(f2_eng_ml$context_cat)
	f2_eng_cs = subset(f2_eng, context_cat=="CS")
		f2_eng_cs$context_cat = factor(f2_eng_cs$context_cat)
f2_sp = subset(data_noout, context_start_lg == "S")
	f2_sp$context_start_lg = factor(f2_sp$context_start_lg)
	f2_sp_ml = subset(f2_sp, context_cat=="ML")
		f2_sp_ml$context_cat = factor(f2_sp_ml$context_cat)
	f2_sp_cs = subset(f2_sp, context_cat=="CS")
		f2_sp_cs$context_cat = factor(f2_sp_cs$context_cat)
		
summary(lm(f2_norm_bark ~ percentage, f2_eng_ml))
summary(lm(f2_norm_bark ~ percentage, f2_eng_cs))

summary(lm(f2_norm_bark ~ percentage, f2_sp_ml))
summary(lm(f2_norm_bark ~ percentage, f2_sp_cs))









