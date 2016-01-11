## ORGAINZIE DATA
# Set working directory for source file and read in file for organizing data
setwd("~/Desktop/Experiments/CS E-S Production - like/Results/scripts/")
source("data_organization_formants.R")


## PREPARE VARAIBLES FOR LMER
# Context (baseline set to ML)
contrasts(f3_noout$context_cat) = c(0.5, -0.5)
f3_noout$context_catContrast = contrasts(f3_noout$context_cat)[,1][as.numeric(f3_noout$context_cat)]

# Language at start of 'like' (baseline set to English)
contrasts(f3_noout$context_start_lg) = c(-0.5, 0.5)
f3_noout$context_start_lgContrast = contrasts(f3_noout$context_start_lg)[,1][as.numeric(f3_noout$context_start_lg)]

# Task category (baseline set to c)
contrasts(f3_noout$task) = c(-0.5, 0.5)
f3_noout$taskContrast = contrasts(f3_noout$task)[,1][as.numeric(f3_noout$task)]

# Grammatical category (baseline set to D)
contrasts(f3_noout$gram_cat) = c(-0.5, 0.5)
f3_noout$gram_catContrast = contrasts(f3_noout$gram_cat)[,1][as.numeric(f3_noout$gram_cat)]

# Grammatical category (baseline set to F)
contrasts(f3_noout$sex) = c(-0.5, 0.5)
f3_noout$sexContrast = contrasts(f3_noout$sex)[,1][as.numeric(f3_noout$sex)]


## RUN LMERS
# Full model
f3.lmer = lmer(f3 ~ context_catContrast * context_start_lgContrast * percentage + sexContrast + (1+context_catContrast+context_start_lgContrast|speaker), data=f3_noout, REML=F)

summary(f3.lmer)

# Test for effect of context
f3_context.lmer = lmer(f3 ~ context_catContrast * context_start_lgContrast * percentage + sexContrast - context_catContrast + (1+context_catContrast+context_start_lgContrast|speaker), data=f3_noout, REML=F)

anova(f3.lmer, f3_context.lmer)

# Test for effect of starting language
f3_startlg.lmer = lmer(f3 ~ context_catContrast * context_start_lgContrast * percentage + sexContrast - context_start_lgContrast + (1+context_catContrast+context_start_lgContrast|speaker), data=f3_noout, REML=F)

anova(f3.lmer, f3_startlg.lmer)

# Test for effect of percentage
f3_percentage.lmer = lmer(f3 ~ context_catContrast * context_start_lgContrast * percentage + sexContrast - percentage + (1+context_catContrast + context_start_lgContrast|speaker), data=f3_noout, REML=F)

anova(f3.lmer, f3_percentage.lmer)

# Test for effect of sex
f3_sex.lmer = lmer(f3 ~ context_catContrast * context_start_lgContrast * percentage + sexContrast - sexContrast + (1+context_catContrast+context_start_lgContrast|speaker), data=f3_noout, REML=F)

anova(f3.lmer, f3_sex.lmer)

# Test for interaction of context and starting language
f3_contextxstartlg.lmer = lmer(f3 ~ context_catContrast * context_start_lgContrast * percentage + sexContrast - context_catContrast:context_start_lgContrast + (1+context_catContrast + context_start_lgContrast|speaker), data=f3_noout, REML=F)

anova(f3.lmer, f3_contextxstartlg.lmer)

# Test for interaction of context and percentage
f3_contextxpercentage.lmer = lmer(f3 ~ context_catContrast * context_start_lgContrast * percentage + sexContrast - context_catContrast:percentage + (1+context_catContrast + context_start_lgContrast|speaker), data=f3_noout, REML=F)

anova(f3.lmer, f3_contextxpercentage.lmer)

# Test for interaction of starting language and percentage
f3_startlgxpercentage.lmer = lmer(f3 ~ context_catContrast * context_start_lgContrast * percentage + sexContrast - context_start_lgContrast:percentage + (1+context_catContrast + context_start_lgContrast|speaker), data=f3_noout, REML=F)

anova(f3.lmer, f3_startlgxpercentage.lmer)

# Test for interaction of context, starting language, and percentage
f3_contextxstartlgxpercentage.lmer = lmer(f3 ~ context_catContrast * context_start_lgContrast * percentage + sexContrast - context_catContrast:context_start_lgContrast:percentage + (1+context_catContrast + context_start_lgContrast|speaker), data=f3_noout, REML=F)

anova(f3.lmer, f3_contextxstartlgxpercentage.lmer)

# Follow up regressions for starting languge x percentage interaction
f3_eng = subset(f3_noout, context_start_lg == "E")
	f3_eng$context_start_lg = factor(f3_eng$context_start_lg)
	f3_eng_ml = subset(f3_eng, context_cat=="ML")
		f3_eng_ml$context_cat = factor(f3_eng_ml$context_cat)
	f3_eng_cs = subset(f3_eng, context_cat=="CS")
		f3_eng_cs$context_cat = factor(f3_eng_cs$context_cat)
f3_sp = subset(f3_noout, context_start_lg == "S")
	f3_sp$context_start_lg = factor(f3_sp$context_start_lg)
	f3_sp_ml = subset(f3_sp, context_cat=="ML")
		f3_sp_ml$context_cat = factor(f3_sp_ml$context_cat)
	f3_sp_cs = subset(f3_sp, context_cat=="CS")
		f3_sp_cs$context_cat = factor(f3_sp_cs$context_cat)

summary(lm(f3 ~ percentage, f3_eng_ml))
summary(lm(f3 ~ percentage, f3_eng_cs))

summary(lm(f3 ~ percentage, f3_sp_ml))
summary(lm(f3 ~ percentage, f3_sp_cs))









