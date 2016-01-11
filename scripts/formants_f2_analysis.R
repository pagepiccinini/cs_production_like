## ORGAINZIE DATA
# Set working directory for source file and read in file for organizing data
setwd("~/Desktop/Experiments/CS E-S Production - like/Results/scripts/")
source("data_organization_formants.R")


## PREPARE VARAIBLES FOR LMER
# Context (baseline set to ML)
contrasts(f2_noout$context_cat) = c(0.5, -0.5)
f2_noout$context_catContrast = contrasts(f2_noout$context_cat)[,1][as.numeric(f2_noout$context_cat)]

# Language at start of 'like' (baseline set to English)
contrasts(f2_noout$context_start_lg) = c(-0.5, 0.5)
f2_noout$context_start_lgContrast = contrasts(f2_noout$context_start_lg)[,1][as.numeric(f2_noout$context_start_lg)]

# Task category (baseline set to c)
contrasts(f2_noout$task) = c(-0.5, 0.5)
f2_noout$taskContrast = contrasts(f2_noout$task)[,1][as.numeric(f2_noout$task)]

# Grammatical category (baseline set to D)
contrasts(f2_noout$gram_cat) = c(-0.5, 0.5)
f2_noout$gram_catContrast = contrasts(f2_noout$gram_cat)[,1][as.numeric(f2_noout$gram_cat)]

# Grammatical category (baseline set to F)
contrasts(f2_noout$sex) = c(-0.5, 0.5)
f2_noout$sexContrast = contrasts(f2_noout$sex)[,1][as.numeric(f2_noout$sex)]


## RUN LMERS
# Full model
f2.lmer = lmer(f2 ~ context_catContrast * context_start_lgContrast * percentage + sexContrast + (1+context_catContrast+context_start_lgContrast|speaker), data=f2_noout, REML=F)

summary(f2.lmer)

# Test for effect of context
f2_context.lmer = lmer(f2 ~ context_catContrast * context_start_lgContrast * percentage + sexContrast - context_catContrast + (1+context_catContrast+context_start_lgContrast|speaker), data=f2_noout, REML=F)

anova(f2.lmer, f2_context.lmer)

# Test for effect of starting language
f2_startlg.lmer = lmer(f2 ~ context_catContrast * context_start_lgContrast * percentage + sexContrast - context_start_lgContrast + (1+context_catContrast+context_start_lgContrast|speaker), data=f2_noout, REML=F)

anova(f2.lmer, f2_startlg.lmer)

# Test for effect of percentage
f2_percentage.lmer = lmer(f2 ~ context_catContrast * context_start_lgContrast * percentage + sexContrast - percentage + (1+context_catContrast + context_start_lgContrast|speaker), data=f2_noout, REML=F)

anova(f2.lmer, f2_percentage.lmer)

# Test for effect of sex
f2_sex.lmer = lmer(f2 ~ context_catContrast * context_start_lgContrast * percentage + sexContrast - sexContrast + (1+context_catContrast+context_start_lgContrast|speaker), data=f2_noout, REML=F)

anova(f2.lmer, f2_sex.lmer)

# Test for interaction of context and starting language
f2_contextxstartlg.lmer = lmer(f2 ~ context_catContrast * context_start_lgContrast * percentage + sexContrast - context_catContrast:context_start_lgContrast + (1+context_catContrast + context_start_lgContrast|speaker), data=f2_noout, REML=F)

anova(f2.lmer, f2_contextxstartlg.lmer)

# Test for interaction of context and percentage
f2_contextxpercentage.lmer = lmer(f2 ~ context_catContrast * context_start_lgContrast * percentage + sexContrast - context_catContrast:percentage + (1+context_catContrast + context_start_lgContrast|speaker), data=f2_noout, REML=F)

anova(f2.lmer, f2_contextxpercentage.lmer)

# Test for interaction of starting language and percentage
f2_startlgxpercentage.lmer = lmer(f2 ~ context_catContrast * context_start_lgContrast * percentage + sexContrast - context_start_lgContrast:percentage + (1+context_catContrast + context_start_lgContrast|speaker), data=f2_noout, REML=F)

anova(f2.lmer, f2_startlgxpercentage.lmer)

# Test for interaction of context, starting language, and percentage
f2_contextxstartlgxpercentage.lmer = lmer(f2 ~ context_catContrast * context_start_lgContrast * percentage + sexContrast - context_catContrast:context_start_lgContrast:percentage + (1+context_catContrast + context_start_lgContrast|speaker), data=f2_noout, REML=F)

anova(f2.lmer, f2_contextxstartlgxpercentage.lmer)

# Follow up regressions for starting languge x percentage interaction
f2_eng = subset(f2_noout, context_start_lg == "E")
	f2_eng$context_start_lg = factor(f2_eng$context_start_lg)
f2_sp = subset(f2_noout, context_start_lg == "S")
	f2_sp$context_start_lg = factor(f2_sp$context_start_lg)
summary(lm(f2 ~ percentage, f2_eng))
summary(lm(f2 ~ percentage, f2_sp))









