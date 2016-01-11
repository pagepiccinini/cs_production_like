## ORGAINZIE DATA
# Set working directory for source file and read in file for organizing data
setwd("~/Desktop/Experiments/CS E-S Production - like/Results/scripts/")
source("data_organization_formants.R")


## PREPARE VARAIBLES FOR LMER
# Context (baseline set to ML)
contrasts(f1_noout$context_cat) = c(0.5, -0.5)
f1_noout$context_catContrast = contrasts(f1_noout$context_cat)[,1][as.numeric(f1_noout$context_cat)]

# Language at start of 'like' (baseline set to English)
contrasts(f1_noout$context_start_lg) = c(-0.5, 0.5)
f1_noout$context_start_lgContrast = contrasts(f1_noout$context_start_lg)[,1][as.numeric(f1_noout$context_start_lg)]

# Task category (baseline set to c)
contrasts(f1_noout$task) = c(-0.5, 0.5)
f1_noout$taskContrast = contrasts(f1_noout$task)[,1][as.numeric(f1_noout$task)]

# Grammatical category (baseline set to D)
contrasts(f1_noout$gram_cat) = c(-0.5, 0.5)
f1_noout$gram_catContrast = contrasts(f1_noout$gram_cat)[,1][as.numeric(f1_noout$gram_cat)]

# Grammatical category (baseline set to F)
contrasts(f1_noout$sex) = c(-0.5, 0.5)
f1_noout$sexContrast = contrasts(f1_noout$sex)[,1][as.numeric(f1_noout$sex)]


## RUN LMERS
# Full model
f1.lmer = lmer(f1 ~ context_catContrast * context_start_lgContrast * percentage + sexContrast + (1+context_catContrast + context_start_lgContrast|speaker), data=f1_noout, REML=F)

summary(f1.lmer)

# Test for effect of context
f1_context.lmer = lmer(f1 ~ context_catContrast * context_start_lgContrast * percentage + sexContrast - context_catContrast + (1+context_catContrast + context_start_lgContrast|speaker), data=f1_noout, REML=F)

anova(f1.lmer, f1_context.lmer)

# Test for effect of starting language
f1_startlg.lmer = lmer(f1 ~ context_catContrast * context_start_lgContrast * percentage + sexContrast - context_start_lgContrast + (1+context_catContrast + context_start_lgContrast|speaker), data=f1_noout, REML=F)

anova(f1.lmer, f1_startlg.lmer)

# Test for effect of percentage
f1_percentage.lmer = lmer(f1 ~ context_catContrast * context_start_lgContrast * percentage + sexContrast - percentage + (1+context_catContrast + context_start_lgContrast|speaker), data=f1_noout, REML=F)

anova(f1.lmer, f1_percentage.lmer)

# Test for effect of sex
f1_sex.lmer = lmer(f1 ~ context_catContrast * context_start_lgContrast * percentage + sexContrast - sexContrast + (1+context_catContrast + context_start_lgContrast|speaker), data=f1_noout, REML=F)

anova(f1.lmer, f1_sex.lmer)

# Test for interaction of context and starting language
f1_contextxstartlg.lmer = lmer(f1 ~ context_catContrast * context_start_lgContrast * percentage + sexContrast - context_catContrast:context_start_lgContrast + (1+context_catContrast + context_start_lgContrast|speaker), data=f1_noout, REML=F)

anova(f1.lmer, f1_contextxstartlg.lmer)

# Test for interaction of context and percentage
f1_contextxpercentage.lmer = lmer(f1 ~ context_catContrast * context_start_lgContrast * percentage + sexContrast - context_catContrast:percentage + (1+context_catContrast + context_start_lgContrast|speaker), data=f1_noout, REML=F)

anova(f1.lmer, f1_contextxpercentage.lmer)

# Test for interaction of starting language and percentage
f1_startlgxpercentage.lmer = lmer(f1 ~ context_catContrast * context_start_lgContrast * percentage + sexContrast - context_start_lgContrast:percentage + (1+context_catContrast + context_start_lgContrast|speaker), data=f1_noout, REML=F)

anova(f1.lmer, f1_startlgxpercentage.lmer)

# Test for interaction of context, starting language, and percentage
f1_contextxstartlgxpercentage.lmer = lmer(f1 ~ context_catContrast * context_start_lgContrast * percentage + sexContrast - context_catContrast:context_start_lgContrast:percentage + (1+context_catContrast + context_start_lgContrast|speaker), data=f1_noout, REML=F)

anova(f1.lmer, f1_contextxstartlgxpercentage.lmer)

# Follow up regressions for starting languge x percentage interaction
f1_eng = subset(f1_noout, context_start_lg == "E")
	f1_eng$context_start_lg = factor(f1_eng$context_start_lg)
f1_sp = subset(f1_noout, context_start_lg == "S")
	f1_sp$context_start_lg = factor(f1_sp$context_start_lg)
summary(lm(f1 ~ percentage, f1_eng))
summary(lm(f1 ~ percentage, f1_sp))









