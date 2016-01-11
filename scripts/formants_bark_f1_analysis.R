## ORGAINZIE DATA
# Set working directory for source file and read in file for organizing data
setwd("~/Desktop/Experiments/CS E-S Production - like/Results/scripts/")
source("data_organization_formants_bark.R")


## PREPARE VARAIBLES FOR LMER
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

# Grammatical category (baseline set to F)
contrasts(data_noout$sex) = c(-0.5, 0.5)
data_noout$sexContrast = contrasts(data_noout$sex)[,1][as.numeric(data_noout$sex)]


## RUN LMERS
# Full model
f1.lmer = lmer(f1_norm_bark ~ context_catContrast * context_start_lgContrast * percentage + sexContrast + (1+context_catContrast + context_start_lgContrast|speaker), data=data_noout, REML=F)

summary(f1.lmer)

# Test for effect of context
f1_context.lmer = lmer(f1_norm_bark ~ context_catContrast * context_start_lgContrast * percentage + sexContrast - context_catContrast + (1+context_catContrast + context_start_lgContrast|speaker), data=data_noout, REML=F)

anova(f1.lmer, f1_context.lmer)

# Test for effect of starting language
f1_startlg.lmer = lmer(f1_norm_bark ~ context_catContrast * context_start_lgContrast * percentage + sexContrast - context_start_lgContrast + (1+context_catContrast + context_start_lgContrast|speaker), data=data_noout, REML=F)

anova(f1.lmer, f1_startlg.lmer)

# Test for effect of percentage
f1_percentage.lmer = lmer(f1_norm_bark ~ context_catContrast * context_start_lgContrast * percentage + sexContrast - percentage + (1+context_catContrast + context_start_lgContrast|speaker), data=data_noout, REML=F)

anova(f1.lmer, f1_percentage.lmer)

# Test for effect of sex
f1_sex.lmer = lmer(f1_norm_bark ~ context_catContrast * context_start_lgContrast * percentage + sexContrast - sexContrast + (1+context_catContrast + context_start_lgContrast|speaker), data=data_noout, REML=F)

anova(f1.lmer, f1_sex.lmer)

# Test for interaction of context and starting language
f1_contextxstartlg.lmer = lmer(f1_norm_bark ~ context_catContrast * context_start_lgContrast * percentage + sexContrast - context_catContrast:context_start_lgContrast + (1+context_catContrast + context_start_lgContrast|speaker), data=data_noout, REML=F)

anova(f1.lmer, f1_contextxstartlg.lmer)

# Test for interaction of context and percentage
f1_contextxpercentage.lmer = lmer(f1_norm_bark ~ context_catContrast * context_start_lgContrast * percentage + sexContrast - context_catContrast:percentage + (1+context_catContrast + context_start_lgContrast|speaker), data=data_noout, REML=F)

anova(f1.lmer, f1_contextxpercentage.lmer)

# Test for interaction of starting language and percentage
f1_startlgxpercentage.lmer = lmer(f1_norm_bark ~ context_catContrast * context_start_lgContrast * percentage + sexContrast - context_start_lgContrast:percentage + (1+context_catContrast + context_start_lgContrast|speaker), data=data_noout, REML=F)

anova(f1.lmer, f1_startlgxpercentage.lmer)

# Test for interaction of context, starting language, and percentage
f1_contextxstartlgxpercentage.lmer = lmer(f1_norm_bark ~ context_catContrast * context_start_lgContrast * percentage + sexContrast - context_catContrast:context_start_lgContrast:percentage + (1+context_catContrast + context_start_lgContrast|speaker), data=data_noout, REML=F)

anova(f1.lmer, f1_contextxstartlgxpercentage.lmer)

# Follow up regressions for starting languge x percentage interaction
f1_eng = subset(data_noout, context_start_lg == "E")
	f1_eng$context_start_lg = factor(f1_eng$context_start_lg)
	f1_eng_ml = subset(f1_eng, context_cat=="ML")
		f1_eng_ml$context_cat = factor(f1_eng_ml$context_cat)
	f1_eng_cs = subset(f1_eng, context_cat=="CS")
		f1_eng_cs$context_cat = factor(f1_eng_cs$context_cat)
f1_sp = subset(data_noout, context_start_lg == "S")
	f1_sp$context_start_lg = factor(f1_sp$context_start_lg)
	f1_sp_ml = subset(f1_sp, context_cat=="ML")
		f1_sp_ml$context_cat = factor(f1_sp_ml$context_cat)
	f1_sp_cs = subset(f1_sp, context_cat=="CS")
		f1_sp_cs$context_cat = factor(f1_sp_cs$context_cat)
		
summary(lm(f1_norm_bark ~ percentage, f1_eng_ml))
summary(lm(f1_norm_bark ~ percentage, f1_eng_cs))

summary(lm(f1_norm_bark ~ percentage, f1_sp_ml))
summary(lm(f1_norm_bark ~ percentage, f1_sp_cs))









