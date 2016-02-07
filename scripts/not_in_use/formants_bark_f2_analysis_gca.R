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

# Set up percentage as polynomial regression
t <- poly(unique(data_noout$percentage), 2)
ot <- t[,1:2]
data_noout$ot1 <- ot[,1]
data_noout$ot2 <- ot[,2]



## RUN LMERS
# Full model
f2.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast * (ot1 + ot2) + sexContrast + (1+context_catContrast+context_start_lgContrast+(ot1+ot2)|speaker), data=data_noout, REML=F)

summary(f2.lmer)

# Test for effect of context
f2_context.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast * (ot1 + ot2) + sexContrast - context_catContrast + (1+context_catContrast+context_start_lgContrast+(ot1+ot2)|speaker), data=data_noout, REML=F)

anova(f2.lmer, f2_context.lmer)

# Test for effect of starting language
f2_startlg.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast * (ot1 + ot2) + sexContrast - context_start_lgContrast + (1+context_catContrast + context_start_lgContrast|speaker), data=data_noout, REML=F)

anova(f2.lmer, f2_startlg.lmer)

# Test for effect of percentage - ot1
f2_percentage_ot1.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast * (ot1 + ot2) + sexContrast - ot1 + (1+context_catContrast + context_start_lgContrast|speaker), data=data_noout, REML=F)

anova(f2.lmer, f2_percentage_ot1.lmer)

# Test for effect of percentage - ot2
f2_percentage_ot2.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast * (ot1 + ot2) + sexContrast - ot2 + (1+context_catContrast + context_start_lgContrast|speaker), data=data_noout, REML=F)

anova(f2.lmer, f2_percentage_ot2.lmer)

# Test for effect of sex
f2_sex.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast * (ot1 + ot2) + sexContrast - sexContrast + (1+context_catContrast + context_start_lgContrast|speaker), data=data_noout, REML=F)

anova(f2.lmer, f2_sex.lmer)

# Test for interaction of context and starting language
f2_contextxstartlg.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast * (ot1 + ot2) + sexContrast - context_catContrast:context_start_lgContrast + (1+context_catContrast + context_start_lgContrast|speaker), data=data_noout, REML=F)

anova(f2.lmer, f2_contextxstartlg.lmer)

# Test for interaction of context and percentage - ot1
f2_contextxpercentage_ot1.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast * (ot1 + ot2) + sexContrast - context_catContrast:ot1 + (1+context_catContrast + context_start_lgContrast|speaker), data=data_noout, REML=F)

anova(f2.lmer, f2_contextxpercentage_ot1.lmer)

# Test for interaction of context and percentage - ot2
f2_contextxpercentage_ot2.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast * (ot1 + ot2) + sexContrast - context_catContrast:ot2 + (1+context_catContrast + context_start_lgContrast|speaker), data=data_noout, REML=F)

anova(f2.lmer, f2_contextxpercentage_ot2.lmer)

# Test for interaction of starting language and percentage - ot1
f2_startlgxpercentage_ot1.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast * (ot1 + ot2) + sexContrast - context_start_lgContrast:ot1 + (1+context_catContrast + context_start_lgContrast|speaker), data=data_noout, REML=F)

anova(f2.lmer, f2_startlgxpercentage_ot1.lmer)

# Test for interaction of starting language and percentage - ot2
f2_startlgxpercentage_ot2.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast * (ot1 + ot2) + sexContrast - context_start_lgContrast:ot2 + (1+context_catContrast + context_start_lgContrast|speaker), data=data_noout, REML=F)

anova(f2.lmer, f2_startlgxpercentage_ot2.lmer)

# Test for interaction of context, starting language, and percentage - ot1
f2_contextxstartlgxpercentage_ot1.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast * (ot1 + ot2) + sexContrast - context_catContrast:context_start_lgContrast:ot1 + (1+context_catContrast + context_start_lgContrast|speaker), data=data_noout, REML=F)

anova(f2.lmer, f2_contextxstartlgxpercentage_ot1.lmer)

# Test for interaction of context, starting language, and percentage - ot2
f2_contextxstartlgxpercentage_ot2.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast * (ot1 + ot2) + sexContrast - context_catContrast:context_start_lgContrast:ot2 + (1+context_catContrast + context_start_lgContrast|speaker), data=data_noout, REML=F)

anova(f2.lmer, f2_contextxstartlgxpercentage_ot2.lmer)

# Follow up regressions for starting languge x percentage interaction
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









