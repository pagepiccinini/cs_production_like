## ORGAINZIE DATA
# Set working directory for source file and read in file for organizing data
setwd("~/Desktop/Experiments/CS E-S Production - like/Results/scripts/")
source("data_organization_formants_segments_bark.R")


## PREPARE VARAIBLES FOR LMER
# Context (baseline set to ML)
contrasts(data_l_summ$context_cat) = c(0.5, -0.5)
data_l_summ$context_catContrast = contrasts(data_l_summ$context_cat)[,1][as.numeric(data_l_summ$context_cat)]

# Language at start of 'like' (baseline set to English)
contrasts(data_l_summ$context_start_lg) = c(-0.5, 0.5)
data_l_summ$context_start_lgContrast = contrasts(data_l_summ$context_start_lg)[,1][as.numeric(data_l_summ$context_start_lg)]

# Task category (baseline set to c)
contrasts(data_l_summ$task) = c(-0.5, 0.5)
data_l_summ$taskContrast = contrasts(data_l_summ$task)[,1][as.numeric(data_l_summ$task)]

# Grammatical category (baseline set to D)
contrasts(data_l_summ$gram_cat) = c(-0.5, 0.5)
data_l_summ$gram_catContrast = contrasts(data_l_summ$gram_cat)[,1][as.numeric(data_l_summ$gram_cat)]

# Sex (baseline set to F)
contrasts(data_l_summ$sex) = c(-0.5, 0.5)
data_l_summ$sexContrast = contrasts(data_l_summ$sex)[,1][as.numeric(data_l_summ$sex)]


## RUN LMER ON F1
# Full model
l_f1.lmer = lmer(mean_f1_norm_bark ~ context_catContrast * context_start_lgContrast + sexContrast + (1+context_catContrast * context_start_lgContrast|speaker), data=data_l_summ, REML=F)

summary(l_f1.lmer)

# Test for effect of context
l_f1_context.lmer = lmer(mean_f1_norm_bark ~ context_catContrast * context_start_lgContrast + sexContrast - context_catContrast + (1+context_catContrast * context_start_lgContrast|speaker), data=data_l_summ, REML=F)

anova(l_f1.lmer, l_f1_context.lmer)

# Test for effect of starting language
l_f1_startlg.lmer = lmer(mean_f1_norm_bark ~ context_catContrast * context_start_lgContrast + sexContrast - context_start_lgContrast + (1+context_catContrast * context_start_lgContrast|speaker), data=data_l_summ, REML=F)

anova(l_f1.lmer, l_f1_startlg.lmer)

# Test for effect of sex
l_f1_sex.lmer = lmer(mean_f1_norm_bark ~ context_catContrast * context_start_lgContrast + sexContrast - sexContrast + (1+context_catContrast * context_start_lgContrast|speaker), data=data_l_summ, REML=F)

anova(l_f1.lmer, l_f1_sex.lmer)

# Test for interaction of context and starting language
l_f1_contextxstartlg.lmer = lmer(mean_f1_norm_bark ~ context_catContrast * context_start_lgContrast + sexContrast - context_catContrast:context_start_lgContrast + (1+context_catContrast * context_start_lgContrast|speaker), data=data_l_summ, REML=F)

anova(l_f1.lmer, l_f1_contextxstartlg.lmer)


## RUN LMER ON F2
# Full model
l_f2.lmer = lmer(mean_f2_norm_bark ~ context_catContrast * context_start_lgContrast + sexContrast + (1+context_catContrast * context_start_lgContrast|speaker), data=data_l_summ, REML=F)

summary(l_f2.lmer)

# Test for effect of context
l_f2_context.lmer = lmer(mean_f2_norm_bark ~ context_catContrast * context_start_lgContrast + sexContrast - context_catContrast + (1+context_catContrast * context_start_lgContrast|speaker), data=data_l_summ, REML=F)

anova(l_f2.lmer, l_f2_context.lmer)

# Test for effect of starting language
l_f2_startlg.lmer = lmer(mean_f2_norm_bark ~ context_catContrast * context_start_lgContrast + sexContrast - context_start_lgContrast + (1+context_catContrast * context_start_lgContrast|speaker), data=data_l_summ, REML=F)

anova(l_f2.lmer, l_f2_startlg.lmer)

# Test for effect of sex
l_f2_sex.lmer = lmer(mean_f2_norm_bark ~ context_catContrast * context_start_lgContrast + sexContrast - sexContrast + (1+context_catContrast * context_start_lgContrast|speaker), data=data_l_summ, REML=F)

anova(l_f2.lmer, l_f2_sex.lmer)

# Test for interaction of context and starting language
l_f2_contextxstartlg.lmer = lmer(mean_f2_norm_bark ~ context_catContrast * context_start_lgContrast + sexContrast - context_catContrast:context_start_lgContrast + (1+context_catContrast * context_start_lgContrast|speaker), data=data_l_summ, REML=F)

anova(l_f2.lmer, l_f2_contextxstartlg.lmer)









