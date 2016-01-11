## ORGAINZIE DATA
# Set working directory for source file and read in file for organizing data
setwd("~/Desktop/Experiments/CS E-S Production - like/Results/scripts/")
source("data_organization_formants_segments_bark.R")


## PREPARE VARAIBLES FOR LMER
# Context (baseline set to ML)
contrasts(data_ai_nuc_summ$context_cat) = c(0.5, -0.5)
data_ai_nuc_summ$context_catContrast = contrasts(data_ai_nuc_summ$context_cat)[,1][as.numeric(data_ai_nuc_summ$context_cat)]

# Language at start of 'like' (baseline set to English)
contrasts(data_ai_nuc_summ$context_start_lg) = c(-0.5, 0.5)
data_ai_nuc_summ$context_start_lgContrast = contrasts(data_ai_nuc_summ$context_start_lg)[,1][as.numeric(data_ai_nuc_summ$context_start_lg)]

# Task category (baseline set to c)
contrasts(data_ai_nuc_summ$task) = c(-0.5, 0.5)
data_ai_nuc_summ$taskContrast = contrasts(data_ai_nuc_summ$task)[,1][as.numeric(data_ai_nuc_summ$task)]

# Grammatical category (baseline set to D)
contrasts(data_ai_nuc_summ$gram_cat) = c(-0.5, 0.5)
data_ai_nuc_summ$gram_catContrast = contrasts(data_ai_nuc_summ$gram_cat)[,1][as.numeric(data_ai_nuc_summ$gram_cat)]

# Sex (baseline set to F)
contrasts(data_ai_nuc_summ$sex) = c(-0.5, 0.5)
data_ai_nuc_summ$sexContrast = contrasts(data_ai_nuc_summ$sex)[,1][as.numeric(data_ai_nuc_summ$sex)]


## RUN LMER ON F1
# Full model
nuc_f1.lmer = lmer(f1_norm_bark ~ context_catContrast * context_start_lgContrast + sexContrast + (1+context_catContrast + context_start_lgContrast|speaker), data=data_ai_nuc_summ, REML=F)

summary(nuc_f1.lmer)

# Test for effect of context
nuc_f1_context.lmer = lmer(f1_norm_bark ~ context_catContrast * context_start_lgContrast + sexContrast - context_catContrast + (1+context_catContrast + context_start_lgContrast|speaker), data=data_ai_nuc_summ, REML=F)

anova(nuc_f1.lmer, nuc_f1_context.lmer)

# Test for effect of starting language
nuc_f1_startlg.lmer = lmer(f1_norm_bark ~ context_catContrast * context_start_lgContrast + sexContrast - context_start_lgContrast + (1+context_catContrast + context_start_lgContrast|speaker), data=data_ai_nuc_summ, REML=F)

anova(nuc_f1.lmer, nuc_f1_startlg.lmer)

# Test for effect of sex
nuc_f1_sex.lmer = lmer(f1_norm_bark ~ context_catContrast * context_start_lgContrast + sexContrast - sexContrast + (1+context_catContrast + context_start_lgContrast|speaker), data=data_ai_nuc_summ, REML=F)

anova(nuc_f1.lmer, nuc_f1_sex.lmer)

# Test for interaction of context and starting language
nuc_f1_contextxstartlg.lmer = lmer(f1_norm_bark ~ context_catContrast * context_start_lgContrast + sexContrast - context_catContrast:context_start_lgContrast + (1+context_catContrast + context_start_lgContrast|speaker), data=data_ai_nuc_summ, REML=F)

anova(nuc_f1.lmer, nuc_f1_contextxstartlg.lmer)


## RUN LMER ON F2
# Full model
nuc_f2.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast + sexContrast + (1+context_catContrast * context_start_lgContrast|speaker), data=data_ai_nuc_summ, REML=F)

summary(nuc_f2.lmer)

# Test for effect of context
nuc_f2_context.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast + sexContrast - context_catContrast + (1+context_catContrast * context_start_lgContrast|speaker), data=data_ai_nuc_summ, REML=F)

anova(nuc_f2.lmer, nuc_f2_context.lmer)

# Test for effect of starting language
nuc_f2_startlg.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast + sexContrast - context_start_lgContrast + (1+context_catContrast * context_start_lgContrast|speaker), data=data_ai_nuc_summ, REML=F)

anova(nuc_f2.lmer, nuc_f2_startlg.lmer)

# Test for effect of sex
nuc_f2_sex.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast + sexContrast - sexContrast + (1+context_catContrast * context_start_lgContrast|speaker), data=data_ai_nuc_summ, REML=F)

anova(nuc_f2.lmer, nuc_f2_sex.lmer)

# Test for interaction of context and starting language
nuc_f2_contextxstartlg.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast + sexContrast - context_catContrast:context_start_lgContrast + (1+context_catContrast * context_start_lgContrast|speaker), data=data_ai_nuc_summ, REML=F)

anova(nuc_f2.lmer, nuc_f2_contextxstartlg.lmer)









