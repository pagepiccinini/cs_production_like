## READ IN DATA ####
source("scripts/cs_production_like_cleaning_formants_segments.R")


## ORGANIZE DATA ####
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


## RUN LMER ON F1 ####
# Full model
l_f1.lmer = lmer(mean_f1_norm_bark ~ context_catContrast * context_start_lgContrast +
                   (1+context_catContrast * context_start_lgContrast|speaker), data=data_l_summ, REML=F)

l_f1.lmer_sum = summary(l_f1.lmer)

# Test for effect of context
l_f1_context.lmer = lmer(mean_f1_norm_bark ~ context_catContrast * context_start_lgContrast - context_catContrast +
                           (1+context_catContrast * context_start_lgContrast|speaker), data=data_l_summ, REML=F)

l_f1_context.anova = anova(l_f1.lmer, l_f1_context.lmer)

# Test for effect of starting language
l_f1_startlg.lmer = lmer(mean_f1_norm_bark ~ context_catContrast * context_start_lgContrast - context_start_lgContrast +
                           (1+context_catContrast * context_start_lgContrast|speaker), data=data_l_summ, REML=F)

l_f1_startlg.anova = anova(l_f1.lmer, l_f1_startlg.lmer)

# Test for interaction of context and starting language
l_f1_contextxstartlg.lmer = lmer(mean_f1_norm_bark ~ context_catContrast * context_start_lgContrast - context_catContrast:context_start_lgContrast +
                                   (1+context_catContrast * context_start_lgContrast|speaker), data=data_l_summ, REML=F)

l_f1_contextxstartlg.anova = anova(l_f1.lmer, l_f1_contextxstartlg.lmer)


## RUN LMER ON F2 ####
# Full model
l_f2.lmer = lmer(mean_f2_norm_bark ~ context_catContrast * context_start_lgContrast +
                   (1+context_catContrast * context_start_lgContrast|speaker), data=data_l_summ, REML=F)

l_f2.lmer_sum = summary(l_f2.lmer)

# Test for effect of context
l_f2_context.lmer = lmer(mean_f2_norm_bark ~ context_catContrast * context_start_lgContrast - context_catContrast +
                           (1+context_catContrast * context_start_lgContrast|speaker), data=data_l_summ, REML=F)

l_f2_context.anova = anova(l_f2.lmer, l_f2_context.lmer)

# Test for effect of starting language
l_f2_startlg.lmer = lmer(mean_f2_norm_bark ~ context_catContrast * context_start_lgContrast - context_start_lgContrast +
                           (1+context_catContrast * context_start_lgContrast|speaker), data=data_l_summ, REML=F)

l_f2_startlg.lmer = anova(l_f2.lmer, l_f2_startlg.lmer)

# Test for interaction of context and starting language
l_f2_contextxstartlg.lmer = lmer(mean_f2_norm_bark ~ context_catContrast * context_start_lgContrast - context_catContrast:context_start_lgContrast +
                                   (1+context_catContrast * context_start_lgContrast|speaker), data=data_l_summ, REML=F)

l_f2_contextxstartlg.lmer = anova(l_f2.lmer, l_f2_contextxstartlg.lmer)









