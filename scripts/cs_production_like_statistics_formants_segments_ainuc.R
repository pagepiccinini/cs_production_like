## READ IN DATA ####
source("scripts/cs_production_like_cleaning_formants_segments.R")


## ORGANIZE DATA ####
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


## RUN LMER ON F1 ####
# Full model
nuc_f1.lmer = lmer(f1_norm_bark ~ context_catContrast * context_start_lgContrast +
                     (1|speaker) +
                     (0+context_catContrast * context_start_lgContrast|speaker), data=data_ai_nuc_summ, REML=F)

nuc_f1.lmer_sum = summary(nuc_f1.lmer)

# Test for effect of context
nuc_f1_context.lmer = lmer(f1_norm_bark ~ context_catContrast * context_start_lgContrast - context_catContrast +
                           (1|speaker) +
                           (0+context_catContrast * context_start_lgContrast|speaker), data=data_ai_nuc_summ, REML=F)

nuc_f1_context.anova = anova(nuc_f1.lmer, nuc_f1_context.lmer)

# Test for effect of starting language
nuc_f1_startlg.lmer = lmer(f1_norm_bark ~ context_catContrast * context_start_lgContrast - context_start_lgContrast +
                             (1|speaker) +
                             (0+context_catContrast * context_start_lgContrast|speaker), data=data_ai_nuc_summ, REML=F)

nuc_f1_startlg.anova = anova(nuc_f1.lmer, nuc_f1_startlg.lmer)

# Test for interaction of context and starting language
nuc_f1_contextxstartlg.lmer = lmer(f1_norm_bark ~ context_catContrast * context_start_lgContrast - context_catContrast:context_start_lgContrast +
                                     (1|speaker) +
                                     (0+context_catContrast + context_start_lgContrast|speaker), data=data_ai_nuc_summ, REML=F)

nuc_f1_contextxstartlg.anova = anova(nuc_f1.lmer, nuc_f1_contextxstartlg.lmer)


## RUN LMER ON F2 ####
# Full model
nuc_f2.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast +
                     (1|speaker) +
                     (0+context_catContrast * context_start_lgContrast|speaker), data=data_ai_nuc_summ, REML=F)

nuc_f2.lmer_sum = summary(nuc_f2.lmer)

# Test for effect of context
nuc_f2_context.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast - context_catContrast +
                             (1|speaker) +
                             (0+context_catContrast * context_start_lgContrast|speaker), data=data_ai_nuc_summ, REML=F)

nuc_f2_context.anova = anova(nuc_f2.lmer, nuc_f2_context.lmer)

# Test for effect of starting language
nuc_f2_startlg.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast - context_start_lgContrast +
                             (1|speaker) +
                             (0+context_catContrast * context_start_lgContrast|speaker), data=data_ai_nuc_summ, REML=F)

nuc_f2_startlg.anova = anova(nuc_f2.lmer, nuc_f2_startlg.lmer)

# Test for interaction of context and starting language
nuc_f2_contextxstartlg.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast - context_catContrast:context_start_lgContrast +
                                     (1|speaker) +
                                     (0+context_catContrast * context_start_lgContrast|speaker), data=data_ai_nuc_summ, REML=F)

nuc_f2_contextxstartlg.anova = anova(nuc_f2.lmer, nuc_f2_contextxstartlg.lmer)









