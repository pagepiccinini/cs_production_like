## READ IN DATA ####
source("scripts/cs_production_like_cleaning_formants_segments.R")


## LOAD PACKAGES ####
library(lme4)


## ORGANIZE DATA ####
# [l] 
formant_l_stats = formant_l_sum %>%
  # Context (baseline set to ML)
  mutate(context_catContrast = ifelse(context_cat == "ML", -0.5, 0.5)) %>%
  # Language at start of 'like' (baseline set to English)
  mutate(context_start_lgContrast = ifelse(context_start_lg == "E", -0.5, 0.5))

# [ai] - nucleus
formant_ai_nuc_stats = formant_ai_nuc_sum %>%
  # Context (baseline set to ML)
  mutate(context_catContrast = ifelse(context_cat == "ML", -0.5, 0.5)) %>%
  # Language at start of 'like' (baseline set to English)
  mutate(context_start_lgContrast = ifelse(context_start_lg == "E", -0.5, 0.5))

# [ai] - off
formant_ai_off_stats = formant_ai_off_sum %>%
  # Context (baseline set to ML)
  mutate(context_catContrast = ifelse(context_cat == "ML", -0.5, 0.5)) %>%
  # Language at start of 'like' (baseline set to English)
  mutate(context_start_lgContrast = ifelse(context_start_lg == "E", -0.5, 0.5))


## [l]: BUILD MODELS FOR F1 ####
# Full model
l_f1.lmer = lmer(mean_f1_norm_bark ~ context_catContrast * context_start_lgContrast +
                (1+context_catContrast * context_start_lgContrast|speaker),
                data = formant_l_stats, REML=F)

l_f1.lmer_sum = summary(l_f1.lmer)

# Test for effect of context
l_f1_context.lmer = lmer(mean_f1_norm_bark ~ context_catContrast * context_start_lgContrast - context_catContrast +
                        (1+context_catContrast * context_start_lgContrast|speaker),
                        data = formant_l_stats, REML=F)

l_f1_context.anova = anova(l_f1.lmer, l_f1_context.lmer)

# Test for effect of starting language
l_f1_startlg.lmer = lmer(mean_f1_norm_bark ~ context_catContrast * context_start_lgContrast - context_start_lgContrast +
                        (1+context_catContrast * context_start_lgContrast|speaker),
                        data = formant_l_stats, REML=F)

l_f1_startlg.anova = anova(l_f1.lmer, l_f1_startlg.lmer)

# Test for interaction of context and starting language
l_f1_contextxstartlg.lmer = lmer(mean_f1_norm_bark ~ context_catContrast * context_start_lgContrast - context_catContrast:context_start_lgContrast +
                                (1+context_catContrast * context_start_lgContrast|speaker),
                                data = formant_l_stats, REML=F)

l_f1_contextxstartlg.anova = anova(l_f1.lmer, l_f1_contextxstartlg.lmer)


## [l]: BUILD MODELS FOR F2 ####
# Full model
l_f2.lmer = lmer(mean_f2_norm_bark ~ context_catContrast * context_start_lgContrast +
                (1+context_catContrast * context_start_lgContrast|speaker),
                data = formant_l_stats, REML=F)

l_f2.lmer_sum = summary(l_f2.lmer)

# Test for effect of context
l_f2_context.lmer = lmer(mean_f2_norm_bark ~ context_catContrast * context_start_lgContrast - context_catContrast +
                        (1+context_catContrast * context_start_lgContrast|speaker),
                        data = formant_l_stats, REML=F)

l_f2_context.anova = anova(l_f2.lmer, l_f2_context.lmer)

# Test for effect of starting language
l_f2_startlg.lmer = lmer(mean_f2_norm_bark ~ context_catContrast * context_start_lgContrast - context_start_lgContrast +
                        (1+context_catContrast * context_start_lgContrast|speaker),
                        data = formant_l_stats, REML=F)

l_f2_startlg.lmer = anova(l_f2.lmer, l_f2_startlg.lmer)

# Test for interaction of context and starting language
l_f2_contextxstartlg.lmer = lmer(mean_f2_norm_bark ~ context_catContrast * context_start_lgContrast - context_catContrast:context_start_lgContrast +
                                (1+context_catContrast * context_start_lgContrast|speaker),
                                data = formant_l_stats, REML=F)

l_f2_contextxstartlg.lmer = anova(l_f2.lmer, l_f2_contextxstartlg.lmer)


## [ai] - NUCLEUS: BUILD MODELS FOR F1 ####
# Full model
nuc_f1.lmer = lmer(f1_norm_bark ~ context_catContrast * context_start_lgContrast +
                  (1|speaker) +
                  (0+context_catContrast * context_start_lgContrast|speaker),
                  data = formant_ai_nuc_stats, REML=F)

nuc_f1.lmer_sum = summary(nuc_f1.lmer)

# Test for effect of context
nuc_f1_context.lmer = lmer(f1_norm_bark ~ context_catContrast * context_start_lgContrast - context_catContrast +
                          (1|speaker) +
                          (0+context_catContrast * context_start_lgContrast|speaker),
                          data = formant_ai_nuc_stats, REML=F)

nuc_f1_context.anova = anova(nuc_f1.lmer, nuc_f1_context.lmer)

# Test for effect of starting language
nuc_f1_startlg.lmer = lmer(f1_norm_bark ~ context_catContrast * context_start_lgContrast - context_start_lgContrast +
                          (1|speaker) +
                          (0+context_catContrast * context_start_lgContrast|speaker),
                          data = formant_ai_nuc_stats, REML=F)

nuc_f1_startlg.anova = anova(nuc_f1.lmer, nuc_f1_startlg.lmer)

# Test for interaction of context and starting language
nuc_f1_contextxstartlg.lmer = lmer(f1_norm_bark ~ context_catContrast * context_start_lgContrast - context_catContrast:context_start_lgContrast +
                                  (1|speaker) +
                                  (0+context_catContrast + context_start_lgContrast|speaker),
                                  data = formant_ai_nuc_stats, REML=F)

nuc_f1_contextxstartlg.anova = anova(nuc_f1.lmer, nuc_f1_contextxstartlg.lmer)


## [ai] - NUCLEUS: BUILD MODELS FOR F2 ####
# Full model
nuc_f2.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast +
                  (1|speaker) +
                  (0+context_catContrast * context_start_lgContrast|speaker),
                  data = formant_ai_nuc_stats, REML=F)

nuc_f2.lmer_sum = summary(nuc_f2.lmer)

# Test for effect of context
nuc_f2_context.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast - context_catContrast +
                          (1|speaker) +
                          (0+context_catContrast * context_start_lgContrast|speaker),
                          data = formant_ai_nuc_stats, REML=F)

nuc_f2_context.anova = anova(nuc_f2.lmer, nuc_f2_context.lmer)

# Test for effect of starting language
nuc_f2_startlg.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast - context_start_lgContrast +
                          (1|speaker) +
                          (0+context_catContrast * context_start_lgContrast|speaker),
                          data = formant_ai_nuc_stats, REML=F)

nuc_f2_startlg.anova = anova(nuc_f2.lmer, nuc_f2_startlg.lmer)

# Test for interaction of context and starting language
nuc_f2_contextxstartlg.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast - context_catContrast:context_start_lgContrast +
                                  (1|speaker) +
                                  (0+context_catContrast * context_start_lgContrast|speaker),
                                  data = formant_ai_nuc_stats, REML=F)

nuc_f2_contextxstartlg.anova = anova(nuc_f2.lmer, nuc_f2_contextxstartlg.lmer)


## [ai] - OFFGLIDE: BUILD MODELS FOR F1 ####
# Full model
off_f1.lmer = lmer(f1_norm_bark ~ context_catContrast * context_start_lgContrast +
                  (1+context_start_lgContrast|speaker),
                  data = formant_ai_off_stats, REML=F)

off_f1.lmer_sum = summary(off_f1.lmer)

# Test for effect of context
off_f1_context.lmer = lmer(f1_norm_bark ~ context_catContrast * context_start_lgContrast - context_catContrast +
                          (1+context_start_lgContrast|speaker),
                          data = formant_ai_off_stats, REML=F)

off_f1_context.anova = anova(off_f1.lmer, off_f1_context.lmer)

# Test for effect of starting language
off_f1_startlg.lmer = lmer(f1_norm_bark ~ context_catContrast * context_start_lgContrast - context_start_lgContrast +
                          (1+context_start_lgContrast|speaker),
                          data = formant_ai_off_stats, REML=F)

off_f1_startlg.anova = anova(off_f1.lmer, off_f1_startlg.lmer)

# Test for interaction of context and starting language
off_f1_contextxstartlg.lmer = lmer(f1_norm_bark ~ context_catContrast * context_start_lgContrast - context_catContrast:context_start_lgContrast +
                                  (1+context_start_lgContrast|speaker),
                                  data = formant_ai_off_stats, REML=F)

off_f1_contextxstartlg.anova = anova(off_f1.lmer, off_f1_contextxstartlg.lmer)


## [ai] - OFFGLIDE: BUILD MODELS FOR F2 ####
# Full model
off_f2.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast +
                  (1+context_start_lgContrast|speaker),
                  data = formant_ai_off_stats, REML=F)

off_f2.lmer_sum = summary(off_f2.lmer)

# Test for effect of context
off_f2_context.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast - context_catContrast +
                          (1+context_start_lgContrast|speaker),
                          data = formant_ai_off_stats, REML=F)

off_f2_context.anova = anova(off_f2.lmer, off_f2_context.lmer)

# Test for effect of starting language
off_f2_startlg.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast - context_start_lgContrast +
                          (1+context_start_lgContrast|speaker),
                          data = formant_ai_off_stats, REML=F)

off_f2_startlg.anova = anova(off_f2.lmer, off_f2_startlg.lmer)

# Test for interaction of context and starting language
off_f2_contextxstartlg.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast - context_catContrast:context_start_lgContrast +
                                  (1+context_start_lgContrast|speaker),
                                  data = formant_ai_off_stats, REML=F)

off_f2_contextxstartlg.anova = anova(off_f2.lmer, off_f2_contextxstartlg.lmer)




