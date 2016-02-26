## READ IN DATA ####
source("scripts/cs_production_like_cleaning_duration.R")


## LOAD PACKAGES ####
library(lme4)
	
	
## ORGANIZE DATA ####
dur_stats = dur_clean %>%
  # Context (baseline set to ML)
  mutate(context_catContrast = ifelse(context_cat == "ML", -0.5, 0.5)) %>%
  # Language at start of 'like' (baseline set to English)
  mutate(context_start_lgContrast = ifelse(context_start_lg == "E", -0.5, 0.5)) %>%
  # Task category (baseline set to c)
  mutate(taskContrast = ifelse(task == "C", -0.5, 0.5)) %>%
  # Grammatical category (baseline set to D)
  mutate(gram_catContrast = ifelse(gram_cat == "D", -0.5, 0.5))

# Subset out data for closure analysis
dur_clos_stats = dur_stats %>%
  filter(closure_duration > 0)

# Subset out data for burst analysis
dur_burst_stats = dur_stats %>%
  filter(burst_duration > 0)


## /lai/ DURATION: BUILD MODELS ####
# Full model
lai.lmer = lmer(like_duration_ms ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast +
               (1+context_catContrast+context_start_lgContrast+taskContrast|speaker),
               data = dur_stats, REML=F)

lai.lmer_sum = summary(lai.lmer)

# Test for effect of context
lai_context.lmer = lmer(like_duration_ms ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - context_catContrast +
                       (1+context_catContrast+context_start_lgContrast+taskContrast|speaker),
                       data = dur_stats, REML=F)

lai_context.anova = anova(lai.lmer, lai_context.lmer)

# Test for effect of starting language
lai_startlg.lmer = lmer(like_duration_ms ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - context_start_lgContrast +
                       (1+context_catContrast+context_start_lgContrast+taskContrast|speaker),
                       data = dur_stats, REML=F)

lai_startlg.anova = anova(lai.lmer, lai_startlg.lmer)

# Test for effect of task
lai_task.lmer = lmer(like_duration_ms ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - taskContrast +
                    (1+context_catContrast+context_start_lgContrast+taskContrast|speaker),
                    data = dur_stats, REML=F)

lai_task.anova = anova(lai.lmer, lai_task.lmer)

# Test for effect of grammatical category
lai_gramcat.lmer = lmer(like_duration_ms ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - gram_catContrast +
                       (1+context_catContrast+context_start_lgContrast+taskContrast|speaker),
                       data = dur_stats, REML=F)

lai_gramcat.anova = anova(lai.lmer, lai_gramcat.lmer)

# Test for interaction of context and starting language
lai_contextxstartlg.lmer = lmer(like_duration_ms ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - context_catContrast:context_start_lgContrast +
                               (1+context_catContrast+context_start_lgContrast+taskContrast|speaker),
                               data = dur_stats, REML=F)

lai_contextxstartlg.anova = anova(lai.lmer, lai_contextxstartlg.lmer)

# Test for interaction of context and task
lai_contextxtask.lmer = lmer(like_duration_ms ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - context_catContrast:taskContrast +
                            (1+context_catContrast+context_start_lgContrast+taskContrast|speaker),
                            data = dur_stats, REML=F)

lai_contextxtask.anova = anova(lai.lmer, lai_contextxtask.lmer)

# Test for interaction of starting language and task
lai_startlgxtask.lmer = lmer(like_duration_ms ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - context_start_lgContrast:taskContrast +
                            (1+context_catContrast+context_start_lgContrast+taskContrast|speaker),
                            data = dur_stats, REML=F)

lai_startlgxtask.anova = anova(lai.lmer, lai_startlgxtask.lmer)

# Test for interaction of context, starting language, and task
lai_contextxstartlgxtask.lmer = lmer(like_duration_ms ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - context_catContrast:context_start_lgContrast:taskContrast +
                                    (1+context_catContrast+context_start_lgContrast+taskContrast|speaker),
                                    data = dur_stats, REML=F)

lai_contextxstartlgxtask.anova = anova(lai.lmer, lai_contextxstartlgxtask.lmer)


## /lai/ DURATION: FOLLOW-UP REGRESSIONS FOR CONTEXT X START LANGUAGE INTERACTION ####
# English versus Spanish for both contexts
dur_ml_stats = dur_stats %>%
  filter(context_cat == "ML") %>%
	mutate(context_cat = factor(context_cat))

dur_cs_stats = dur_stats %>%
  filter(context_cat == "CS") %>%
	mutate(context_cat = factor(context_cat))

lai_ml.lm = lm(like_duration_ms ~ context_start_lgContrast, dur_ml_stats)
lai_ml.lm_sum = summary(lai_ml.lm)

lai_cs.lm = lm(like_duration_ms ~ context_start_lgContrast, dur_cs_stats)
lai_cs.lm_sum = summary(lai_cs.lm)

# Monolingual versus code-switching for both languages
dur_e_stats = dur_stats %>%
  filter(context_start_lg == "E") %>%
	mutate(context_start_lg = factor(context_start_lg))

dur_s_stats = dur_stats %>%
  filter(context_start_lg == "S") %>%
	mutate(context_start_lg = factor(context_start_lg))

lai_e.lm = lm(like_duration_ms ~ context_catContrast, dur_e_stats)
lai_e.lm_sum = summary(lai_e.lm)

lai_s.lm = lm(like_duration_ms ~ context_catContrast, dur_s_stats)
lai_s.lm_sum = summary(lai_s.lm)


## CLOSURE DURATION: BUILD MODELS ####
# Full model
clos.lmer = lmer(closure_duration ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast +
                (1+context_catContrast+context_start_lgContrast+taskContrast+gram_catContrast|speaker),
                data = dur_clos_stats, REML=F)

clos.lmer_sum = summary(clos.lmer)

# Test for effect of context
clos_context.lmer = lmer(closure_duration ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - context_catContrast +
                             (1+context_catContrast+context_start_lgContrast+taskContrast+gram_catContrast|speaker),
                             data = dur_clos_stats, REML=F)

clos_context.anova = anova(clos.lmer, clos_context.lmer)

# Test for effect of starting language
clos_startlg.lmer = lmer(closure_duration ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - context_start_lgContrast +
                             (1+context_catContrast+context_start_lgContrast+taskContrast+gram_catContrast|speaker),
                             data = dur_clos_stats, REML=F)

clos_startlg.anova = anova(clos.lmer, clos_startlg.lmer)

# Test for effect of task
clos_task.lmer = lmer(closure_duration ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - taskContrast +
                          (1+context_catContrast+context_start_lgContrast+taskContrast+gram_catContrast|speaker),
                          data = dur_clos_stats, REML=F)

clos_task.anova = anova(clos.lmer, clos_task.lmer)

# Test for effect of grammatical category
clos_gramcat.lmer = lmer(closure_duration ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - gram_catContrast +
                             (1+context_catContrast+context_start_lgContrast+taskContrast+gram_catContrast|speaker),
                             data = dur_clos_stats, REML=F)

clos_gramcat.anova = anova(clos.lmer, clos_gramcat.lmer)

# Test for interaction of context and starting language
clos_contextxstartlg.lmer = lmer(closure_duration ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - context_catContrast:context_start_lgContrast +
                                     (1+context_catContrast+context_start_lgContrast+taskContrast+gram_catContrast|speaker),
                                     data = dur_clos_stats, REML=F)

clos_contextxstartlg.anova = anova(clos.lmer, clos_contextxstartlg.lmer)

# Test for interaction of context and task
clos_contextxtask.lmer = lmer(closure_duration ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - context_catContrast:taskContrast +
                                  (1+context_catContrast+context_start_lgContrast+taskContrast+gram_catContrast|speaker),
                                  data = dur_clos_stats, REML=F)

clos_contextxtask.anova = anova(clos.lmer, clos_contextxtask.lmer)

# Test for interaction of starting language and task
clos_startlgxtask.lmer = lmer(closure_duration ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - context_start_lgContrast:taskContrast +
                             (1+context_catContrast+context_start_lgContrast+taskContrast+gram_catContrast|speaker),
                             data = dur_clos_stats, REML=F)

clos_startlgxtask.anova = anova(clos.lmer, clos_startlgxtask.lmer)

# Test for interaction of context, starting language, and task
clos_contextxstartlgxtask.lmer = lmer(closure_duration ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - context_catContrast:context_start_lgContrast:taskContrast +
                                     (1+context_catContrast+context_start_lgContrast+taskContrast+gram_catContrast|speaker),
                                     data = dur_clos_stats, REML=F)

clos_contextxstartlgxtask.anova = anova(clos.lmer, clos_contextxstartlgxtask.lmer)


## CLOSURE DURATION: FOLLOW-UP REGRESSIONS FOR CONTEXT X START LANGAUGE INTERACTION ####
# English versus Spanish for both contexts
dur_clos_ml_stats = dur_clos_stats %>%
  filter(context_cat == "ML") %>%
  mutate(context_cat = factor(context_cat))

dur_clos_cs_stats = dur_clos_stats %>%
  filter(context_cat == "CS") %>%
  mutate(context_cat = factor(context_cat))

clos_ml.lm = lm(closure_duration ~ context_start_lgContrast, dur_clos_ml_stats)
clos_ml.lm_sum = summary(clos_ml.lm)

clos_cs.lm = lm(closure_duration ~ context_start_lgContrast, dur_clos_cs_stats)
clos_cs.lm_sum = summary(clos_cs.lm)

# Monolingual versus code-switching for both languages
dur_clos_e_stats = dur_clos_stats %>%
  filter(context_start_lg == "E") %>%
  mutate(context_start_lg = factor(context_start_lg))

dur_clos_s_stats = dur_clos_stats %>%
  filter(context_start_lg == "S") %>%
  mutate(context_start_lg = factor(context_start_lg))

clos_e.lm = lm(closure_duration ~ context_catContrast, dur_clos_e_stats)
clos_e.lm_sum = summary(clos_e.lm)

clos_s.lm = lm(closure_duration ~ context_catContrast, dur_clos_s_stats)
clos_s.lm_sum = summary(clos_s.lm)


## BURST DURATION: BUILD MODELS ####
# Full model
burst.lmer = lmer(burst_duration ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast +
                 (1+context_catContrast+context_start_lgContrast+taskContrast+gram_catContrast|speaker),
                 data = dur_burst_stats, REML=F)

burst.lmer_sum = summary(burst.lmer)

# Test for effect of context
burst_context.lmer = lmer(burst_duration ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - context_catContrast +
                         (1+context_catContrast+context_start_lgContrast+taskContrast+gram_catContrast|speaker),
                         data = dur_burst_stats, REML=F)

burst_context.anova = anova(burst.lmer, burst_context.lmer)

# Test for effect of starting language
burst_startlg.lmer = lmer(burst_duration ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - context_start_lgContrast +
                         (1+context_catContrast+context_start_lgContrast+taskContrast+gram_catContrast|speaker),
                         data = dur_burst_stats, REML=F)

burst_startlg.anova = anova(burst.lmer, burst_startlg.lmer)

# Test for effect of task
burst_task.lmer = lmer(burst_duration ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - taskContrast +
                      (1+context_catContrast+context_start_lgContrast+taskContrast+gram_catContrast|speaker),
                      data = dur_burst_stats, REML=F)

burst_task.anova = anova(burst.lmer, burst_task.lmer)

# Test for effect of grammatical category
burst_gramcat.lmer = lmer(burst_duration ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - gram_catContrast +
                         (1+context_catContrast+context_start_lgContrast+taskContrast+gram_catContrast|speaker),
                         data = dur_burst_stats, REML=F)

burst_gramcat.anova = anova(burst.lmer, burst_gramcat.lmer)

# Test for interaction of context and starting language
burst_contextxstartlg.lmer = lmer(burst_duration ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - context_catContrast:context_start_lgContrast +
                                 (1+context_catContrast+context_start_lgContrast+taskContrast+gram_catContrast|speaker),
                                 data = dur_burst_stats, REML=F)

burst_contextxstartlg.anova = anova(burst.lmer, burst_contextxstartlg.lmer)

# Test for interaction of context and task
burst_contextxtask.lmer = lmer(burst_duration ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - context_catContrast:taskContrast +
                              (1+context_catContrast+context_start_lgContrast+taskContrast+gram_catContrast|speaker),
                              data = dur_burst_stats, REML=F)

burst_contextxtask.anova = anova(burst.lmer, burst_contextxtask.lmer)

# Test for interaction of starting language and task
burst_startlgxtask.lmer = lmer(burst_duration ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - context_start_lgContrast:taskContrast +
                              (1+context_catContrast+context_start_lgContrast+taskContrast+gram_catContrast|speaker),
                              data = dur_burst_stats, REML=F)

burst_startlgxtask.anova = anova(burst.lmer, burst_startlgxtask.lmer)

# Test for interaction of context, starting language, and task
burst_contextxstartlgxtask.lmer = lmer(burst_duration ~ context_catContrast * context_start_lgContrast * taskContrast + gram_catContrast - context_catContrast:context_start_lgContrast:taskContrast +
                                      (1+context_catContrast+context_start_lgContrast+taskContrast+gram_catContrast|speaker),
                                      data = dur_burst_stats, REML=F)

burst_contextxstartlgxtask.anova = anova(burst.lmer, burst_contextxstartlgxtask.lmer)

















