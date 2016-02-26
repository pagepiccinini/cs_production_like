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


## CLOSURE PRESENCE: BUILD MODELS ####
# Full model
clos.glmer = glmer(closure_presence ~ context_catContrast * context_start_lgContrast +
                  (1+context_catContrast|speaker),
                  data = dur_stats, family="binomial")

clos.glmer_sum = summary(clos.glmer)

# Test for effect of context
clos_context.glmer = glmer(closure_presence ~ context_catContrast * context_start_lgContrast - context_catContrast +
                          (1+context_catContrast|speaker),
                          data = dur_stats, family="binomial")

clos_context.anova = anova(clos.glmer, clos_context.glmer)

# Test for effect of starting language
clos_startlg.glmer = glmer(closure_presence ~ context_catContrast * context_start_lgContrast - context_start_lgContrast +
                          (1+context_catContrast|speaker),
                          data = dur_stats, family="binomial")

clos_startlg.anova = anova(clos.glmer, clos_startlg.glmer)

# Test for interaction of context and starting language
clos_contextxstartlg.glmer = glmer(closure_presence ~ context_catContrast * context_start_lgContrast - context_catContrast:context_start_lgContrast +
                                  (1+context_catContrast|speaker),
                                  data = dur_stats, family="binomial")

clos_contextxstartlg.anova = anova(clos.glmer, clos_contextxstartlg.glmer)


## CLOSURE PRESENCE: FOLLOW-UP REGRESSIONS FOR CONTEXT X START LG. INTERACTION ####
# English versus Spanish for both contexts
dur_ml_stats = dur_stats %>%
  filter(context_cat == "ML") %>%
  mutate(context_cat = factor(context_cat))

dur_cs_stats = dur_stats %>%
  filter(context_cat == "CS") %>%
  mutate(context_cat = factor(context_cat))

clos_ml.glm = glm(closure_presence ~ context_start_lgContrast, dur_ml_stats, family="binomial")
clos_ml.glm_sum = summary(clos_ml.glm)

clos_cs.glm = glm(closure_presence ~ context_start_lgContrast, dur_cs_stats, family="binomial")
clos_cs.glm_sum = summary(clos_cs.glm)

# Monolingual versus code-switching for both languages
dur_e_stats = dur_stats %>%
  filter(context_start_lg == "E") %>%
  mutate(context_start_lg = factor(context_start_lg))

dur_s_stats = dur_stats %>%
  filter(context_start_lg == "S") %>%
  mutate(context_start_lg = factor(context_start_lg))
	
clos_e.glm = glm(closure_presence ~ context_cat, dur_e_stats, family="binomial")
clos_e.glm_sum = summary(clos_e.glm)

clos_s.glm = glm(closure_presence ~ context_cat, dur_s_stats, family="binomial")
clos_s.glm_sum = summary(clos_s.glm)


## BURST PRESENCE: BUILD MODELS ####
# Full model
burst.glmer = glmer(burst_presence ~ context_catContrast * context_start_lgContrast + taskContrast + gram_catContrast +
                   (1+context_catContrast+taskContrast|speaker),
                   data = dur_stats, family="binomial")

burst.glmer_sum = summary(burst.glmer)

# Test for effect of context
burst_context.glmer = glmer(burst_presence ~ context_catContrast * context_start_lgContrast + taskContrast + gram_catContrast - context_catContrast +
                           (1+context_catContrast+taskContrast|speaker),
                           data = dur_stats, family="binomial")

burst_context.anova = anova(burst.glmer, burst_context.glmer)

# Test for effect of starting language
burst_startlg.glmer = glmer(burst_presence ~ context_catContrast * context_start_lgContrast + taskContrast + gram_catContrast - context_start_lgContrast +
                           (1+context_catContrast+taskContrast|speaker),
                           data = dur_stats, family="binomial")

burst_startlg.anova = anova(burst.glmer, burst_startlg.glmer)

# Test for effect of task
burst_task.glmer = glmer(burst_presence ~ context_catContrast * context_start_lgContrast + taskContrast + gram_catContrast - taskContrast +
                        (1+context_catContrast+taskContrast|speaker),
                        data = dur_stats, family="binomial")

burst_task.anova = anova(burst.glmer, burst_task.glmer)

# Test for effect of grammatical category
burst_gramcat.glmer = glmer(burst_presence ~ context_catContrast * context_start_lgContrast + taskContrast + gram_catContrast - gram_catContrast +
                           (1+context_catContrast+taskContrast|speaker),
                           data = dur_stats, family="binomial")

burst_gramcat.anova = anova(burst.glmer, burst_gramcat.glmer)

# Test for interaction of context and starting language
burst_contextxstartlg.glmer = glmer(burst_presence ~ context_catContrast * context_start_lgContrast + taskContrast + gram_catContrast - context_catContrast:context_start_lgContrast +
                                   (1+context_catContrast+taskContrast|speaker),
                                   data = dur_stats, family="binomial")

burst_contextxstartlg.anova = anova(burst.glmer, burst_contextxstartlg.glmer)





