## READ IN DATA ####
source("scripts/cs_production_like_cleaning_formants.R")


## LOAD PACKAGES ####
library(lme4)


## ORGANIZE DATA ####
formant_stats = formant_noout %>%
  # Context (baseline set to ML)
  mutate(context_catContrast = ifelse(context_cat == "ML", -0.5, 0.5)) %>%
  # Language at start of 'like' (baseline set to English)
  mutate(context_start_lgContrast = ifelse(context_start_lg == "E", -0.5, 0.5))
  


## BULD MODELS FOR F1 ####
# Full model
f1.lmer = lmer(f1_norm_bark ~ context_catContrast * context_start_lgContrast * percentage +
              (1+context_catContrast+context_start_lgContrast|speaker),
              data = formant_stats, REML=F)

f1.lmer_sum = summary(f1.lmer)

# Test for effect of context
f1_context.lmer = lmer(f1_norm_bark ~ context_catContrast * context_start_lgContrast * percentage - context_catContrast +
                      (1+context_catContrast+context_start_lgContrast|speaker),
                      data = formant_stats, REML=F)

f1_context.anova = anova(f1.lmer, f1_context.lmer)

# Test for effect of starting language
f1_startlg.lmer = lmer(f1_norm_bark ~ context_catContrast * context_start_lgContrast * percentage - context_start_lgContrast +
                      (1+context_catContrast+context_start_lgContrast|speaker),
                      data = formant_stats, REML=F)

f1_startlg.anova = anova(f1.lmer, f1_startlg.lmer)

# Test for effect of percentage
f1_percentage.lmer = lmer(f1_norm_bark ~ context_catContrast * context_start_lgContrast * percentage - percentage +
                         (1+context_catContrast+context_start_lgContrast|speaker),
                         data = formant_stats, REML=F)

f1_percentage.anova = anova(f1.lmer, f1_percentage.lmer)

# Test for interaction of context and starting language
f1_contextxstartlg.lmer = lmer(f1_norm_bark ~ context_catContrast * context_start_lgContrast * percentage - context_catContrast:context_start_lgContrast +
                              (1+context_catContrast+context_start_lgContrast|speaker),
                              data = formant_stats, REML=F)

f1_contextxstartlg.anova = anova(f1.lmer, f1_contextxstartlg.lmer)

# Test for interaction of context and percentage
f1_contextxpercentage.lmer = lmer(f1_norm_bark ~ context_catContrast * context_start_lgContrast * percentage - context_catContrast:percentage +
                                 (1+context_catContrast+context_start_lgContrast|speaker),
                                 data = formant_stats, REML=F)

f1_contextxpercentage.anova = anova(f1.lmer, f1_contextxpercentage.lmer)

# Test for interaction of starting language and percentage
f1_startlgxpercentage.lmer = lmer(f1_norm_bark ~ context_catContrast * context_start_lgContrast * percentage - context_start_lgContrast:percentage +
                                 (1+context_catContrast+context_start_lgContrast|speaker),
                                 data = formant_stats, REML=F)

f1_startlgxpercentage.anova = anova(f1.lmer, f1_startlgxpercentage.lmer)

# Test for interaction of context, starting language, and percentage
f1_contextxstartlgxpercentage.lmer = lmer(f1_norm_bark ~ context_catContrast * context_start_lgContrast * percentage - context_catContrast:context_start_lgContrast:percentage +
                                         (1+context_catContrast+context_start_lgContrast|speaker),
                                         data = formant_stats, REML=F)

f1_contextxstartlgxpercentage.anova = anova(f1.lmer, f1_contextxstartlgxpercentage.lmer)


## FOLLOW UP REGRESSIONS FOR F1 CONTEXT X STARTING LANGUAGE x PERCENTAGE INTERACTION ####
# Organize data
f1_eng = formant_stats %>%
  filter(context_start_lg == "E") %>%
	mutate(context_start_lg = factor(context_start_lg))
	       
f1_eng_ml = f1_eng %>%
  filter(context_cat == "ML") %>%
	mutate(context_cat = factor(context_cat))

f1_eng_cs = f1_eng %>%
  filter(context_cat == "CS") %>%
	mutate(context_cat = factor(context_cat))

f1_sp = formant_stats %>%
  filter(context_start_lg == "S") %>%
	mutate(context_start_lg = factor(context_start_lg))

f1_sp_ml = f1_sp %>%
  filter(context_cat == "ML") %>%
	mutate(context_cat = factor(context_cat))

f1_sp_cs = f1_sp %>%
  filter(context_cat == "CS") %>%
	mutate(context_cat = factor(context_cat))

# Run regressions
f1_eng_ml.lm = lm(f1_norm_bark ~ percentage, f1_eng_ml)
f1_eng_ml.lm_sum = summary(f1_eng_ml.lm)

f1_eng_cs.lm = lm(f1_norm_bark ~ percentage, f1_eng_cs)
f1_eng_cs.lm_sum = summary(f1_eng_cs.lm)

f1_sp_ml.lm = lm(f1_norm_bark ~ percentage, f1_sp_ml)
f1_sp_ml.lm_sum = summary(f1_sp_ml.lm)

f1_sp_cs.lm = lm(f1_norm_bark ~ percentage, f1_sp_cs)
f1_sp_cs.lm_sum = summary(f1_sp_cs.lm)


## BUILD MODEL FOR F2 ####
# Full model
f2.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast * percentage +
              (1+context_catContrast*context_start_lgContrast|speaker),
              data = formant_stats, REML=F)

f2.lmer_sum = summary(f2.lmer)

# Test for effect of context
f2_context.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast * percentage - context_catContrast +
                      (1+context_catContrast*context_start_lgContrast|speaker),
                      data = formant_stats, REML=F)

f2_context.anova = anova(f2.lmer, f2_context.lmer)

# Test for effect of starting language
f2_startlg.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast * percentage - context_start_lgContrast +
                      (1+context_catContrast*context_start_lgContrast|speaker),
                      data = formant_stats, REML=F)

f2_startlg.anova = anova(f2.lmer, f2_startlg.lmer)

# Test for effect of percentage
f2_percentage.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast * percentage - percentage +
                         (1+context_catContrast*context_start_lgContrast|speaker),
                         data = formant_stats, REML=F)

f2_percentage.anova = anova(f2.lmer, f2_percentage.lmer)

# Test for interaction of context and starting language
f2_contextxstartlg.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast * percentage - context_catContrast:context_start_lgContrast +
                              (1+context_catContrast*context_start_lgContrast|speaker),
                              data = formant_stats, REML=F)

f2_contextxstartlg.anova = anova(f2.lmer, f2_contextxstartlg.lmer)

# Test for interaction of context and percentage
f2_contextxpercentage.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast * percentage - context_catContrast:percentage +
                                 (1+context_catContrast*context_start_lgContrast|speaker),
                                 data = formant_stats, REML=F)

f2_contextxpercentage.anova = anova(f2.lmer, f2_contextxpercentage.lmer)

# Test for interaction of starting language and percentage
f2_startlgxpercentage.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast * percentage - context_start_lgContrast:percentage +
                                 (1+context_catContrast*context_start_lgContrast|speaker),
                                 data = formant_stats, REML=F)

f2_startlgxpercentage.anova = anova(f2.lmer, f2_startlgxpercentage.lmer)

# Test for interaction of context, starting language, and percentage
f2_contextxstartlgxpercentage.lmer = lmer(f2_norm_bark ~ context_catContrast * context_start_lgContrast * percentage - context_catContrast:context_start_lgContrast:percentage +
                                         (1+context_catContrast*context_start_lgContrast|speaker),
                                         data = formant_stats, REML=F)

f2_contextxstartlgxpercentage.anova = anova(f2.lmer, f2_contextxstartlgxpercentage.lmer)


## FOLLOW UP REGRESSIONS FOR F2 CONTEXT X STARTING LANGUAGE x PERCENTAGE INTERACTION ####
# Organize data
f2_eng = formant_stats %>%
  filter(context_start_lg == "E") %>%
  mutate(context_start_lg = factor(context_start_lg))
         
f2_eng_ml = f2_eng %>%
  filter(context_cat == "ML") %>%
  mutate(context_cat = factor(context_cat))

f2_eng_cs = f2_eng %>%
  filter(context_cat == "CS") %>%
  mutate(context_cat = factor(context_cat))

f2_sp = formant_stats %>%
  filter(context_start_lg == "S") %>%
  mutate(context_start_lg = factor(context_start_lg))

f2_sp_ml = f2_sp %>%
  filter(context_cat == "ML") %>%
  mutate(context_cat = factor(context_cat))

f2_sp_cs = f2_sp %>%
  filter(context_cat == "CS") %>%
  mutate(context_cat = factor(context_cat))

# Run regressions
f2_eng_ml.lm = lm(f2_norm_bark ~ percentage, f2_eng_ml)
f2_eng_ml.lm_sum = summary(f2_eng_ml.lm)

f2_eng_cs.lm = lm(f2_norm_bark ~ percentage, f2_eng_cs)
f2_eng_cs.lm_sum = summary(f2_eng_cs.lm)

f2_sp_ml.lm = lm(f2_norm_bark ~ percentage, f2_sp_ml)
f2_sp_ml.lm_sum = summary(f2_sp_ml.lm)

f2_sp_cs.lm = lm(f2_norm_bark ~ percentage, f2_sp_cs)
f2_sp_cs.lm_sum = summary(f2_sp_cs.lm)



















