## ORGAINZIE DATA
# Set working directory for source file and read in file for organizing data
setwd("~/Desktop/Experiments/CS E-S Production - like/Results/scripts/")
source("data_organization.R")

data_clos = data_sub	
	
## PREPARE VARAIBLES FOR glmer
# Context (baseline set to ML)
contrasts(data_clos$context_cat) = c(0.5, -0.5)
data_clos$context_catContrast = contrasts(data_clos$context_cat)[,1][as.numeric(data_clos$context_cat)]

# Language at start of 'like' (baseline set to English)
contrasts(data_clos$context_start_lg) = c(-0.5, 0.5)
data_clos$context_start_lgContrast = contrasts(data_clos$context_start_lg)[,1][as.numeric(data_clos$context_start_lg)]

# Task category (baseline set to c)
contrasts(data_clos$task) = c(-0.5, 0.5)
data_clos$taskContrast = contrasts(data_clos$task)[,1][as.numeric(data_clos$task)]

# Grammatical category (baseline set to D)
contrasts(data_clos$gram_cat) = c(-0.5, 0.5)
data_clos$gram_catContrast = contrasts(data_clos$gram_cat)[,1][as.numeric(data_clos$gram_cat)]


## RUN GLMERS ON PRESENCE OF CLOSURE
# Full model
data_clos.glmer = glmer(closure_presence ~ context_catContrast * context_start_lgContrast + (1+context_catContrast|speaker), data=data_clos, family="binomial")

summary(data_clos.glmer)

# Test for effect of context
data_clos_context.glmer = glmer(closure_presence ~ context_catContrast * context_start_lgContrast - context_catContrast + (1+context_catContrast|speaker), data=data_clos, family="binomial")

anova(data_clos.glmer, data_clos_context.glmer)

# Test for effect of starting language
data_clos_startlg.glmer = glmer(closure_presence ~ context_catContrast * context_start_lgContrast - context_start_lgContrast + (1+context_catContrast|speaker), data=data_clos, family="binomial")

anova(data_clos.glmer, data_clos_startlg.glmer)

# Test for interaction of context and starting language
data_clos_contextxstartlg.glmer = glmer(closure_presence ~ context_catContrast * context_start_lgContrast - context_catContrast:context_start_lgContrast + (1+context_catContrast|speaker), data=data_clos, family="binomial")

anova(data_clos.glmer, data_clos_contextxstartlg.glmer)

# Follow-up regressions for interaction
data_clos_ml = subset(data_clos, context_cat=="ML")
	data_clos_ml$context_cat = factor(data_clos_ml$context_cat)
data_clos_cs = subset(data_clos, context_cat=="CS")
	data_clos_cs$context_cat = factor(data_clos_cs$context_cat)
summary(glm(closure_presence ~ context_start_lg, data_clos_ml, family="binomial"))
summary(glm(closure_presence ~ context_start_lg, data_clos_cs, family="binomial"))

data_clos_e = subset(data_clos, context_start_lg=="E")
	data_clos_e$context_start_lg = factor(data_clos_e$context_start_lg)
data_clos_s = subset(data_clos, context_start_lg=="S")
	data_clos_s$context_start_lg = factor(data_clos_s$context_start_lg)
summary(glm(closure_presence ~ context_cat, data_clos_e, family="binomial"))
summary(glm(closure_presence ~ context_cat, data_clos_s, family="binomial"))











