## READ IN REQUIRED LIBRARIES
library(pcaMethods)

## ORGAINZIE DATA
# Set working directory for source file and read in file for organizing data
setwd("~/Desktop/Experiments/CS E-S Production - like/Results/scripts/")
source("data_organization_clustering.R")

# Add ids to each token
data_morph$id = as.character(1:nrow(data_morph))

# Look at only training data
data_morph_sub = subset(data_morph, context_specific=="E" | context_specific=="S" | context_specific=="CS_ES" | context_specific=="CS_SE")
	data_morph_sub$context_specific = factor(data_morph_sub$context_specific)

# Downsample training data
data_morph_sub_downsamp = data_morph_sub %>%
	group_by(context_specific) %>%
	sample_n(31, replace=F) %>%
	ungroup()


## RUN PCA ON DOWNSAMPLED TRAINING DATA
m = as.matrix(data_morph_sub_downsamp[,8:71])
m_nosex = t(scale(t(m)))
rownames(m_nosex) = data_morph_sub_downsamp$id

data_sub.pca = pca(m_nosex, method="ppca", nPcs=10)

data_sub.scores = data_sub.pca %>%
  scores %>%
  as.data.frame %>%
  mutate(id=rownames(.)) %>%
  inner_join(data_morph_sub_downsamp)
  
# Run GLM call on PCs
	# Starting language
start_lg.glm = glm(context_start_lg ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data=data_sub.scores, family="binomial")
summary(start_lg.glm)

	# Context cat
context.glm = glm(context_cat ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data=data_sub.scores, family="binomial")
summary(context.glm)
 


## PREDICT ON FULL DATASET
n = as.matrix(data_morph_sub[,8:71])
n_nosex = t(scale(t(n)))
rownames(n_nosex) = data_morph_sub$id
  
  
  
  
  
  
  
  
  
  
  