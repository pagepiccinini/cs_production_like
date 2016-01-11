## READ IN REQUIRED LIBRARIES
library(pcaMethods)

## ORGAINZIE DATA
# Set working directory for source file and read in file for organizing data
setwd("~/Desktop/Experiments/CS E-S Production - like/Results/scripts/")
source("data_organization_clustering_bark.R")

# Add ids to each token
data_morph$id = as.character(1:nrow(data_morph))

# Look at only training data
data_morph_sub = subset(data_morph, context_specific=="E" | context_specific=="S" | context_specific=="CS_ES" | context_specific=="CS_SE")
	data_morph_sub$context_specific = factor(data_morph_sub$context_specific)
	data_morph_sub$context_cat = factor(data_morph_sub$context_cat)
	data_morph_sub$context_start_lg = factor(data_morph_sub$context_start_lg)

# Downsample training data
data_morph_sub_downsamp = data_morph_sub %>%
	group_by(context_specific) %>%
	sample_n(31, replace=F) %>%
	ungroup()


## RUN PCA ON DOWNSAMPLED TRAINING DATA
# Format data
m = as.matrix(data_morph_sub_downsamp[,8:50])
rownames(m) = data_morph_sub_downsamp$id

# Run PCA
data_sub.pca = pca(m, method="ppca", nPcs=10, scale="uv")

# Save scores
data_sub.scores = data_sub.pca %>%
  scores %>%
  as.data.frame %>%
  mutate(id=rownames(.)) %>%
  inner_join(data_morph_sub_downsamp)
  
# Run GLM call on PCs
	# Starting language
start_lg.glm = glm(context_start_lg ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data=data_sub.scores, family="binomial")
summary(start_lg.glm)
	# --> PC1 *, PC3 **
	# --> PC5 ~, PC6 ~

	# Context cat
context.glm = glm(context_cat ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data=data_sub.scores, family="binomial")
summary(context.glm)
	# --> PC10 **
	
# Plot PCs
plot(PC10 ~ PC3, data=data_sub.scores, col=context_specific, pch=20, cex=2)
legend("topleft", legend=levels(data_sub.scores$context_specific), fill=1:4)


plot(PC1 ~ PC3, data=subset(data_sub.scores, context_cat=="ML"), col=context_start_lg, pch=20, cex=2)
legend("topleft", legend=levels(data_sub.scores$context_specific), fill=1:4)
 

## PREDICT ON FULL DATASET
# Pull out testing data
data_morph_test = subset(data_morph, context_specific!="E" & context_specific!="S" & context_specific!="CS_ES" & context_specific!="CS_SE")
	data_morph_test$context_specific = factor(data_morph_test$context_specific)
	data_morph_test$context_cat = factor(data_morph_test$context_cat)
	data_morph_test$context_start_lg = factor(data_morph_test$context_start_lg)
	
# Combine down sampled training data and testing data
data_full = rbind_list(data_morph_sub_downsamp, data_morph_test)

# Run prediction
data.predict = predict(data_sub.pca, data_full[,8:50])

# Save scores
data_predict.scores = data.predict$scores %>%
	as.data.frame %>%
	mutate(id=rownames(.)) %>%
	inner_join(data_morph)
	
# Run GLM call on PCs
	# Starting language
start_lg_predict.glm = glm(context_start_lg ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data=data_predict.scores, family="binomial")
summary(start_lg_predict.glm)

	# Context cat
context_predict.glm = glm(context_cat ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data=data_predict.scores, family="binomial")
summary(context_predict.glm)


  
  
  
  
  
  
  
  
  
  
  