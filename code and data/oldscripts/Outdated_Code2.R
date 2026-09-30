# broke code

center[is.na(center)] <- 0

##### beetle model #####

# beetles
model2 <- glm(cbind(count_wbp_beetle, count_wbp_nobeetle) ~  avg_rust + heat_value + nonwbp_conifer_basal_area + wbp_basal_area + prop_notwbp_beetle +cwd + aet + precip * site + WindExposure + spring_avg_temp ,
              family = binomial, data = center)
summary(model2)
Anova(model2, type=3)

## what is correlated? 

vars <- center[, c("avg_rust", "heat_value", "nonwbp_conifer_basal_area", "wbp_basal_area", 
                   "prop_notwbp_beetle", "cwd", "aet", "precip",  
                   "WindExposure", "spring_avg_temp")]

cor_matrix <- cor(vars, use = "complete.obs", method = "pearson")

print(cor_matrix)

corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", 
         addCoef.col = "black", tl.cex = 0.8, tl.srt = 45)

## put all those into a pca

# Replace NA values with 0 in the entire 'center' dataframe
center[is.na(center)] <- 0


pca_data <- center[, c("avg_rust", "heat_value", "nonwbp_conifer_basal_area", "wbp_basal_area", "cwd", "aet", "precip",
                       "WindExposure", "spring_avg_temp")]

pca_result <- prcomp(pca_data, center = TRUE, scale. = TRUE)

summary(pca_result)

autoplot(pca_result, data = center, colour = 'site', 
         loadings = TRUE, loadings.label = TRUE, 
         loadings.label.size = 4) +
  labs(title = "PCA of Beetle Model Predictors") +
  theme_minimal()

## pca on predicots

# first pca - wind exposure and heat
pca_vars_2 <- center[, c("WindExposure", "heat_value")]

pca_result_2 <- prcomp(pca_vars_2, center = TRUE, scale. = TRUE)

center$pca_2 <- pca_result_2$x[, 1]  

# second pca - aet, cwd, precip
pca_vars_1 <- center[, c("cwd", "aet", "precip")]

pca_result_1 <- prcomp(pca_vars_1, center = TRUE, scale. = TRUE)

center$pca_1 <- pca_result_1$x[, 1]  


model2_pca <- glm(cbind(count_wbp_beetle, count_wbp_nobeetle) ~ avg_rust + nonwbp_conifer_basal_area + prop_notwbp_beetle + wbp_basal_area  + pca_2 + pca_1 * site + spring_avg_temp ,
                  family = binomial, data = center)

summary(model2_pca)
Anova(model2_pca, type = 3)


## test variance explained


null_model2 <- glm(cbind(count_wbp_beetle, count_wbp_nobeetle) ~ 1, family = binomial, data = center)

r2_mcfadden_model2 <- 1 - (as.numeric(logLik(model2_pca)) / as.numeric(logLik(null_model2)))

print(r2_mcfadden_model2)



eff <- effect("pca_1", model2_pca) 
plot(eff)

eff_df <- as.data.frame(eff)

ggplot(eff_df, aes(x = pca_1, y = fit)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs(title = "Effect of Aridity on Beetle Attack",
       x = "PC2 (CWD, AET, and precip)",
       y = "Beetle Attack") +
  theme_classic()

###### ok lets try from other data ####

# broke code



##### beetle model #####

# beetles
model2 <- glm(cbind(count_wbp_beetle, count_wbp_nobeetle) ~  avg_rust + heat_value + nonwbp_conifer_basal_area + wbp_basal_area + prop_notwbp_beetle +cwd + aet + precip * site + WindExposure + spring_avg_temp ,
              family = binomial, data = finaldata)
summary(model2)
Anova(model2, type=3)

## what is correlated? 

vars <- center[, c("avg_rust", "heat_value", "nonwbp_conifer_basal_area", "wbp_basal_area", 
                   "prop_notwbp_beetle", "cwd", "aet", "precip",  
                   "WindExposure", "spring_avg_temp")]

cor_matrix <- cor(vars, use = "complete.obs", method = "pearson")

print(cor_matrix)

corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", 
         addCoef.col = "black", tl.cex = 0.8, tl.srt = 45)

## put all those into a pca

# Replace NA values with 0 in the entire 'center' dataframe
center[is.na(center)] <- 0


pca_data <- finaldata[, c("avg_rust", "heat_value", "nonwbp_conifer_basal_area", "wbp_basal_area", "cwd", "aet", "precip",
                       "WindExposure", "spring_avg_temp")]

pca_result <- prcomp(pca_data, center = TRUE, scale. = TRUE)

summary(pca_result)

autoplot(pca_result, data = finaldata, colour = 'site', 
         loadings = TRUE, loadings.label = TRUE, 
         loadings.label.size = 4) +
  labs(title = "PCA of Beetle Model Predictors") +
  theme_minimal()

## pca on predicots

# first pca - wind exposure and heat
pca_vars_2 <- finaldata[, c("WindExposure", "heat_value")]

pca_result_2 <- prcomp(pca_vars_2, center = TRUE, scale. = TRUE)

finaldata$pca_aspect <- pca_result_2$x[, 1]  

# second pca - aet, cwd, precip
pca_vars_1 <- finaldata[, c("cwd", "aet", "precip")]

pca_result_1 <- prcomp(pca_vars_1, center = TRUE, scale. = TRUE)

finaldata$pca_aridity <- pca_result_1$x[, 1]  


model2_pca <- glm(cbind(count_wbp_beetle, count_wbp_nobeetle) ~ avg_rust + nonwbp_conifer_basal_area + prop_notwbp_beetle + wbp_basal_area  + pca_aspect + pca_aridity * site + spring_avg_temp ,
                  family = binomial, data = finaldata)

summary(model2_pca)
Anova(model2_pca, type = 3)


## test variance explained


null_model2 <- glm(cbind(count_wbp_beetle, count_wbp_nobeetle) ~ 1, family = binomial, data = finaldata)

r2_mcfadden_model2 <- 1 - (as.numeric(logLik(model2_pca)) / as.numeric(logLik(null_model2)))

print(r2_mcfadden_model2)



eff <- effect("pca_aridity", model2_pca) 
plot(eff)

eff_df <- as.data.frame(eff)

ggplot(eff_df, aes(x = pca_aridity, y = fit)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs(title = "Effect of Aridity on Beetle Attack",
       x = "PC2 (CWD, AET, and precip)",
       y = "Beetle Attack") +
  theme_classic()


#########################################

##### beetle model #####

# beetles
model2 <- glm(cbind(count_wbp_beetle, count_wbp_nobeetle) ~  avg_rust + heat_value + nonwbp_conifer_basal_area + wbp_basal_area + prop_notwbp_beetle +cwd + aet + precip * site + WindExposure + spring_avg_temp ,
              family = binomial, data = center)
summary(model2)
Anova(model2, type=3)

## what is correlated? 

vars <- center[, c("avg_rust", "heat_value", "nonwbp_conifer_basal_area", "wbp_basal_area", 
                   "prop_notwbp_beetle", "cwd", "aet", "precip",  
                   "WindExposure", "spring_avg_temp")]

cor_matrix <- cor(vars, use = "complete.obs", method = "pearson")

print(cor_matrix)

corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", 
         addCoef.col = "black", tl.cex = 0.8, tl.srt = 45)

## put all those into a pca

# Replace NA values with 0 in the entire 'center' dataframe
center[is.na(center)] <- 0


pca_data <- center[, c("avg_rust", "heat_value", "nonwbp_conifer_basal_area", "wbp_basal_area", "cwd", "aet", "precip",
                       "WindExposure", "spring_avg_temp")]

pca_result <- prcomp(pca_data, center = TRUE, scale. = TRUE)

summary(pca_result)

autoplot(pca_result, data = center, colour = 'site', 
         loadings = TRUE, loadings.label = TRUE, 
         loadings.label.size = 4) +
  labs(title = "PCA of Beetle Model Predictors") +
  theme_minimal()

## pca on predicots

# first pca - wind exposure and heat
pca_vars_2 <- center[, c("WindExposure", "heat_value")]

pca_result_2 <- prcomp(pca_vars_2, center = TRUE, scale. = TRUE)

center$pca_2 <- pca_result_2$x[, 1]  

# second pca - aet, cwd, precip
pca_vars_1 <- center[, c("cwd", "aet", "precip")]

pca_result_1 <- prcomp(pca_vars_1, center = TRUE, scale. = TRUE)

center$pca_1 <- pca_result_1$x[, 1]  


model2_pca <- glm(cbind(count_wbp_beetle, count_wbp_nobeetle) ~ avg_rust + nonwbp_conifer_basal_area + prop_notwbp_beetle + wbp_basal_area  + pca_2 + pca_1 * site + spring_avg_temp ,
                  family = binomial, data = center)

summary(model2_pca)
Anova(model2_pca, type = 3)


## test variance explained


null_model2 <- glm(cbind(count_wbp_beetle, count_wbp_nobeetle) ~ 1, family = binomial, data = center)

r2_mcfadden_model2 <- 1 - (as.numeric(logLik(model2_pca)) / as.numeric(logLik(null_model2)))

print(r2_mcfadden_model2)


## effects plots 

eff <- effect("pca_1", model2_pca) 
plot(eff)

eff_df <- as.data.frame(eff)

ggplot(eff_df, aes(x = pca_1, y = fit)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs(title = "Effect of Aridity on Beetle Attack",
       x = "PC2 (CWD, AET, and precip)",
       y = "Beetle Attack") +
  theme_classic()

mortality_effects <- allEffects(model)
plot(mortality_effects)
