# more path analysis #





library(dplyr);library(DiagrammeR)

##### calculate proportions #####

# proportion of attacked trees
final_dataset$beetleprop <- final_dataset$count_wbp_beetle / (final_dataset$count_wbp_beetle + final_dataset$count_wbp_nobeetle)

final_dataset$beetleprop[is.na(final_dataset$beetleprop)] <- 0

epsilon <- 0.0001 
final_dataset$beetleprop <- pmax(epsilon, pmin(final_dataset$beetleprop, 1 - epsilon))

head(final_dataset$beetleprop)

# proportion of dead trees
final_dataset <- final_dataset %>%
  mutate(
    prop_dead_wbp = count_dead_wbp / (count_dead_wbp + count_live_wbp),
    prop_dead_wbp = ifelse(is.na(prop_dead_wbp), 0, prop_dead_wbp))

epsilon <- 0.0001
final_dataset$prop_dead_wbp <- pmax(epsilon, pmin(final_dataset$prop_dead_wbp, 1 - epsilon))

head(final_dataset)

##### mortality model #####

#  pca: "airidity" cwd, aet, and precip

# plug in data frame name and predictor variables
mortality_pca_vars <- scale(final_dataset[, c("precip", "aet", "cwd")], center = TRUE, scale = TRUE)

# view result - should show that PC axis explains most of the variance
mortality_pca_result <- prcomp(mortality_pca_vars, center = TRUE, scale. = TRUE)
summary(mortality_pca_result)

# save PC axis 1 as a column in data frame
final_dataset$mortality_PC1 <- mortality_pca_result$x[, 1]

# new model: 
#### FROM CHRIS: scaling the predictor variables
final_dataset1 <- final_dataset
final_dataset1[,5:22] <- apply(final_dataset[,5:22], 2, scale)
mortality_model_pca <- glm(cbind(count_dead_wbp, count_live_wbp) ~ beetleprop  + avg_rust  + site * mortality_PC1 + spring_avg_temp, 
                           family = binomial, data = final_dataset1)
summary(mortality_model_pca)
Anova(mortality_model_pca, type=3)

# test variance explained

null_model <- glm(cbind(count_dead_wbp, count_live_wbp) ~ 1, family = binomial, data = final_dataset)

r2_mcfadden <- 1 - (as.numeric(logLik(mortality_model_pca)) / as.numeric(logLik(null_model)))
print(r2_mcfadden)


##### beetle model #####

# first pca: "airidity" cwd, aet, and precip

beetle_pca1_vars <- scale(final_dataset[, c("precip", "aet", "cwd")], center = TRUE, scale = TRUE)

beetle_pca1_result <- prcomp(beetle_pca1_vars, center = TRUE, scale. = TRUE)
summary(beetle_pca1_result)

final_dataset$beetle_PC1 <- beetle_pca1_result$x[, 1]

# second pca: "aspect" heat value and wind exposure

beetle_pca2_vars <- scale(final_dataset[, c("WindExposure", "heat_value")], center = TRUE, scale = TRUE)

beetle_pca2_result <- prcomp(beetle_pca2_vars, center = TRUE, scale. = TRUE)
summary(beetle_pca2_result)

final_dataset$beetle_PC2 <- beetle_pca2_result$x[, 1]

# new model: 
#### FROM CHRIS: scaling the predictor variables
final_dataset2 <- final_dataset
final_dataset2[,c(5:9, 12:24)] <- apply(final_dataset[,c(5:9, 12:24)], 2, scale)
beetle_model_pca <- glm(cbind(count_wbp_beetle, count_wbp_nobeetle) ~ avg_rust + nonwbp_conifer_basal_area + prop_notwbp_beetle + wbp_basal_area  + beetle_PC1 + beetle_PC2 * site + spring_avg_temp ,
                        family = binomial, data = final_dataset2)

summary(beetle_model_pca)
Anova(beetle_model_pca, type = 3)

## test variance explained

null_model2 <- glm(cbind(count_wbp_beetle, count_wbp_nobeetle) ~ 1, family = binomial, data = final_dataset)

r2_mcfadden_model2 <- 1 - (as.numeric(logLik(beetle_model_pca)) / as.numeric(logLik(null_model2)))

print(r2_mcfadden_model2)





##### path analysis #####

# Extract coefficients from models
coef_model2 <- coef(beetle_model_pca) # For the beetle attack model
coef_model <- coef(mortality_model_pca)  # For the mortality model

# Extract relevant coefficients 
#### FROM CHRIS: I have commented out the latent linear calculation. Just remove the comments to run it.
#### Sorry for a bit of mess... this is not as tidy as I would normally do. The steps of how I got here are
#### described in the vignette section: 2.2.4 Standardization for Binary Response Models

coef_avg_rust_to_beetle <- coef_model2["avg_rust"]* sd(final_dataset2$avg_rust)
coef_nonwbp_conifer_basal_area <- coef_model2["nonwbp_conifer_basal_area"] * sd(final_dataset2$nonwbp_conifer_basal_area) / (sqrt(var(predict(beetle_model_pca, type = "link")) + pi^2/3))
coef_prop_notwbp_beetle <- coef_model2["prop_notwbp_beetle"] * sd(final_dataset2$prop_notwbp_beetle) / (sqrt(var(predict(beetle_model_pca, type = "link")) + pi^2/3))
coef_wbp_basal_area_beetle <- coef_model2["wbp_basal_area"] * sd(final_dataset2$wbp_basal_area) / (sqrt(var(predict(beetle_model_pca, type = "link")) + pi^2/3))
coef_beetle_PC1_to_beetle <- coef_model2["beetle_PC1"] * sd(final_dataset2$beetle_PC1) / (sqrt(var(predict(beetle_model_pca, type = "link")) + pi^2/3))
coef_beetle_PC2_to_beetle <- coef_model2["beetle_PC2"] * sd(final_dataset2$beetle_PC2) / (sqrt(var(predict(beetle_model_pca, type = "link")) + pi^2/3))
coef_spring_avg_temp_beetle <- coef_model2["spring_avg_temp"] * sd(final_dataset2$spring_avg_temp) / (sqrt(var(predict(beetle_model_pca, type = "link")) + pi^2/3))

coef_beetle_attack_to_mortality <- coef_model["beetleprop"] * sd(final_dataset1$beetleprop) / (sqrt(var(predict(mortality_model_pca, type = "link")) + pi^2/3))
coef_avg_rust_to_mortality <- coef_model["avg_rust"] * sd(final_dataset1$avg_rust) / (sqrt(var(predict(mortality_model_pca, type = "link")) + pi^2/3))
coef_mortality_PC1_to_mortality <- coef_model["mortality_PC1"] * sd(final_dataset1$mortality_PC1) / (sqrt(var(predict(mortality_model_pca, type = "link")) + pi^2/3))
coef_spring_avg_temp_mortality <- coef_model["spring_avg_temp"] * sd(final_dataset1$spring_avg_temp) / (sqrt(var(predict(mortality_model_pca, type = "link")) + pi^2/3))

# Format coefficients
formatted_avg_rust_to_beetle <- sprintf("%.4f", coef_avg_rust_to_beetle)
formatted_nonwbp_conifer_basal_area <- sprintf("%.4f", coef_nonwbp_conifer_basal_area)
formatted_prop_notwbp_beetle <- sprintf("%.4f", coef_prop_notwbp_beetle)
formatted_wbp_basal_area_beetle <- sprintf("%.4f", coef_wbp_basal_area_beetle)
formatted_beetle_PC1_to_beetle <- sprintf("%.4f", coef_beetle_PC1_to_beetle)
formatted_beetle_PC2_to_beetle <- sprintf("%.4f", coef_beetle_PC2_to_beetle)

formatted_spring_avg_temp_beetle <- sprintf("%.4f", coef_spring_avg_temp_beetle)

formatted_beetle_attack_to_mortality <- sprintf("%.4f", coef_beetle_attack_to_mortality)
formatted_avg_rust_to_mortality <- sprintf("%.4f", coef_avg_rust_to_mortality)
formatted_mortality_PC1_to_mortality <- sprintf("%.4f", coef_mortality_PC1_to_mortality)
formatted_spring_avg_temp_mortality <- sprintf("%.4f", coef_spring_avg_temp_mortality)

# Generate Graphviz diagram
grViz(sprintf("
digraph PathAnalysis {
  graph [layout = dot, rankdir = LR]
  
  # Nodes
  avg_rust [label = 'Average Rust', shape = ellipse, style = filled, fillcolor = green]
  nonwbp_conifer_basal_area [label = 'Non-WBP Conifer Basal Area', shape = ellipse, style = filled, fillcolor = lightcyan]
  prop_notwbp_beetle [label = 'Proportion Not WBP Beetle', shape = ellipse, style = filled, fillcolor = lightpink]
  wbp_basal_area [label = 'WBP Basal Area', shape = ellipse, style = filled, fillcolor = lightyellow]
  beetle_PC1 [label = 'Beetle PC1', shape = ellipse, style = filled, fillcolor = lightblue]
  beetle_PC2 [label = 'Beetle PC2', shape = ellipse, style = filled, fillcolor = lightblue]
  spring_avg_temp [label = 'Spring Avg Temp', shape = ellipse, style = filled, fillcolor = purple]
  beetle_attack [label = 'Beetle Attack', shape = ellipse, style = filled, fillcolor = lightcoral]
  mortality [label = 'WBP Mortality', shape = ellipse, style = filled, fillcolor = lightgoldenrod]
  mortality_PC1 [label = 'Mortality PC1', shape = ellipse, style = filled, fillcolor = orange]
  beetleprop [label = 'Beetle Proportion', shape = ellipse, style = filled, fillcolor = lightpink]

  # Edges (Path Coefficients)
  avg_rust -> beetle_attack [label = '%s']
  nonwbp_conifer_basal_area -> beetle_attack [label = '%s']
  prop_notwbp_beetle -> beetle_attack [label = '%s']
  wbp_basal_area -> beetle_attack [label = '%s']
  beetle_PC1 -> beetle_attack [label = '%s']
  beetle_PC2 -> beetle_attack [label = '%s']
  spring_avg_temp -> beetle_attack [label = '%s']
  
  beetle_attack -> mortality [label = '%s']
  beetleprop -> mortality [label = '%s']
  avg_rust -> mortality [label = '%s']
  mortality_PC1 -> mortality [label = '%s']
  spring_avg_temp -> mortality [label = '%s']
}
",
formatted_avg_rust_to_beetle, 
formatted_nonwbp_conifer_basal_area, 
formatted_prop_notwbp_beetle, 
formatted_wbp_basal_area_beetle, 
formatted_beetle_PC1_to_beetle, 
formatted_beetle_PC2_to_beetle, 
formatted_spring_avg_temp_beetle, 
formatted_beetle_attack_to_mortality, 
formatted_beetle_attack_to_mortality, 
formatted_avg_rust_to_mortality, 
formatted_mortality_PC1_to_mortality, 
formatted_spring_avg_temp_mortality))



##### indirect effects #####

# Extract coefficients and SEs
coef_beetle <- coef(beetle_model_pca)
se_beetle <- sqrt(diag(vcov(beetle_model_pca)))

coef_mortality <- coef(mortality_model_pca)
se_mortality <- sqrt(diag(vcov(mortality_model_pca)))

# Get values for rust
a_rust <- coef_beetle["avg_rust"]  # Rust -> Beetle
se_a_rust <- se_beetle["avg_rust"]

b_rust <- coef_mortality["beetleprop"]  # Beetle -> Mortality
se_b_rust <- se_mortality["beetleprop"]

# Indirect effect (Rust -> Beetle -> Mortality)
indirect_rust <- a_rust * b_rust

# Delta method for SE
se_indirect_rust <- sqrt((b_rust^2 * se_a_rust^2) + (a_rust^2 * se_b_rust^2))

# Compute 95% CI
lower_rust <- indirect_rust - 1.96 * se_indirect_rust
upper_rust <- indirect_rust + 1.96 * se_indirect_rust

# Print result
cat("Indirect effect of Rust on Mortality:", indirect_rust, "\n")
cat("95% CI:", lower_rust, "to", upper_rust, "\n")

# Repeat for Spring Temp
a_temp <- coef_beetle["spring_avg_temp"]
se_a_temp <- se_beetle["spring_avg_temp"]

b_temp <- coef_mortality["beetleprop"]
se_b_temp <- se_mortality["beetleprop"]

indirect_temp <- a_temp * b_temp
se_indirect_temp <- sqrt((b_temp^2 * se_a_temp^2) + (a_temp^2 * se_b_temp^2))

lower_temp <- indirect_temp - 1.96 * se_indirect_temp
upper_temp <- indirect_temp + 1.96 * se_indirect_temp

cat("Indirect effect of Spring Temp on Mortality:", indirect_temp, "\n")
cat("95% CI:", lower_temp, "to", upper_temp, "\n")

names(coef(beetle_model_pca))
print(coef(beetle_model_pca))


## including indirect effects visually #####

# Direct effects for Rust and Spring Temp
formatted_a_rust <- sprintf("%.4f", a_rust)
formatted_b_rust <- sprintf("%.4f", b_rust)
formatted_beetle_attack_to_mortality <- sprintf("%.4f", coef_mortality["beetleprop"])

# Indirect effects (Rust -> Beetle -> Mortality and Spring Temp -> Beetle -> Mortality)
formatted_indirect_rust <- sprintf("%.4f", indirect_rust)
formatted_indirect_temp <- sprintf("%.4f", indirect_temp)

# Create the path diagram
grViz(sprintf("
digraph PathAnalysis {
  graph [layout = dot, rankdir = LR]
  
  # Nodes
  avg_rust [label = 'Average Rust', shape = ellipse, style = filled, fillcolor = green]
  spring_avg_temp [label = 'Spring Avg Temp', shape = ellipse, style = filled, fillcolor = purple]
  beetle_attack [label = 'Beetle Attack', shape = ellipse, style = filled, fillcolor = lightcoral]
  mortality [label = 'WBP Mortality', shape = ellipse, style = filled, fillcolor = lightgoldenrod]

  # Edges (Path Coefficients)
  avg_rust -> beetle_attack [label = '%s', color = black] 
  spring_avg_temp -> beetle_attack [label = '%s', color = black] 
  
  beetle_attack -> mortality [label = '%s', color = black]
  
  # Indirect effects (dashed blue lines)
  avg_rust -> mortality [label = '%s', style = dashed, color = blue] 
  spring_avg_temp -> mortality [label = '%s', style = dashed, color = blue] 
}
",
formatted_a_rust, formatted_b_rust, formatted_beetle_attack_to_mortality,
formatted_indirect_rust, formatted_indirect_temp))

##### combine #####

# Define all the required coefficients and indirect effects
# You should have these variables from your previous code

# Direct effects
formatted_avg_rust_to_beetle <- sprintf("%.4f", coef_beetle["avg_rust"])
formatted_nonwbp_conifer_basal_area <- sprintf("%.4f", coef_beetle["nonwbp_conifer_basal_area"])
formatted_prop_notwbp_beetle <- sprintf("%.4f", coef_beetle["prop_notwbp_beetle"])
formatted_wbp_basal_area_beetle <- sprintf("%.4f", coef_beetle["wbp_basal_area"])
formatted_beetle_PC1_to_beetle <- sprintf("%.4f", coef_beetle["beetle_PC1"])
formatted_beetle_PC2_to_beetle <- sprintf("%.4f", coef_beetle["beetle_PC2"])
formatted_spring_avg_temp_beetle <- sprintf("%.4f", coef_beetle["spring_avg_temp"])

formatted_beetle_attack_to_mortality <- sprintf("%.4f", coef_mortality["beetleprop"])
formatted_avg_rust_to_mortality <- sprintf("%.4f", coef_mortality["avg_rust"])
formatted_mortality_PC1_to_mortality <- sprintf("%.4f", coef_mortality["mortality_PC1"])
formatted_spring_avg_temp_mortality <- sprintf("%.4f", coef_mortality["spring_avg_temp"])

# Indirect effects
formatted_indirect_rust <- sprintf("%.4f", indirect_rust)
formatted_indirect_temp <- sprintf("%.4f", indirect_temp)

# Generate Graphviz diagram
grViz(sprintf("
digraph PathAnalysis {
  graph [layout = dot, rankdir = LR]
  
  # Nodes
  avg_rust [label = 'Average Rust', shape = ellipse, style = filled, fillcolor = green]
  nonwbp_conifer_basal_area [label = 'Non-WBP Conifer Basal Area', shape = ellipse, style = filled, fillcolor = lightcyan]
  prop_notwbp_beetle [label = 'Proportion Not WBP Beetle', shape = ellipse, style = filled, fillcolor = lightpink]
  wbp_basal_area [label = 'WBP Basal Area', shape = ellipse, style = filled, fillcolor = lightyellow]
  beetle_PC1 [label = 'Beetle PC1', shape = ellipse, style = filled, fillcolor = lightblue]
  beetle_PC2 [label = 'Beetle PC2', shape = ellipse, style = filled, fillcolor = lightblue]
  spring_avg_temp [label = 'Spring Avg Temp', shape = ellipse, style = filled, fillcolor = purple]
  beetle_attack [label = 'Beetle Attack', shape = ellipse, style = filled, fillcolor = lightcoral]
  mortality [label = 'WBP Mortality', shape = ellipse, style = filled, fillcolor = lightgoldenrod]
  mortality_PC1 [label = 'Mortality PC1', shape = ellipse, style = filled, fillcolor = orange]

  # Edges (Path Coefficients)
  avg_rust -> beetle_attack [label = '%s']
  nonwbp_conifer_basal_area -> beetle_attack [label = '%s']
  prop_notwbp_beetle -> beetle_attack [label = '%s']
  wbp_basal_area -> beetle_attack [label = '%s']
  beetle_PC1 -> beetle_attack [label = '%s']
  beetle_PC2 -> beetle_attack [label = '%s']
  spring_avg_temp -> beetle_attack [label = '%s']
  
  beetle_attack -> mortality [label = '%s']
  avg_rust -> mortality [label = '%s']
  mortality_PC1 -> mortality [label = '%s']
  spring_avg_temp -> mortality [label = '%s']
  
  # Indirect effects (dashed blue lines)
  avg_rust -> mortality [label = '%s', style = dashed, color = blue]
  spring_avg_temp -> mortality [label = '%s', style = dashed, color = blue]
}
",
formatted_avg_rust_to_beetle, 
formatted_nonwbp_conifer_basal_area, 
formatted_prop_notwbp_beetle, 
formatted_wbp_basal_area_beetle, 
formatted_beetle_PC1_to_beetle, 
formatted_beetle_PC2_to_beetle, 
formatted_spring_avg_temp_beetle, 
formatted_beetle_attack_to_mortality, 
formatted_beetle_attack_to_mortality, 
formatted_avg_rust_to_mortality, 
formatted_mortality_PC1_to_mortality, 
formatted_spring_avg_temp_mortality, 
formatted_indirect_rust, 
formatted_indirect_temp))


#####

sprintf("
digraph PathAnalysis {
  graph [layout = dot, rankdir = LR]
  
  # Nodes
  avg_rust [label = 'Average Rust', shape = ellipse, style = filled, fillcolor = green]
  nonwbp_conifer_basal_area [label = 'Non-WBP Conifer Basal Area', shape = ellipse, style = filled, fillcolor = lightcyan]
  wbp_basal_area [label = 'WBP Basal Area', shape = ellipse, style = filled, fillcolor = lightyellow]
  aridity_pca [label = 'Aridity PCA', shape = ellipse, style = filled, fillcolor = lightblue]
  aspect_pca [label = 'Aspect PCA', shape = ellipse, style = filled, fillcolor = lightblue]
  spring_avg_temp [label = 'Spring Avg Temp', shape = ellipse, style = filled, fillcolor = purple]
  beetle_attack [label = 'Beetle Attack', shape = ellipse, style = filled, fillcolor = lightcoral]
  mortality [label = 'WBP Mortality', shape = ellipse, style = filled, fillcolor = lightgoldenrod]
  mortality_pca [label = 'Mortality PCA', shape = ellipse, style = filled, fillcolor = orange]

  # Edges (Path Coefficients)
  avg_rust -> beetle_attack [label = '%s']
  nonwbp_conifer_basal_area -> beetle_attack [label = '%s']
  wbp_basal_area -> beetle_attack [label = '%s']
  aridity_pca -> beetle_attack [label = '%s']
  aspect_pca -> beetle_attack [label = '%s']
  spring_avg_temp -> beetle_attack [label = '%s']
  
  beetle_attack -> mortality [label = '%s']
  avg_rust -> mortality [label = '%s']
  mortality_pca -> mortality [label = '%s']
  spring_avg_temp -> mortality [label = '%s']
  
  # Indirect effects (dashed blue lines)
  avg_rust -> mortality [label = '%s', style = dashed, color = blue]
  spring_avg_temp -> mortality [label = '%s', style = dashed, color = blue]
}", path_coefficients...)


# Indirect Effects
indirect_spring_temp <- coef_spring_avg_temp_beetle * coef_beetle_attack_to_mortality
indirect_avg_rust <- coef_avg_rust_to_beetle * coef_beetle_attack_to_mortality
indirect_aridity <- coef_beetle_aridity_to_beetle * coef_beetle_attack_to_mortality

# Total Effects = Direct + Indirect
total_spring_temp <- coef_spring_avg_temp_mortality + indirect_spring_temp
total_avg_rust <- coef_avg_rust_to_mortality + indirect_avg_rust
total_aridity <- coef_aridity_to_mortality + indirect_aridity

indirect_spring_temp
indirect_avg_rust
indirect_aridity

total_spring_temp
total_avg_rust
total_aridity
