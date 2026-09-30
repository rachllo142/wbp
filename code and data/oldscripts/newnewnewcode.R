setwd("C:/Users/Rachel/OneDrive - Chatham University/Desktop/wbpproject")

allcenterdata <- read_excel("allcenterdata11.xlsx")
alltrees <- read_excel("AllTreeData.xlsx")

library(readxl);library(dplyr);library(ggplot2);library(car);library(ggcorrplot);library(lme4);library(lmerTest);library(tidyr);library(vegan);library(glmmTMB);library(randomForest);library(reshape2);library(writexl)

###### rf code #####
data_for_model <- na.omit(allcenterdata) 
response_variable <- "prop_deadwbp" 
predictors <- setdiff(names(data_for_model), response_variable) 

set.seed(123)  
rf_model <- randomForest(
  formula = as.formula(paste(response_variable, "~ .")), 
  data = data_for_model,
  importance = TRUE,  
  ntree = 500,       
  mtry = sqrt(length(predictors)) )
print(rf_model)

varImpPlot(rf_model)

##### correlation matrix code #####
cor_matrix <- cor(data_for_model[, sapply(data_for_model, is.numeric)], use = "complete.obs")

cor_melted <- melt(cor_matrix)

ggplot(cor_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, 
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(angle = 45, hjust = 1)) +
  labs(x = "Variables", y = "Variables")

##### prop of other trees #####

species_counts <- alltrees %>%
  group_by(PLOTID, Species) %>%
  summarise(tree_count = n(), .groups = "drop")

species_proportions <- species_counts %>%
  group_by(PLOTID) %>%
  mutate(total_trees = sum(tree_count),
         proportion = tree_count / total_trees) %>%
  select(PLOTID, Species, proportion)

species_proportions_wide <- species_proportions %>%
  pivot_wider(names_from = Species, values_from = proportion, 
              values_fill = 0)  

colnames(species_proportions_wide) <- colnames(species_proportions_wide) %>%
  if_else(. == "PLOTID", ., paste0("prop_", .))


head(species_proportions_wide)

allcenterdata <- allcenterdata %>%
  left_join(species_proportions_wide, by = "PLOTID")  

### do we see higher wait of mortality at lowercwd?
# revist path analysis in r 

## dbh into cum basal area for wbp 
alltrees$basalarea <- pi*(alltrees$DBH/2)^2

wbp_basal_area <- alltrees %>%
  filter(Species == "PIAL") %>%
  group_by(PLOTID) %>%
  summarize(cum_wbp_basal_area = sum(basalarea, na.rm = TRUE))

allcenterdata <- allcenterdata %>%
  left_join(wbp_basal_area, by = "PLOTID")

unique_species <- unique(alltrees$Species)
print(unique_species)

## cum basal area for all conifers

allconifer_basal_area <- alltrees %>%
  filter(!Species %in% c("UNK", "CELE")) %>%
  group_by(PLOTID) %>%
  summarize(cum_conifer_basal_area = sum(basalarea, na.rm = TRUE))

allcenterdata <- allcenterdata %>%
  left_join(allconifer_basal_area, by = "PLOTID")


## cum basal area for non wbp conifers

nonwbp_conifer_basal_area <- alltrees %>%
  filter(!Species %in% c("UNK", "CELE", "PIAL")) %>%
  group_by(PLOTID) %>%
  summarize(cum_conifer_basal_area = sum(basalarea, na.rm = TRUE))

allcenterdata <- allcenterdata %>%
  left_join(nonwbp_conifer_basal_area, by = "PLOTID")

## cum basal area for non conifers (just CELE)

nonconifer_basal_area <- alltrees %>%
  filter(Species == "CELE") %>%
  group_by(PLOTID) %>%
  summarize(cum_nonconifer_basal_area = sum(basalarea, na.rm = TRUE))

allcenterdata <- allcenterdata %>%
  left_join(nonconifer_basal_area, by = "PLOTID")

## cum basal area for pines
allpine_basal_area <- alltrees %>%
  filter(Species %in% c("PIAL", "PICO", "PIJE", "PIMO3")) %>%
  group_by(PLOTID) %>%
  summarize(cum_pine_basal_area = sum(basalarea, na.rm = TRUE))

allcenterdata <- allcenterdata %>%
  left_join(allpine_basal_area, by = "PLOTID")

write_xlsx(allcenterdata, "allcenterdata11.xlsx")
