library(vegan)
library(lmPerm)
library(RVAideMemoire)
library(multcomp)
library(lsr)
library(dplyr)
library(multcomp)
library(DescTools)
library(effectsize)
library(effsize)
library(openxlsx)
library(dplyr)
library(openxlsx)
library(effsize)
library(permuco)
library(writexl)
library(here)


# Read and preprocess the data
#file_path <- "H:/My Drive/2. Post-PhD/2. Research/4. Window View Quality Global Dataset/7. Interim Result/2. Data Filtering, Sorting, and Merging/C.FINAL_VERSION.csv"
#file_path <- "C:/Users/chang/My Drive (chang93a@gmail.com)/2. Post-PhD/2. Research/4. Window View Quality Global Dataset/7. Interim Result/2. Data Filtering, Sorting, and Merging/B.ColumnsWithoutNames.csv"
#file_path <- "G:/My Drive/2. Post-PhD/2. Research/4. Window View Quality Global Dataset/7. Interim Result/2. Data Filtering, Sorting, and Merging/C.FINAL_VERSION.csv"

data <- read.csv("View_experiment.csv")
names(data)
data <- na.omit(data)

# Convert numeric categorical predictors to factors and normalize numerical predictors
data <- data |>
  mutate(
    # ordered factors (categorical forms)
    predictor1_category   = factor(predictor1,   levels = c("1", "2", "3"), ordered = TRUE),
    predictor3_category   = factor(predictor3_category, levels = c("d < 20 m","d ≥ 20 m"), ordered = TRUE),
    predictor4_category   = factor(predictor4_category, levels = c("d > 5.1 m","d ≤ 5.1 m"), ordered = TRUE),
    predictor5_category   = factor(predictor5_category, levels = c("r < 50%","r ≥ 50%"),   ordered = TRUE),
    predictor6_category   = factor(predictor6_category, levels = c("θ < 54°","θ ≥ 54°"),   ordered = TRUE),
    predictor6.5_category = factor(predictor6.5_category, levels = c("θ < 54°","θ ≥ 54°"), ordered = TRUE),
    Fenestration_category = factor(Fenestration, levels = c("Shading","Clear"), ordered = TRUE),
    predictor2=factor(predictor2,   levels = c( "No-Nature", "Nature"), ordered = TRUE)
  )

unique(data$predictor1_category)
unique(data$predictor2)
unique(data$predictor3_category)
unique(data$predictor4_category)
unique(data$predictor5_category)
unique(data$predictor6_category)
unique(data$predictor6.5_category)
unique(data$Fenestration_category)

unique(data$Overall_view_quality)

result_aovp <- aovperm(
  Overall_view_quality ~
    (predictor1_category +
       predictor2 +
       predictor3_category +
       predictor4_category +
       predictor5_category +
       predictor6_category +
       predictor6.5_category +
       Fenestration_category),
  data = data,
  np = 10000,  # number of permutations
  type = "permutation",
  within = ~Participant_ID  # specify the within-subject variable
)

summary(result_aovp)


anova_table<-summary(result_aovp)
anova_table

# Extract ANOVA table
anova_table <- result_aovp$table

# Calculate total SS (sum of all SS values)
total_SS <- sum(anova_table$SS)

# Calculate eta squared for each predictor
eta_squared <- anova_table$SS / total_SS

# Add it to the table
anova_table$eta_squared <- eta_squared

anova_table$eta_label <- cut(
  anova_table$eta_squared,
  breaks = c(-Inf, 0.01, 0.06, 0.14, Inf),
  labels = c("negligible", "small", "medium", "large"),
  right = FALSE
)
anova_table
anova_table$Factor <- rownames(anova_table)
anova_table <- anova_table[, c("Factor", setdiff(names(anova_table), "Factor"))]

# Annotate resampled p-values
anova_table$significance <- cut(
  anova_table$`resampled P(>F)`,
  breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
  labels = c("***", "**", "*", ".", ""),
  right = FALSE
)

# Optional: round numeric values
anova_table$eta_squared <- round(anova_table$eta_squared, 3)
anova_table$SS <- round(anova_table$SS, 3)
anova_table$F <- round(anova_table$F, 3)
anova_table$`parametric P(>F)` <- signif(anova_table$`parametric P(>F)`, 3)
anova_table$`resampled P(>F)` <- signif(anova_table$`resampled P(>F)`, 3)

ordered_cols <- c(
  "Factor",
  "SS",
  "df",
  "F",
  "parametric P(>F)",
  "resampled P(>F)",
  "significance",
  "eta_squared",
  "eta_label"
)
anova_table <- anova_table[, ordered_cols]
anova_table

# Save Excel in the same folder as the R script
output_file <- here("Analyzing the impact of eight variables (Table 3).xlsx")
write.xlsx(anova_table, output_file)

cat("File saved to:", output_file, "\n")

#### Run Within-actor between level comparison 

# Define predictors
predictors <- c("predictor1_category", "predictor2", "Fenestration_category",
                     "predictor3_category", "predictor4_category",
                     "predictor5_category", "predictor6_category",
                     "predictor6.5_category")

## Make the table 

pairwise_mannwhitney <- function(data, response_var, predictors, output_excel_path) {
  final_results <- data.frame()
  
  for (pred in predictors) {
    levels <- unique(data[[pred]])
    
    # Skip predictor if fewer than 2 levels
    if (length(levels) < 2) {
      warning(paste("Skipping", pred, "-- fewer than 2 levels."))
      next
    }
    
    level_pairs <- combn(levels, 2, simplify = FALSE)
    
    for (pair in level_pairs) {
      data_pair <- data %>% filter(.data[[pred]] %in% pair)
      group1 <- data_pair %>% filter(.data[[pred]] == pair[1]) %>% pull(.data[[response_var]])
      group2 <- data_pair %>% filter(.data[[pred]] == pair[2]) %>% pull(.data[[response_var]])
      
      # Round means before calculating difference
      mean1 <- round(mean(group1, na.rm = TRUE), 2)
      mean2 <- round(mean(group2, na.rm = TRUE), 2)
      
      # Calculate mean (sd) strings
      group1_summary <- paste0(mean1, " (", round(sd(group1, na.rm = TRUE), 2), ")")
      group2_summary <- paste0(mean2, " (", round(sd(group2, na.rm = TRUE), 2), ")")
      
      # Mean difference (rounded means)
      mean_diff <- round(mean1 - mean2, 3)
      
      # Mann-Whitney U Test
      test <- wilcox.test(group1, group2)
      p_value <- round(test$p.value, 4)
      
      annotation <- case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01  ~ "**",
        p_value < 0.05  ~ "*",
        TRUE ~ ""
      )
      
      # Cliff's Delta
      cliffs <- cliff.delta(group1, group2)
      cliff_estimate <- round(cliffs$estimate, 3)
      cliff_magnitude <- case_when(
        abs(cliff_estimate) < 0.147 ~ "negligible",
        abs(cliff_estimate) < 0.33  ~ "small",
        abs(cliff_estimate) < 0.474 ~ "medium",
        TRUE ~ "large"
      )
      
      final_results <- rbind(final_results, data.frame(
        Predictor = pred,
        Level_1 = pair[1],
        Level_2 = pair[2],
        Mean_SD_Level_1 = group1_summary,
        Mean_SD_Level_2 = group2_summary,
        Mean_Difference = mean_diff,
        P_Value = p_value,
        Annotation = annotation,
        Cliffs_Delta = cliff_estimate,
        Cliff_Magnitude = cliff_magnitude
      ))
    }
  }
  
  write.xlsx(final_results, output_excel_path)
  return(final_results)
}
#

##
pairwise_mannwhitney_interaction <- function(data, response_var, predictors, output_excel_path) {
  
  final_results <- data.frame()
  
  # Loop through each single predictor (original code preserved)
  for (pred in predictors) {
    levels <- unique(data[[pred]])
    if (length(levels) < 2) next
    level_pairs <- combn(levels, 2, simplify = FALSE)
    
    for (pair in level_pairs) {
      data_pair <- data %>% filter(.data[[pred]] %in% pair)
      group1 <- data_pair %>% filter(.data[[pred]] == pair[1]) %>% pull(.data[[response_var]])
      group2 <- data_pair %>% filter(.data[[pred]] == pair[2]) %>% pull(.data[[response_var]])
      mean1 <- round(mean(group1, na.rm = TRUE), 2)
      mean2 <- round(mean(group2, na.rm = TRUE), 2)
      group1_summary <- paste0(mean1, " (", round(sd(group1, na.rm = TRUE), 2), ")")
      group2_summary <- paste0(mean2, " (", round(sd(group2, na.rm = TRUE), 2), ")")
      mean_diff <- round(mean1 - mean2, 3)
      test <- wilcox.test(group1, group2)
      p_value <- round(test$p.value, 4)
      annotation <- case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01  ~ "**",
        p_value < 0.05  ~ "*",
        TRUE ~ ""
      )
      cliffs <- cliff.delta(group1, group2)
      cliff_estimate <- round(cliffs$estimate, 3)
      cliff_magnitude <- case_when(
        abs(cliff_estimate) < 0.147 ~ "negligible",
        abs(cliff_estimate) < 0.33  ~ "small",
        abs(cliff_estimate) < 0.474 ~ "medium",
        TRUE ~ "large"
      )
      
      final_results <- rbind(final_results, data.frame(
        Predictor = pred,
        Level_1 = pair[1],
        Level_2 = pair[2],
        Mean_SD_Level_1 = group1_summary,
        Mean_SD_Level_2 = group2_summary,
        Mean_Difference = mean_diff,
        P_Value = p_value,
        Annotation = annotation,
        Cliffs_Delta = cliff_estimate,
        Cliff_Magnitude = cliff_magnitude,
        Interaction = FALSE
      ))
    }
  }
  
  # Now handle interactions between pairs of predictors
  interaction_pairs <- combn(predictors, 2, simplify = FALSE)

    for (pair in interaction_pairs) {
    pred1 <- pair[1]
    pred2 <- pair[2]
    
    # Create interaction variable
    data$interaction_group <- interaction(data[[pred1]], data[[pred2]], drop = TRUE)
    levels <- unique(data$interaction_group)
    if (length(levels) < 2) next
    level_pairs <- combn(levels, 2, simplify = FALSE)
    
    for (lv_pair in level_pairs) {
      data_pair <- data %>% filter(interaction_group %in% lv_pair)
      group1 <- data_pair %>% filter(interaction_group == lv_pair[1]) %>% pull(.data[[response_var]])
      group2 <- data_pair %>% filter(interaction_group == lv_pair[2]) %>% pull(.data[[response_var]])
      mean1 <- round(mean(group1, na.rm = TRUE), 2)
      mean2 <- round(mean(group2, na.rm = TRUE), 2)
      group1_summary <- paste0(mean1, " (", round(sd(group1, na.rm = TRUE), 2), ")")
      group2_summary <- paste0(mean2, " (", round(sd(group2, na.rm = TRUE), 2), ")")
      mean_diff <- round(mean1 - mean2, 3)
      test <- wilcox.test(group1, group2)
      p_value <- round(test$p.value, 4)
      annotation <- case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01  ~ "**",
        p_value < 0.05  ~ "*",
        TRUE ~ ""
      )
      cliffs <- cliff.delta(group1, group2)
      cliff_estimate <- round(cliffs$estimate, 3)
      cliff_magnitude <- case_when(
        abs(cliff_estimate) < 0.147 ~ "negligible",
        abs(cliff_estimate) < 0.33  ~ "small",
        abs(cliff_estimate) < 0.474 ~ "medium",
        TRUE ~ "large"
      )
      
      final_results <- rbind(final_results, data.frame(
        Predictor = paste(pred1, "×", pred2),
        Level_1 = as.character(lv_pair[1]),
        Level_2 = as.character(lv_pair[2]),
        Mean_SD_Level_1 = group1_summary,
        Mean_SD_Level_2 = group2_summary,
        Mean_Difference = mean_diff,
        P_Value = p_value,
        Annotation = annotation,
        Cliffs_Delta = cliff_estimate,
        Cliff_Magnitude = cliff_magnitude,
        Interaction = TRUE
      ))
    }
  }
  
  write.xlsx(final_results, output_excel_path)
  return(final_results)
}

# Call the function
results_df <- pairwise_mannwhitney_interaction(
  data = data,
  response_var = "Overall_view_quality",
  predictors = predictors,
  output_excel_path = here("within-level-analysis-aovperm (Table 3).xlsx")
)

