# Load required packages
library(vegan)
library(lmPerm)
library(RVAideMemoire)
library(multcomp)
library(lsr)
library(dplyr)
library(DescTools)
library(effectsize)
library(MASS)       # For polr
library(ordinal)    # For clm
library(car)
library(brant)
library(effects)
library(tidyr)      # for pivot_longer
library(ggplot2)    # for plotting
library(patchwork)
library(effectsize)
library(writexl)
library(dplyr)
library(effsize)


# for combining multiple plots

# READING DATA
#file_path <- "G:/My Drive/2. Post-PhD/2. Research/4. Window View Quality Global Dataset/7. Interim Result/2. Data Filtering, Sorting, and Merging/C.FINAL_VERSION.csv"
#file_path <- "H:/My Drive/2. Post-PhD/2. Research/4. Window View Quality Global Dataset/7. Interim Result/2. Data Filtering, Sorting, and Merging/C.FINAL_VERSION.csv"
#file_path <- "G:/My Drive/2. Post-PhD/2. Research/4. Window View Quality Global Dataset/7. Interim Result/2. Data Filtering, Sorting, and Merging/C.FINAL_VERSION.csv"

data <- read.csv("View_experiment.csv")
print(names(data))

# PREPROCESSING
data <- na.omit(data)

# Convert relevant variables to ordered factors
data <- data %>%
  mutate(
    Overall_view_quality = as.numeric(Overall_view_quality)
  )
print(data$Overall_view_quality)

# Make new column 

# Add LEED_certificate column
data <- data %>%
  mutate(
    LEED_certificate = ifelse(Stimuli %in% c("C-711", "C-712", "C-714"), "LEED", "Non-LEED")
  )

# ===============================
# LEED vs Non-LEED stacked bars
# ===============================

table(data$LEED_certificate)

# Define the predictors you want to analyze
predictors <- c("LEED_certificate")

# Initialize list to store comparison results
all_results <- list()

# Loop over each predictor variable
for (var in predictors) {
  # Get the two levels of the current variable
  lvls <- unique(data[[var]])
  
  if (length(lvls) != 2) stop(paste("Variable", var, "does not have exactly two levels"))
  
  # Split Overall_view_quality by level
  group1 <- data$Overall_view_quality[data[[var]] == lvls[1]]
  group2 <- data$Overall_view_quality[data[[var]] == lvls[2]]
  
  n1 <- length(group1)
  n2 <- length(group2)
  
  # Skip if one of the groups is empty
  if (n1 == 0 | n2 == 0) next
  
  # Pairwise comparisons
  cmp <- outer(group1, group2, FUN = "-")
  
  # Calculate probabilities
  p_gt <- sum(cmp > 0) / (n1 * n2)
  p_eq <- sum(cmp == 0) / (n1 * n2)
  p_lt <- sum(cmp < 0) / (n1 * n2)
  
  # Create result data frame with consistent comparison labels
  df <- data.frame(
    Comparison = c("greater", "equal", "less"),
    Probability = c(p_gt, p_eq, p_lt),
    Group = var
  )
  
  all_results[[var]] <- df
}

# Combine all into one data frame
results_df <- do.call(rbind, all_results)

# Ensure consistent factor levels for Comparison (for color and stacking order)
results_df$Comparison <- factor(
  results_df$Comparison,
  levels = c("less", "equal", "greater")
)


# Plot horizontal stacked bar chart
ggplot(results_df, aes(x = "Comparison", y = Probability, fill = Comparison)) +
  geom_bar(stat = "identity", position = "fill", width = 0.5) +
  geom_text(aes(label = paste0(round(Probability * 100), "%")),
            position = position_fill(vjust = 0.5),
            color = "black", size = 4) +
  facet_wrap(~ Group, ncol = 1) +
  coord_flip() +
  scale_fill_manual(values = c(
    "greater" = "#6fa6cc",
    "equal"   = "#bbbbbb",
    "less"    = "#cdbb46"
  )) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 14)
  )

# ggsave(
#   filename = "G:/My Drive/2. Post-PhD/2. Research/4. Window View Quality Global Dataset/100. View Quality Paper/1001. Figures/7. Section 4 LEED vs Non-LEED/nature_comparison_2col.jpg",
#   width = 4, height = 1.83, units = "in", dpi = 600
# )

