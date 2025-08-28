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
library(ggplot2)


# Read and preprocess the data
#file_path <- "H:/My Drive/2. Post-PhD/2. Research/4. Window View Quality Global Dataset/7. Interim Result/2. Data Filtering, Sorting, and Merging/C.FINAL_VERSION.csv"
#file_path <- "C:/Users/chang/My Drive (chang93a@gmail.com)/2. Post-PhD/2. Research/4. Window View Quality Global Dataset/7. Interim Result/2. Data Filtering, Sorting, and Merging/B.ColumnsWithoutNames.csv"
#file_path <- "G:/My Drive/2. Post-PhD/2. Research/4. Window View Quality Global Dataset/7. Interim Result/2. Data Filtering, Sorting, and Merging/C.FINAL_VERSION.csv"

data <- read.csv("View_experiment.csv")
print(names(data))
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

###

# Define the predictors you want to analyze
predictors <- c("predictor2", "predictor3_category", "Fenestration_category")
outcome <- "Overall_view_quality"

#-----------------------
#Calculate Mann Whitney U test and Cliff's Delta

# ---- Helpers ----
# Significance stars
stars <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) return("***")
  if (p < 0.01)  return("**")
  if (p < 0.05)  return("*")
  "ns"
}

# APA-style p value string
p_apa <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) return("< .001")
  # round to 3 decimals and remove leading zero
  paste0(sub("^0", "", sprintf("%.3f", p)))
}

# Coerce outcome to numeric if it's an ordered factor
as_numeric_outcome <- function(x) {
  if (is.ordered(x) || is.factor(x)) {
    # Map factor levels to 1..K (typical for ordinal)
    return(as.numeric(x))
  }
  return(as.numeric(x))
}

# ---- Compute tests for each predictor ----
results_list <- lapply(predictors, function(pred) {
  # Keep only the two columns; drop NAs
  df <- data[, c(outcome, pred)]
  df <- stats::na.omit(df)
  
  # Ensure predictor has exactly 2 levels
  pred_factor <- droplevels(as.factor(df[[pred]]))
  levs <- levels(pred_factor)
  if (length(levs) != 2) {
    warning(sprintf("Predictor '%s' does not have exactly 2 levels. Skipping.", pred))
    return(NULL)
  }
  
  # Outcome numeric for Wilcoxon
  y <- as_numeric_outcome(df[[outcome]])
  
  # Group labels and grouping vector
  g <- pred_factor
  
  # Wilcoxon rank-sum (Mann–Whitney U)
  wt <- suppressWarnings(wilcox.test(y ~ g, exact = FALSE))  # exact=FALSE for larger samples
  
  # Cliff's delta (note order is group1 vs group2 by factor levels)
  cd <- cliff.delta(y, g)  # effsize handles grouping factor
  
  # Build row
  data.frame(
    Predictor        = pred,
    Level_1          = levs[1],
    Level_2          = levs[2],
    W_statistic      = unname(as.numeric(wt$statistic)),
    p_value_raw      = wt$p.value,
    p_value_APA      = p_apa(wt$p.value),
    Significance     = stars(wt$p.value),
    Cliff_delta      = unname(as.numeric(cd$estimate)),
    Magnitude        = as.character(cd$magnitude),
    stringsAsFactors = FALSE
  )
})

# Bind results
results_df <- do.call(rbind, results_list)

# Optional: nice rounding for numeric columns (kept raw p separately)
results_df <- results_df %>%
  mutate(
    W_statistic = round(W_statistic, 3),
    Cliff_delta = round(Cliff_delta, 3)
  )


# ---- Write formatted Excel (using here) ----
output_file <- here::here("Cliffs_Delta_Results (Figure 3).xlsx")

# Create workbook and worksheet
wb <- createWorkbook()
addWorksheet(wb, "Results")

# Write data with filter enabled
writeData(wb, "Results", results_df, withFilter = TRUE)

# Save workbook
saveWorkbook(wb, output_file, overwrite = TRUE)

cat("File saved to:", output_file, "\n")

# Styling
headerStyle <- createStyle(textDecoration = "bold", halign = "center", valign = "center")
addStyle(wb, "Results", headerStyle, rows = 1, cols = 1:ncol(results_df), gridExpand = TRUE)

# Freeze top row
freezePane(wb, "Results", firstRow = TRUE)

# Auto width
setColWidths(wb, "Results", cols = 1:ncol(results_df), widths = "auto")

# Optional number formats: set p_value_raw to 3 decimals
numStyle3 <- createStyle(numFmt = "0.000")
# find the column indices
p_raw_col <- which(names(results_df) == "p_value_raw")
w_col     <- which(names(results_df) == "W_statistic")
cd_col    <- which(names(results_df) == "Cliff_delta")

if (length(p_raw_col) == 1) addStyle(wb, "Results", numStyle3, rows = 2:(nrow(results_df) + 1), cols = p_raw_col, gridExpand = TRUE)
if (length(w_col) == 1)     addStyle(wb, "Results", numStyle3, rows = 2:(nrow(results_df) + 1), cols = w_col,     gridExpand = TRUE)
if (length(cd_col) == 1)    addStyle(wb, "Results", numStyle3, rows = 2:(nrow(results_df) + 1), cols = cd_col,    gridExpand = TRUE)

# Save
saveWorkbook(wb, output_file, overwrite = TRUE)

message("Excel saved to: ", output_file)

# Manual calculation and visualization

#-----------------------
# Manual calculation and visualization

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
    Comparison  = c("greater", "equal", "less"),
    Probability = c(p_gt, p_eq, p_lt),
    Group       = var
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

# Ensure consistent facet order
results_df$Group <- factor(
  results_df$Group,
  levels = c("predictor2", "predictor3_category", "Fenestration_category")
)

#-----
#Bar chart

library(grid)  # for unit()

# --- Build plot (keep bar width the same) ---
p <- ggplot(results_df, aes(x = "Comparison", y = Probability, fill = Comparison)) +
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
    strip.text = element_text(size = 14),
    # increase space BETWEEN facet rows (this does NOT shrink bars — we’ll grow figure height too)
    panel.spacing.y = unit(1.0, "cm")   # <-- adjust as you like (e.g., 1.0–2.0 cm)
  )

# --- Keep EXACT same bar height as your current single-facet figure (1.83 in) ---
base_per_facet_height_in <- 0.9         # this is the height you used for the 1-row plot
spacing_cm               <- 5          # must match theme(panel.spacing.y) above
spacing_in               <- spacing_cm / 2.54

n_facets <- nlevels(results_df$Group)

# Total height = (per-facet height * #facets) + (spacing between rows * (#facets - 1))
# This preserves the bar/panel height you already have, and just adds extra space between them.
height_in <- n_facets * base_per_facet_height_in + (n_facets - 1) * spacing_in

# ggsave(
#   filename = "G:/My Drive/2. Post-PhD/2. Research/4. Window View Quality Global Dataset/100. View Quality Paper/1001. Figures/4.1 Cliff's delta calculation/three_predictors_fixed_size.jpg",
#   plot = p,
#   width = 6,
#   height = height_in,
#   units = "in",
#   dpi = 600
# )

# Open folder in Windows Explorer
