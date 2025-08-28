# ===== Load Libraries =====
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ordinal)    # clmm
library(patchwork)  # stacking plots

# ===== Read and Preprocess Data =====
# file_path <- "H:/My Drive/2. Post-PhD/2. Research/4. Window View Quality Global Dataset/7. Interim Result/2. Data Filtering, Sorting, and Merging/C.FINAL_VERSION.csv"
#file_path <- "G:/My Drive/2. Post-PhD/2. Research/4. Window View Quality Global Dataset/7. Interim Result/2. Data Filtering, Sorting, and Merging/C.FINAL_VERSION.csv"

data <- read.csv("View_experiment.csv")
print(names(data))
data <- na.omit(data)

# Ensure column names exist (adjust indices if needed)
colnames(data)[47] <- "predictor6.5"
colnames(data)[9]  <- "participant_id"

# Factors and numerics
data <- data |>
  dplyr::mutate(
    predictor1_category   = factor(predictor1, levels = c("1", "2", "3"), ordered = TRUE),
    predictor3_category   = factor(predictor3_category, levels = c("d < 20 m", "d ≥ 20 m"), ordered = TRUE),
    predictor4_category   = factor(predictor4_category, levels = c("d > 5.1 m", "d ≤ 5.1 m"), ordered = TRUE),
    predictor5_category   = factor(predictor5_category, levels = c("r < 50%", "r ≥ 50%"), ordered = TRUE),
    predictor6_category   = factor(predictor6_category, levels = c("θ < 54°", "θ ≥ 54°"), ordered = TRUE),
    predictor6.5_category = factor(predictor6.5_category, levels = c("θ < 54°", "θ ≥ 54°"), ordered = TRUE),
    
    # Keep Fenestration as unordered factor with "Shading" as reference
    Fenestration = factor(Fenestration, levels = c("Shading", "Clear"), ordered = FALSE),
    predictor2   = factor(predictor2, levels = c("No-Nature", "Nature"), ordered = TRUE),
    
    # Numeric conversions
    dplyr::across(c(Greenery_ratio, Sky_ratio, Nature_ratio,
                    predictor3, predictor4, predictor5, predictor6, predictor6.5),
                  ~ suppressWarnings(as.numeric(as.character(.))))
  )


unique(data$Fenestration)


# Ordered outcome with 7 categories
data$Overall_view_quality <- factor(
  data$Overall_view_quality,
  levels = c("-3", "-2", "-1", "0", "1", "2", "3"),
  labels = c("Very dissatisfied", "Dissatisfied", "Slightly dissatisfied",
             "Neutral", "Slightly satisfied", "Satisfied", "Very satisfied"),
  ordered = TRUE
)

# ===== Log-transformed Predictors =====
data <- data |>
  dplyr::mutate(
    predictor3_log     = log(predictor3 + 1),
    predictor4_log     = log(predictor4 + 1),
    predictor5_log     = log(predictor5 + 1),
    predictor6_log     = log(predictor6 + 1),
    predictor6.5_log   = log(predictor6.5 + 1),
    Greenery_ratio_log = log(Greenery_ratio + 1),
    Sky_ratio_log      = log(Sky_ratio + 1),
    Nature_ratio_log   = log(Nature_ratio + 1)
  )

# Ensure participant_id is a factor and set treatment contrasts
data$participant_id <- factor(data$participant_id)
contrasts(data$Fenestration) <- contr.treatment(n = 2, base = 1)  # "Shading" baseline

# ===== Fit CLMM Model =====
formula_str <- "Overall_view_quality ~
          Nature_ratio_log +
          predictor3_log +
          predictor4_log +
          predictor6_log * predictor6.5_log +
          Fenestration +
          (1 | participant_id)"

model_clmm <- clmm(as.formula(formula_str), data = data)
print(summary(model_clmm))

# ===== Helper: Compute probabilities from RAW inputs =====
predict_category_probs <- function(model, input_raw, outcome_levels) {
  # 1) Transform raw -> log(x+1) where needed
  input_vals <- list(
    Nature_ratio_log  = log(input_raw$Nature_ratio + 1),
    predictor3_log    = log(input_raw$predictor3 + 1),
    predictor4_log    = log(input_raw$predictor4 + 1),
    predictor6_log    = log(input_raw$predictor6 + 1),
    predictor6.5_log  = log(input_raw$predictor6.5 + 1),
    Fenestration      = input_raw$Fenestration
  )
  
  # 2) Extract fixed effects and thresholds
  coefs      <- model$beta
  thresholds <- model$Theta
  
  # 3) Build eta (includes interaction term)
  eta <- 0
  add_if_present <- function(name, value) {
    if (name %in% names(coefs)) return(value * coefs[[name]]) else return(0)
  }
  eta <- eta + add_if_present("Nature_ratio_log",  input_vals$Nature_ratio_log)
  eta <- eta + add_if_present("predictor3_log",    input_vals$predictor3_log)
  eta <- eta + add_if_present("predictor4_log",    input_vals$predictor4_log)
  eta <- eta + add_if_present("predictor6_log",    input_vals$predictor6_log)
  eta <- eta + add_if_present("predictor6.5_log",  input_vals$predictor6.5_log)
  eta <- eta + add_if_present("predictor6_log:predictor6.5_log",
                              input_vals$predictor6_log * input_vals$predictor6.5_log)
  
  # Fenestration (robust to naming)
  fen_names <- c("FenestrationClear", "Fenestration.L", "Fenestration1")
  fen_coef_name <- fen_names[fen_names %in% names(coefs)]
  if (length(fen_coef_name) == 1 && input_vals$Fenestration == "Clear") {
    eta <- eta + coefs[[fen_coef_name]]
  }
  
  # 4) Cumulative and category probabilities
  cum_probs <- plogis(thresholds - eta)  # P(Y <= k)
  probs <- numeric(length(thresholds) + 1)
  probs[1] <- cum_probs[1]
  if (length(thresholds) > 1) {
    for (i in 2:length(thresholds)) {
      probs[i] <- cum_probs[i] - cum_probs[i - 1]
    }
  }
  probs[length(probs)] <- 1 - cum_probs[length(cum_probs)]
  
  data.frame(
    Category    = factor(outcome_levels, levels = outcome_levels, ordered = TRUE),
    Probability = as.numeric(probs)
  )
}

# ===== Helper: Predicted vs Observed plot for a given Stimuli (with top-centered labels) =====
make_pred_vs_obs_plot <- function(stim_id, legend_where = c("none","bottom")) {
  legend_where <- match.arg(legend_where)
  
  # 1) Pull first row for predictors
  row1 <- data %>%
    dplyr::filter(Stimuli == stim_id) %>%
    dplyr::slice_head(n = 1)
  if (nrow(row1) == 0) stop(sprintf("No rows found for Stimuli = '%s'.", stim_id))
  
  # 2) Fenestration value (choose correct column name)
  # 2) Fenestration value (use Fenestration column directly)
  fen_value <- as.character(row1$Fenestration[1])
  
  
  # 3) Build input_raw list from FIRST row (no averaging)
  input_raw <- list(
    Nature_ratio = as.numeric(row1$Nature_ratio[1]),
    predictor3   = as.numeric(row1$predictor3[1]),
    predictor4   = as.numeric(row1$predictor4[1]),
    predictor6   = as.numeric(row1$predictor6[1]),
    predictor6.5 = as.numeric(row1$predictor6.5[1]),
    Fenestration = fen_value
  )
  
  # 4) Predicted probabilities
  plot_df <- predict_category_probs(
    model = model_clmm,
    input_raw = input_raw,
    outcome_levels = levels(data$Overall_view_quality)
  )
  
  # 5) Observed proportions for this Stimuli
  obs_df <- data %>%
    dplyr::filter(Stimuli == stim_id) %>%
    dplyr::count(Overall_view_quality, name = "n") %>%
    tidyr::complete(Overall_view_quality = levels(data$Overall_view_quality), fill = list(n = 0)) %>%
    dplyr::mutate(Probability_observed = n / sum(n)) %>%
    dplyr::transmute(Category = Overall_view_quality, Probability_observed)
  
  # 6) Merge and reshape
  plot_merge <- plot_df %>%
    dplyr::left_join(obs_df, by = "Category") %>%
    dplyr::mutate(Probability_observed = tidyr::replace_na(Probability_observed, 0)) %>%
    dplyr::rename(Predicted = Probability, Observed = Probability_observed)
  
  plot_long <- plot_merge %>%
    tidyr::pivot_longer(cols = c(Predicted, Observed),
                        names_to = "Type",
                        values_to = "Probability")
  
  # 7) Fixed category order: Very dissatisfied ... Neutral ... Very satisfied
  plot_long$Category <- factor(
    plot_long$Category,
    levels = c(
      "Very dissatisfied",
      "Dissatisfied",
      "Slightly dissatisfied",
      "Neutral",
      "Slightly satisfied",
      "Satisfied",
      "Very satisfied"
    ),
    ordered = TRUE
  )
  
  # 8) Labels + colors
  custom_labels <- c(
    "Very dissatisfied"     = "Very\ndissatisfied",
    "Dissatisfied"          = "Dissatisfied",
    "Slightly dissatisfied" = "Slightly\ndissatisfied",
    "Neutral"               = "Neutral",
    "Slightly satisfied"    = "Slightly\nsatisfied",
    "Satisfied"             = "Satisfied",
    "Very satisfied"        = "Very\nsatisfied"
  )
  my_cols <- c(Predicted = "#228833", Observed = "#BBBBBB")
  
  # 9) Plot with top-centered percent labels
  ggplot(plot_long, aes(x = Category, y = Probability, fill = Type)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.75) +
    # labels above each bar (centered)
    geom_text(
      aes(label = ifelse(Probability > 0, scales::percent(Probability, accuracy = 1), "")),
      position = position_dodge(width = 0.8),
      vjust = -0.25, size = 3
    ) +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
    # add headroom for labels & prevent clipping
    coord_cartesian(ylim = c(0, 1.05), clip = "off") +
    scale_x_discrete(labels = custom_labels) +
    scale_fill_manual(values = my_cols) +
    labs(
      x = paste0("Overall view quality (Scenario: ", stim_id, ")"),
      y = "",
      fill = ""
    ) +
    guides(fill = guide_legend(title = NULL)) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border     = element_blank(),
      axis.line.x      = element_line(color = "black", size = 0.8),
      axis.line.y      = element_line(color = "black", size = 0.8),
      legend.position  = if (legend_where == "bottom") "bottom" else "none",
      legend.margin    = margin(t = 4, r = 4, b = 4, l = 4),
      plot.margin      = margin(t = 10, r = 8, b = 8, l = 8)
    )
}

# ===== Build THREE rows (legend only on bottom) =====
top_id <- "B-BD-711"   # change as needed
mid_id <- "BD-711"      # change as needed
bot_id <- "C-714"      # change as needed

p_top <- make_pred_vs_obs_plot(top_id, legend_where = "none")
p_mid <- make_pred_vs_obs_plot(mid_id, legend_where = "none")
p_bot <- make_pred_vs_obs_plot(bot_id, legend_where = "bottom")

combined <- (p_top / p_mid / p_bot) 

# Show
print(combined)

# Save (increase height for 3 rows)
# ggsave(
#   filename = "G:/My Drive/2. Post-PhD/2. Research/4. Window View Quality Global Dataset/100. View Quality Paper/1001. Figures/8. Section 4.3 Model validation/pred_vs_obs_3rows.jpg",
#   plot = combined, width = 6, height = 8, dpi = 500
# )
