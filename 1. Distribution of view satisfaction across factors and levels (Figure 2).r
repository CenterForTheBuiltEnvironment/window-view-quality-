# =========================
# Load libraries
# =========================
library(vegan)
library(lmPerm)
library(RVAideMemoire)
library(multcomp)
library(lsr)
library(dplyr)
library(DescTools)
library(effectsize)
library(MASS)    # For polr
library(ordinal) # For clm
library(car)
library(brant)
library(effects)
library(ggplot2)
library(forcats)
library(tidyr)
library(scales)

# =========================
# Load data
# =========================
# file_path <- "H:/My Drive/2. Post-PhD/2. Research/4. Window View Quality Global Dataset/7. Interim Result/2. Data Filtering, Sorting, and Merging/C.FINAL_VERSION.csv"
#file_path <- "G:/My Drive/2. Post-PhD/2. Research/4. Window View Quality Global Dataset/7. Interim Result/2. Data Filtering, Sorting, and Merging/C.FINAL_VERSION.csv"

data <- read.csv("View_experiment.csv")
print(names(data))

# =========================
# Fix labels
# =========================
data <- data %>%
  mutate(Fenestration = case_when(
    Fenestration == "Shading" ~ "Blind down",
    Fenestration == "Clear"   ~ "Clear glazing",
    TRUE                      ~ Fenestration
  ))

# =========================
# Remove missing
# =========================
data <- na.omit(data)

# =========================
# Ensure Overall_view_quality is ordered factor from 3 to -3
# =========================
data$Overall_view_quality <- factor(
  data$Overall_view_quality,
  levels = as.character(3:-3),
  ordered = TRUE
)
unique(data$Overall_view_quality)

# =========================
# Predictor variables & facet order
# =========================
predictor_vars <- c(
  "predictor1", "predictor2", "predictor3_category", "predictor4_category",
  "predictor5_category", "predictor6_category", "predictor6.5_category", "Fenestration"
)
facet_order <- predictor_vars

# =========================
# Level order for each predictor (AFTER remapping WWR labels)
# =========================
level_orders <- list(
  predictor1        = c("1", "2", "3"),
  predictor2        = c("No-Nature", "Nature"),
  predictor3_category = c("d < 20 m", "d ≥ 20 m"),
  predictor4_category = c("d > 5.1 m", "d ≤ 5.1 m"),
  predictor5_category = c("WWR < 50%", "WWR ≥ 50%"),  # updated label order
  predictor6_category = c("θ < 54°", "θ ≥ 54°"),
  predictor6.5_category = c("θ < 54°", "θ ≥ 54°"),
  Fenestration        = c("Blind down", "Clear glazing")
)

# =========================
# Step 1: Convert to character for pivoting
# =========================
data[predictor_vars] <- lapply(data[predictor_vars], as.character)

# =========================
# Step 2: Pivot longer and compute proportions
# =========================
plot_data_long <- data %>%
  pivot_longer(cols = all_of(predictor_vars), names_to = "Predictor", values_to = "Level") %>%
  group_by(Predictor, Level, Overall_view_quality) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Predictor, Level) %>%
  mutate(
    prop  = count / sum(count),
    label = ifelse(prop >= 0.10, scales::percent(prop, accuracy = 1), "")
  ) %>%
  ungroup()

# =========================
# Remap r-threshold labels to WWR labels BEFORE applying level orders
# (handles possible variants like 'r<50%' or 'r >= 50%')
# =========================
plot_data_long <- plot_data_long %>%
  mutate(
    Level = dplyr::case_when(
      Predictor == "predictor5_category" & Level %in% c("r < 50%", "r<50%", "r< 50%", "r <50%") ~ "WWR < 50%",
      Predictor == "predictor5_category" & Level %in% c("r ≥ 50%", "r >= 50%", "r>=50%", "r ≥50%") ~ "WWR ≥ 50%",
      TRUE ~ Level
    )
  )

# =========================
# Step 3: Apply desired facet and level order
# =========================
plot_data_long$Predictor <- factor(plot_data_long$Predictor, levels = facet_order)

plot_data_long <- plot_data_long %>%
  rowwise() %>%
  mutate(Level = factor(Level, levels = level_orders[[as.character(Predictor)]])) %>%
  ungroup()

# =========================
# Recode predictor names for facet strips
# =========================
plot_data_long$Predictor <- fct_recode(
  plot_data_long$Predictor,
  "View content:\nNumber of visible layers(*)"              = "predictor1",
  "View content:\nPresence of nature(***)"                     = "predictor2",
  "View content:\nDistance between object and glazing(***)"    = "predictor3_category",
  "View access:\nObserver proxy"                          = "predictor4_category",
  "View access:\nWindow-to-wall ratio"                    = "predictor5_category",
  "View access:\nHorizontal View Angle"                   = "predictor6_category",
  "View access:\nVertical View Angle"                     = "predictor6.5_category",
  "View clarity:\nBlind position(***)"                         = "Fenestration"
)

# =========================
# Fill color scale for -3 to 3
# =========================
fill_colors <- c(
  "-3" = "#aa3f3f",
  "-2" = "#d6766b",
  "-1" = "#dcadb5",
  "0"  = "#f4eeef",
  "1"  = "#cbecc6",
  "2"  = "#98d498",
  "3"  = "#5aa773"
)

# Optional: set Arial on Windows (won't error if not available)
if (.Platform$OS.type == "windows") {
  suppressWarnings(try({
    windowsFonts(Arial = windowsFont("Arial"))
  }, silent = TRUE))
}

# =========================
# Step 4: Plot
# =========================
p <- ggplot(plot_data_long, aes(x = prop, y = fct_rev(Level), fill = Overall_view_quality)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 4) +
  facet_wrap(~ Predictor, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = fill_colors) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme_minimal(base_family = "Arial") +
  theme(
    axis.title.x  = element_blank(),
    axis.text.x   = element_blank(),
    axis.ticks.x  = element_blank(),
    axis.text.y   = element_text(size = 12),
    panel.grid    = element_blank(),
    legend.position = "none",
    strip.text    = element_text(size = 12, hjust = 0)
  )

print(p)

# =========================
# Save the figure
# =========================
# ggsave(
#   filename = "G:/My Drive/2. Post-PhD/2. Research/4. Window View Quality Global Dataset/100. View Quality Paper/1001. Figures/3. Figure 2 Bar graphs showing differences in view satisfaction/Figure2.jpg",
#   plot = p,
#   width = 8.5, height = 8, dpi = 500, units = "in"
# )
