# ===== Libraries =====
library(ggplot2)
library(dplyr)
library(readr)
library(forcats)
library(scales)
library(stringr)

# ===== File paths =====
#file_path <- "G:/My Drive/2. Post-PhD/2. Research/4. Window View Quality Global Dataset/7. Interim Result/2. Data Filtering, Sorting, and Merging/C.FINAL_VERSION.csv"
save_path <- "G:/My Drive/2. Post-PhD/2. Research/4. Window View Quality Global Dataset/100. View Quality Paper/1001. Figures/6. Section 4.2 Observer proxy/Proxy.jpg"

# ===== Load data =====
data <- read.csv("View_experiment.csv")
data <- na.omit(data)

# ===== Filter, drop NA, & preprocess =====
filtered_data <- data %>%
  filter(Floor == 7) %>%
  select(
    Observer_Distance = predictor4,
    WWR = predictor5,
    Shading_Color = predictor7,
    Overall_view_quality
  ) %>%
  filter(!is.na(Observer_Distance),
         !is.na(WWR),
         !is.na(Shading_Color),
         !is.na(Overall_view_quality)) %>%
  mutate(across(c(Observer_Distance, WWR, Shading_Color), as.character),
         Clarity_WWR = paste0(Shading_Color, "\n", WWR))

# ===== Set factor levels (keep your original order) =====
filtered_data$Clarity_WWR <- factor(
  filtered_data$Clarity_WWR,
  levels = c("100\n64", "5.6\n64", "5.6\n32")
)
filtered_data$Observer_Distance <- factor(
  filtered_data$Observer_Distance,
  levels = c("1.4", "2.3", "5.1")
)

# ===== Reverse OVQ order so 'Very satisfied' is left =====
ovq_levels <- as.character(3:-3)  # 3 first, -3 last
ovq_labels <- c("Very satisfied","Satisfied","Slightly satisfied",
                "Neutral","Slightly dissatisfied","Dissatisfied","Very dissatisfied")

filtered_data <- filtered_data %>%
  mutate(
    OVQ_num = as.numeric(as.character(Overall_view_quality)),
    OVQ_fac = factor(as.character(Overall_view_quality),
                     levels = ovq_levels, ordered = TRUE,
                     labels = ovq_labels)
  )

# ===== Row order by mean score (desc) =====
combo_order <- filtered_data %>%
  mutate(Combo = paste0(Observer_Distance, " m\n", Clarity_WWR)) %>%
  group_by(Combo) %>%
  summarise(mean_score = mean(OVQ_num, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(mean_score)) %>%
  pull(Combo)

# ===== Aggregate to proportions =====
plot_df <- filtered_data %>%
  mutate(Combo = paste0(Observer_Distance, " m\n", Clarity_WWR),
         Combo = factor(Combo, levels = combo_order)) %>%
  group_by(Combo, OVQ_fac) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  mutate(label = ifelse(prop >= 0.10, percent(prop, accuracy = 1), ""))

# ===== Helper: remap y-axis tick labels with line breaks =====
label_map <- function(vals) {
  sapply(vals, function(v) {
    parts <- strsplit(as.character(v), "\n")[[1]]
    od  <- parts[1]  # Observer Distance
    scv <- parts[2]  # Shading_Color
    wwv <- parts[3]  # WWR
    
    # Map shading/clarity
    sc_label <- ifelse(scv == "100", "Clear", "Blind down")
    
    # Map WWR
    wwr_label <- paste0("WWR of ", wwv, "%")
    
    # Return label with line breaks between each part
    paste0(od, "\n", sc_label, "\n", wwr_label)
  }, USE.NAMES = FALSE)
}


# ===== Color palette (reversed order) =====
fill_colors <- c(
  "Very satisfied"        = "#5aa773",
  "Satisfied"             = "#98d498",
  "Slightly satisfied"    = "#cbecc6",
  "Neutral"               = "#f4eeef",
  "Slightly dissatisfied" = "#dcadb5",
  "Dissatisfied"          = "#d6766b",
  "Very dissatisfied"     = "#aa3f3f"
)

# ===== Plot =====
if (.Platform$OS.type == "windows") {
  suppressWarnings(try(windowsFonts(Arial = windowsFont("Arial")), silent = TRUE))
}

p <- ggplot(plot_df, aes(x = prop, y = forcats::fct_rev(Combo), fill = OVQ_fac)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3.8) +
  scale_x_continuous(labels = percent_format(accuracy = 10)) +
  scale_y_discrete(labels = label_map) +  # <— remap y tick labels here
  scale_fill_manual(values = fill_colors, drop = FALSE) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_family = "Arial") +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_blank(),    # no x-axis labels
    axis.ticks.x = element_blank(),   # no x-axis ticks
    axis.text.y = element_text(size = 11),
    panel.grid = element_blank(),
    legend.position = "none"
  )

print(p)

# ===== Save =====
# ggsave(
#   filename = save_path,
#   plot = p,
#   width = 8, height = 8, dpi = 500, units = "in"
# )


#----------------


# ==== Sequential pairwise MWU based on mean OVQ (adjacent ranks) ====
library(effsize)   # cliff.delta
library(openxlsx)  # Excel
library(purrr)
library(tibble)
library(dplyr)
library(stringr)

# Helper: R guideline star mapping
p_to_stars <- function(p) {
  if (is.na(p)) return("NA")
  if (p <= 0.001) "***"
  else if (p <= 0.01) "**"
  else if (p <= 0.05) "*"
  else "ns"
}

# Build a compact ID for groups (for subsetting) and keep numeric OVQ
fd_seq <- filtered_data %>%
  mutate(
    combined_category = interaction(Clarity_WWR, Observer_Distance, sep = "_"), # e.g., "100\n64_1.4"
    OVQ_num = as.numeric(as.character(Overall_view_quality))
  ) %>%
  filter(!is.na(OVQ_num), !is.na(combined_category))

# Compute mean OVQ per group and rank (high → low)
rank_tbl <- fd_seq %>%
  group_by(combined_category) %>%
  summarise(mean_OVQ = mean(OVQ_num, na.rm = TRUE),
            n = n(), .groups = "drop") %>%
  arrange(desc(mean_OVQ)) %>%
  mutate(rank = row_number())

# Build adjacent pairs: (1 vs 2), (2 vs 3), ...
pairs_adjacent <- if (nrow(rank_tbl) >= 2) {
  map2(rank_tbl$combined_category[-nrow(rank_tbl)],
       rank_tbl$combined_category[-1],
       ~c(.x, .y))
} else list()

# Pretty label for Excel readability
pretty_label <- function(cc) {
  # cc like "100\n64_1.4"
  parts <- strsplit(cc, "_", fixed = TRUE)[[1]]
  cw <- parts[1]     # "100\n64"
  od <- parts[2]     # "1.4"
  cw_parts <- strsplit(cw, "\n", fixed = TRUE)[[1]]
  scv <- cw_parts[1] # "100" or "5.6"
  wwv <- cw_parts[2] # "64" or "32"
  sc_label <- ifelse(scv == "100", "Clear", "Blind down")
  paste0(od, " m — ", sc_label, ", WWR of ", wwv, "%")
}

# Test runner for one adjacent pair
run_mwu_cliff <- function(df, g1, g2) {
  x <- df %>% filter(combined_category == g1) %>% pull(OVQ_num)
  y <- df %>% filter(combined_category == g2) %>% pull(OVQ_num)
  
  if (length(x) == 0 || length(y) == 0) {
    return(tibble(
      rank1 = NA_integer_, group1 = g1, mean1 = NA_real_, n1 = length(x),
      rank2 = NA_integer_, group2 = g2, mean2 = NA_real_, n2 = length(y),
      W = NA_real_, p_value = NA_real_, stars = "NA",
      cliffs_delta = NA_real_, magnitude = NA_character_,
      ci_low = NA_real_, ci_high = NA_real_
    ))
  }
  
  wt <- wilcox.test(x, y, alternative = "two.sided", exact = FALSE)
  cd <- effsize::cliff.delta(x, y, conf.level = 0.95)
  
  # Pull ranks & means from rank_tbl
  r1 <- rank_tbl %>% filter(combined_category == g1)
  r2 <- rank_tbl %>% filter(combined_category == g2)
  
  tibble(
    rank1 = r1$rank, group1 = g1, mean1 = r1$mean_OVQ, n1 = r1$n,
    rank2 = r2$rank, group2 = g2, mean2 = r2$mean_OVQ, n2 = r2$n,
    W = unname(wt$statistic),
    p_value = wt$p.value,
    stars = p_to_stars(wt$p.value),
    cliffs_delta = unname(cd$estimate),
    magnitude = as.character(cd$magnitude),
    ci_low = unname(cd$conf.int[1]),
    ci_high = unname(cd$conf.int[2])
  )
}

# Run all adjacent comparisons
results_seq <- if (length(pairs_adjacent) > 0) {
  map_dfr(pairs_adjacent, ~ run_mwu_cliff(fd_seq, .x[1], .x[2])) %>%
    arrange(rank1)
} else {
  tibble(
    rank1 = integer(), group1 = character(), mean1 = double(), n1 = integer(),
    rank2 = integer(), group2 = character(), mean2 = double(), n2 = integer(),
    W = double(), p_value = double(), stars = character(),
    cliffs_delta = double(), magnitude = character(),
    ci_low = double(), ci_high = double()
  )
}

# Add human-friendly labels (single column per group)
results_seq <- results_seq %>%
  mutate(
    group1 = as.character(group1),
    group2 = as.character(group2),
    group1_label = vapply(as.character(group1), pretty_label, character(1)),
    group2_label = vapply(as.character(group2), pretty_label, character(1))
  ) %>%
  relocate(group1_label, .after = group1) %>%
  relocate(group2_label, .after = group2)

# Preview in console
print(rank_tbl)
print(results_seq)

#

# ---- Save to Excel (same folder as your script using here) ----
excel_out <- here::here("MWU_SequentialPairs_results (Figure 5).xlsx")

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "Ranking")
openxlsx::addWorksheet(wb, "Adjacent MWU")

openxlsx::writeData(wb, "Ranking", rank_tbl)
openxlsx::writeData(wb, "Adjacent MWU", results_seq)

openxlsx::saveWorkbook(wb, excel_out, overwrite = TRUE)

# Open folder (Windows)
browseURL(dirname(excel_out))

cat("Excel file saved to:", excel_out, "\n")
