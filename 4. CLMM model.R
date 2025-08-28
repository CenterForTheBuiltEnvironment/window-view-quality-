# ===== Load Libraries =====
library(vegan)
library(lmPerm)
library(RVAideMemoire)
library(multcomp)
library(lsr)
library(dplyr)
library(DescTools)
library(effectsize)
library(effsize)
library(openxlsx)
library(lme4)
library(ordinal)
library(ggeffects)
library(ggplot2)
library(insight)
library(scales)
library(patchwork)


# ===== Read and Preprocess Data =====
#file_path <- "H:/My Drive/2. Post-PhD/2. Research/4. Window View Quality Global Dataset/7. Interim Result/2. Data Filtering, Sorting, and Merging/C.FINAL_VERSION.csv"
#file_path <- "G:/My Drive/2. Post-PhD/2. Research/4. Window View Quality Global Dataset/7. Interim Result/2. Data Filtering, Sorting, and Merging/C.FINAL_VERSION.csv"


data <- read.csv("View_experiment.csv")
print(names(data))

data <- na.omit(data)
colnames(data)[47] <- "predictor6.5"
colnames(data)[9] <- "participant_id"

data <- data |>
  mutate(
    # Ordered categorical variables
    predictor1_category   = factor(predictor1, levels = c("1", "2", "3"), ordered = TRUE),
    predictor3_category   = factor(predictor3_category, levels = c("d < 20 m", "d ≥ 20 m"), ordered = TRUE),
    predictor4_category   = factor(predictor4_category, levels = c("d > 5.1 m", "d ≤ 5.1 m"), ordered = TRUE),
    predictor5_category   = factor(predictor5_category, levels = c("r < 50%", "r ≥ 50%"), ordered = TRUE),
    predictor6_category   = factor(predictor6_category, levels = c("θ < 54°", "θ ≥ 54°"), ordered = TRUE),
    predictor6.5_category = factor(predictor6.5_category, levels = c("θ < 54°", "θ ≥ 54°"), ordered = TRUE),
    Fenestration_category = factor(Fenestration, levels = c("Shading", "Clear"), ordered = TRUE),
    predictor2 = factor(predictor2, levels = c("No-Nature", "Nature"), ordered = TRUE),
    Fenestration = factor(Fenestration, levels = c("Shading", "Clear"), ordered = TRUE),
    # Convert to numeric
    across(c(Greenery_ratio, Sky_ratio, Nature_ratio,
             predictor3, predictor4, predictor5, predictor6, predictor6.5),
           ~ as.numeric(as.character(.)))
  )

data$Overall_view_quality <- factor(data$Overall_view_quality,
                                    levels = c("-3", "-2", "-1", "0", "1", "2", "3"),
                                    labels = c("Very dissatisfied", "Dissatisfied", "Slightly dissatisfied",
                                               "Neutral", "Slightly satisfied", "Satisfied", "Very satisfied"),
                                    ordered = TRUE)
print(unique(data$Stimuli))
# ===== Log-transformed Predictors =====
data <- data |>
  mutate(
    predictor3_log   = log(predictor3 + 1),
    predictor4_log   = log(predictor4 + 1),
    predictor5_log   = log(predictor5 + 1),
    predictor6_log   = log(predictor6 + 1),
    predictor6.5_log = log(predictor6.5 + 1),
    Greenery_ratio_log = log(Greenery_ratio + 1),
    Sky_ratio_log       = log(Sky_ratio + 1),
    Nature_ratio_log    = log(Nature_ratio + 1)
  )

## Section 3.2 model building

table(data$predictor1)
table(data$predictor5_category)
table(data$predictor5)
table(data$Overall_view_quality)

outcome<-"6"
outcome

levels(data$Fenestration)
is.ordered(data$Fenestration)
class(data$Fenestration)

#===== Fit CLMM Model =====
formula_str <- "Overall_view_quality ~
          Nature_ratio_log +
          predictor3_log +
          predictor4_log +
          predictor6_log * predictor6.5_log +
          Fenestration +
          (1 | participant_id)"
# 
model_clmm <- clmm(as.formula(formula_str), data = data)
summary(model_clmm)


#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
####
library(performance)
r2(model_clmm)

##
# Predict
pred <- ggpredict(model_clmm, terms = "Nature_ratio_log")

# Plot
plot(pred)

# ===== Extract Predictor Names =====
all_terms <- attr(terms(model_clmm), "term.labels")
predictor_names <- unique(unlist(strsplit(all_terms, ":")))

# ===== Create Prediction + Observed Data Overlay =====
output_path <- "G:/My Drive/2. Post-PhD/2. Research/4. Window View Quality Global Dataset/100. View Quality Paper/1001. Figures/5. Section 3.2 Development of CLMM model"
#
unique(data$Overall_view_quality)

#First plot
unique(data$Nature_ratio)

# Focus on "Nature_ratio_log" as the predictor
predicted_probs <- ggpredict(model_clmm, terms = c("Nature_ratio_log [all]"))

# ===== Filter only "Satisfied" level =====
# ===== Predict probabilities using ggpredict =====

# Focus on "Nature_ratio_log" as the predictor
predicted_probs <- ggpredict(model_clmm, terms = c("Nature_ratio_log [all]", "Fenestration"))
# ===== Filter only "Satisfied" level =====
satisfied_probs <- predicted_probs %>%
  filter(response.level == outcome)

unique(data$Fenestration)

my_cols <- c(
  "Clear"   = "#003262",
  "Shading" = "#BBBBBB"
)
p1<-ggplot(satisfied_probs,
       aes(x = x, y = predicted, colour = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.20, colour = NA) +
  scale_x_continuous(
    breaks  = log(c(10,25,40) + 1),
    labels  = c("10%","25%",  "40%")
  ) +
  scale_y_continuous(
    labels  = scales::label_percent(accuracy = 1),
    limits  = c(-0.01, 1)  # Set shared y-axis
  ) +
  scale_colour_manual(values = my_cols) +
  scale_fill_manual(values  = my_cols) +
  labs(
    x = "Ratio of nature elements",
    y = "Probability of being 'Satisfied'"
  ) +
  theme(
    legend.position   = "none",
    panel.grid        = element_blank(),
    panel.background  = element_blank(),   # <-- no panel fill
    plot.background   = element_blank(),   # <-- no plot fill
    axis.text.x       = element_text(size = 14),
    axis.text.y       = element_text(size = 14),
    axis.ticks        = element_line(),
    axis.ticks.length = unit(0.2, "cm"),
    axis.line.x       = element_line(colour = "black", linewidth = 0.5),
    axis.line.y       = element_line(colour = "black", linewidth = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )
print(p1)
####

# Focus on "predictor3_log" as the predictor
predicted_probs <- ggpredict(model_clmm, terms = c("predictor3_log [all]", "Fenestration"))

# ===== Filter only "Satisfied" level =====
satisfied_probs <- predicted_probs %>%
  filter(response.level == outcome)

# Define colors (reuse or adjust as needed)

unique(data$predictor3)
unique(data$predictor3_log)

##
library(ggeffects)
log_7.5 <- log(8.5)
log(21)

# Specify exact value in terms
pred_7.5 <- ggpredict(model_clmm, terms = c(paste0("predictor3_log [", log_7.5, "]"), "Fenestration"))

# Filter for your outcome
pred_7.5 %>% filter(response.level == "6")
##

##
# Plot
p2<-ggplot(satisfied_probs,
       aes(x = x, y = predicted, colour = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.20, colour = NA) +
  scale_x_continuous(
    breaks  = log(c(3,7.5,20,30) + 1),
    labels  = c("3 m","7.5 m",  "20 m","30 m")
  ) +
  scale_y_continuous(
    labels  = scales::label_percent(accuracy = 1),
    limits  = c(-0.01, 1)  # Set shared y-axis
  ) +
  scale_colour_manual(values = my_cols) +
  scale_fill_manual(values  = my_cols) +
  labs(
    x = "Object-to-glazing distance",
    y = " "
  ) +
  theme(
    legend.position   = "none",
    panel.grid        = element_blank(),
    panel.background  = element_blank(),
    plot.background   = element_blank(),
    axis.text.x       = element_text(size = 14),
    axis.text.y       = element_text(size = 14),
    axis.ticks        = element_line(),
    axis.ticks.length = unit(0.2, "cm"),
    axis.line.x       = element_line(colour = "black", linewidth = 0.5),
    axis.line.y       = element_line(colour = "black", linewidth = 0.5),
    axis.title.x = element_text(size = 14)
  )

####
print(p2)
#

#
unique(data$predictor4)

# Focus on "predictor4_log" as the predictor
predicted_probs <- ggpredict(model_clmm, terms = c("predictor4_log [all]", "Fenestration"))


# Extract both levels
satisfied_probs    <- predicted_probs %>% filter(response.level == "6")
very_satisfied_probs <- predicted_probs %>% filter(response.level == "7")

# Plot
p4 <- ggplot() +
  # Line for Satisfied (6)
  geom_line(data = satisfied_probs, aes(x = x, y = predicted, color = group), size = 1.2, linetype = "solid") +
  geom_ribbon(data = satisfied_probs, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  
  # Line for Very Satisfied (7)
  geom_line(data = very_satisfied_probs, aes(x = x, y = predicted, color = group), size = 1.2, linetype = "dashed") +
  
  # Axis formatting
  scale_x_continuous(
    breaks = log(c(0.9, 2.3, 5.1, 11) + 1),
    labels = c("0.9 m", "2.3 m", "5.1 m", "11 m")
  ) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1),
    limits = c(-0.01, 1)
  ) +
  scale_colour_manual(values = my_cols) +
  scale_fill_manual(values = my_cols) +
  labs(
    x = "Observer proxy",
    y = "Probability of being 'Satisfied' or 'Very satisfied'"
  ) +
  theme(
    legend.position   = "none",
    panel.grid        = element_blank(),
    panel.background  = element_blank(),
    plot.background   = element_blank(),
    axis.text.x       = element_text(size = 14),
    axis.text.y       = element_text(size = 14),
    axis.ticks        = element_line(),
    axis.ticks.length = unit(0.2, "cm"),
    axis.line.x       = element_line(colour = "black", linewidth = 0.5),
    axis.line.y       = element_line(colour = "black", linewidth = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

print(p4)
####
unique(data$predictor6)

unique(data$Overall_view_quality)

# Focus on "predictor6_log" as the predictor
predicted_probs <- ggpredict(model_clmm, terms = c("predictor6_log [all]", "Fenestration"))
predicted_probs
# Filter only "Satisfied" level
satisfied_probs <- predicted_probs %>% filter(response.level == outcome)

# Plot for predictor6_log
p6 <- ggplot(satisfied_probs, aes(x = x, y = predicted, colour = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.20, colour = NA) +
  scale_x_continuous(
    breaks = log(c(5, 13, 36,84) + 1),  # ← adjust depending on range of `predictor6`
    labels = c("5°", "13°", "36°", "84°")
  ) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1),
    limits = c(-0.01, 1)
  ) +
  scale_colour_manual(values = my_cols) +
  scale_fill_manual(values  = my_cols) +
  labs(
    x = "Horizontal sight angle",
    y = " "
  ) +
  theme(
    legend.position   = "none",
    panel.grid        = element_blank(),
    panel.background  = element_blank(),
    plot.background   = element_blank(),
    axis.text.x       = element_text(size = 14),
    axis.text.y       = element_text(size = 14),
    axis.ticks        = element_line(),
    axis.ticks.length = unit(0.2, "cm"),
    axis.line.x       = element_line(colour = "black", linewidth = 0.5),
    axis.line.y       = element_line(colour = "black", linewidth = 0.5),
    axis.title.x = element_text(size = 14)
  )




combined_plot_2x2 <- (p1 + p2) / (p4 + p6)

# ===== Save Combined Plot =====

file_path<-file.path(output_path, "CLMM model result by factors-advanced.jpg")
file_path
#ggsave(filename = file_path, plot = combined_plot_2x2, width = 8, height = 8, dpi = 600)

####

#### Left: Clear Right: Blinds down 

library(ggeffects)
library(dplyr)

# Step 1: Get predictions across the interaction
interaction_plot <- ggpredict(model_clmm, terms = c("predictor6_log", "predictor6.5_log"))

# Step 2: Check which levels are available
unique(interaction_plot$response.level)  # optional, to confirm the naming

# Step 3: Filter to only show predicted probability of being "Satisfied"
filtered_plot <- interaction_plot |>
  filter(response.level == "6")

# Step 4: Plot
ggplot(filtered_plot, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group, group = group),
              alpha = 0.2, color = NA) +
  labs(
    x = "predictor6_log",
    y = "Probability of being 'Satisfied'",
    color = "predictor6.5_log",
    fill  = "predictor6.5_log"
  ) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  theme_minimal()


###

fenestration_level <- "Clear"  # Replace with your actual level

objective_function <- function(x) {
  log_val <- log(x + 1)  # Apply the same transformation used in your data
  pred <- ggpredict(model_clmm, terms = c(paste0("predictor3_log [", log_val, "]"), "Fenestration [Clear]"))
  prob <- pred %>% filter(response.level == "6") %>% pull(predicted)
  return(prob - 0.5)
}

# Run uniroot on the original scale
result <- uniroot(objective_function, lower = 0, upper = 30)  # adjust range based on your data
solution <- result$root
solution

### What is the ideal observer proximity to the windows? to make people 50% satisfied with the view? 

# Apply log(+1) transformation for the desired value (e.g., 8.5 meters)
log_8.5 <- log(3.5 + 1)

# Predict using predictor4_log instead of predictor3_log
pred_8.5 <- ggpredict(model_clmm, terms = c(paste0("predictor4_log [", log_8.5, "]"), "Fenestration"))

# Filter to show probability of response level 6
pred_8.5 %>% filter(response.level == "6")


# Apply log(+1) transformation for the desired value (e.g., 8.5 meters)
log_8.5 <- log(8.5 + 1)

# Predict using predictor4_log instead of predictor3_log
pred_8.5 <- ggpredict(model_clmm, terms = c(paste0("predictor4_log [", log_8.5, "]"), "Fenestration"))

# Filter to show probability of response level 6
pred_8.5 %>% filter(response.level == "6")


## What is the presence of nature ratio?

### What is the ideal observer proximity to the windows? to make people 50% satisfied with the view? 

# Apply log(+1) transformation for the desired value (e.g., 8.5 meters)
log_8.5 <- log(35 + 1)

# Predict using predictor4_log instead of predictor3_log
pred_8.5 <- ggpredict(model_clmm, terms = c(paste0("Nature_ratio_log [", log_8.5, "]"), "Fenestration"))

# Filter to show probability of response level 6
pred_8.5 %>% filter(response.level == "6")

## What is the object-to-glazing distance??

### What is the ideal observer proximity to the windows? to make people 50% satisfied with the view? 

# Apply log(+1) transformation for the desired value (e.g., 8.5 meters)
log_8.5 <- log(17 + 1)

# Predict using predictor4_log instead of predictor3_log
pred_8.5 <- ggpredict(model_clmm, terms = c(paste0("predictor3_log [", log_8.5, "]"), "Fenestration"))

# Filter to show probability of response level 6
pred_8.5 %>% filter(response.level == "6")


