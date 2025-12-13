#!/usr/bin/env Rscript
# ============================================================
# Shiny GUI: Joint prediction for One Scenario
# - Inputs: Nature_ratio, predictor3, predictor4, predictor6,
#           predictor6.5, Fenestration
# - Output (right panel): y1..y7 probabilities + Expected Y
#   for that exact combination
# ============================================================

library(shiny)
library(dplyr)
library(ggeffects)
library(ordinal)   # clmm

# ============================================================
# 1) DATA + MODEL SETUP (runs once when app starts)
# ============================================================

file_path <- "I:/My Drive/2. Post-PhD/2. Research/4. Window View Quality Global Dataset/7. Interim Result/2. Data Filtering, Sorting, and Merging/C.FINAL_VERSION.csv"
file_path <- "G:/My Drive/2. Post-PhD/2. Research/4. Window View Quality Global Dataset/7. Interim Result/2. Data Filtering, Sorting, and Merging/C.FINAL_VERSION.csv"


data <- read.csv(file_path)
data <- na.omit(data)
colnames(data)[9] <- "participant_id"

data <- data |>
  dplyr::mutate(
    predictor1_category   = factor(predictor1, levels = c("1", "2", "3"), ordered = TRUE),
    predictor3_category   = factor(predictor3_category, levels = c("d < 20 m", "d ≥ 20 m"), ordered = TRUE),
    predictor4_category   = factor(predictor4_category, levels = c("d > 5.1 m", "d ≤ 5.1 m"), ordered = TRUE),
    predictor5_category   = factor(predictor5_category, levels = c("r < 50%", "r ≥ 50%"), ordered = TRUE),
    predictor6_category   = factor(predictor6_category, levels = c("θ < 54°", "θ ≥ 54°"), ordered = TRUE),
    predictor6.5_category = factor(predictor6.5_category, levels = c("θ < 54°", "θ ≥ 54°"), ordered = TRUE),
    Fenestration_category = factor(Fenestration, levels = c("Shading", "Clear"), ordered = TRUE),
    predictor2            = factor(predictor2, levels = c("No-Nature", "Nature"), ordered = TRUE),
    Fenestration          = factor(Fenestration, levels = c("Shading", "Clear"), ordered = TRUE),
    dplyr::across(
      c(Greenery_ratio, Sky_ratio, Nature_ratio,
        predictor3, predictor4, predictor5, predictor6, predictor6.5),
      ~ as.numeric(as.character(.))
    )
  )

data$Overall_view_quality <- factor(
  data$Overall_view_quality,
  levels = c("-3", "-2", "-1", "0", "1", "2", "3"),
  labels = c("Very dissatisfied", "Dissatisfied", "Slightly dissatisfied",
             "Neutral", "Slightly satisfied", "Satisfied", "Very satisfied"),
  ordered = TRUE
)

data <- data |>
  dplyr::mutate(
    predictor3_log      = log(predictor3 + 1),
    predictor4_log      = log(predictor4 + 1),
    predictor5_log      = log(predictor5 + 1),
    predictor6_log      = log(predictor6 + 1),
    predictor6.5_log    = log(predictor6.5 + 1),
    Greenery_ratio_log  = log(Greenery_ratio + 1),
    Sky_ratio_log       = log(Sky_ratio + 1),
    Nature_ratio_log    = log(Nature_ratio + 1)
  )

formula_str <- Overall_view_quality ~
  Nature_ratio_log +
  predictor3_log +
  predictor4_log +
  predictor6_log * predictor6.5_log +
  Fenestration +
  (1 | participant_id)

model_clmm <- clmm(formula_str, data = data)
summary(model_clmm)
# Mapping ordinal categories y1..y7 → -3..3
raw_y_values <- c(-3, -2, -1, 0, 1, 2, 3)

# Fenestration levels
fen_levels <- levels(data$Fenestration)

# ============================================================
# 2) SHINY UI
# ============================================================

ui <- fluidPage(
  titlePanel("CLMM – Joint Prediction for One Scenario"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("nature_raw", "Nature ratio (%)", value = 30, min = 0, max = 100, step = 1),
      numericInput("pred3_raw", "Object-to-glazing distance (m) (predictor3)", 
                   value = 15, min = 0, step = 0.1),
      numericInput("pred4_raw", "Observer-to-glazing distance (m) (predictor4)", 
                   value = 3.5, min = 0, step = 0.1),
      numericInput("pred6_raw", "Horizontal sight angle (°) (predictor6)", 
                   value = 41, min  = 0, step = 0.1),
      numericInput("pred65_raw", "Vertical sight angle (°) (predictor6.5)", 
                   value = 35, min = 0, step = 0.1),
      selectInput("fen_level", "Fenestration", choices = fen_levels, selected = "Clear"),
      actionButton("run_btn", "Run prediction", class = "btn-primary")
    ),
    
    mainPanel(
      h3("Current scenario"),
      verbatimTextOutput("scenario_text"),
      tags$hr(),
      
      h4("Category probabilities (y1..y7)"),
      tableOutput("tbl_probs"),
      tags$hr(),
      
      h4("Expected Y (on -3..3 scale)"),
      tableOutput("tbl_expectedY"),
      tags$hr(),
      
      h4("Messages"),
      verbatimTextOutput("status_text"),
      
      tags$hr(),
      helpText("y1..y7 correspond to:",
               "y1 = Very dissatisfied (-3),",
               "y2 = Dissatisfied (-2),",
               "y3 = Slightly dissatisfied (-1),",
               "y4 = Neutral (0),",
               "y5 = Slightly satisfied (1),",
               "y6 = Satisfied (2),",
               "y7 = Very satisfied (3).")
    )
  )
)

# ============================================================
# 3) SHINY SERVER
# ============================================================

server <- function(input, output, session) {
  
  output$scenario_text <- renderText({
    paste0(
      "Fenestration = ", input$fen_level, "\n",
      "Nature ratio = ", input$nature_raw, "%\n",
      "predictor3 (object distance) = ", input$pred3_raw, " m\n",
      "predictor4 (observer distance) = ", input$pred4_raw, " m\n",
      "predictor6 (horizontal angle) = ", input$pred6_raw, "°\n",
      "predictor6.5 (vertical angle) = ", input$pred65_raw, "°"
    )
  })
  
  observeEvent(input$run_btn, {
    req(input$nature_raw, input$pred3_raw, input$pred4_raw,
        input$pred6_raw, input$pred65_raw, input$fen_level)
    
    preds <- tryCatch(
      {
        ggpredict(
          model_clmm,
          terms = c(paste0("Nature_ratio_log [", log(input$nature_raw + 1), "]")),
          condition = c(
            predictor3_log   = log(input$pred3_raw + 1),
            predictor4_log   = log(input$pred4_raw + 1),
            predictor6_log   = log(input$pred6_raw + 1),
            predictor6.5_log = log(input$pred65_raw + 1),
            Fenestration     = input$fen_level
          )
        )
      },
      error = function(e) {
        output$status_text <- renderText(
          paste("Error from ggpredict():", e$message)
        )
        return(NULL)
      }
    )
    
    if (is.null(preds)) return(NULL)
    
    df <- as.data.frame(preds)
    
    n_cat <- nrow(df)
    if (n_cat != length(raw_y_values)) {
      output$status_text <- renderText(
        paste0("Unexpected number of categories: ", n_cat,
               " (expected ", length(raw_y_values), ").")
      )
      return(NULL)
    }
    
    prob_df <- df %>%
      dplyr::mutate(
        y        = paste0("y", dplyr::row_number()),
        category = response.level,
        prob     = predicted,
        raw_y    = raw_y_values[dplyr::row_number()]
      ) %>%
      dplyr::select(y, category, prob, raw_y)
    
    expected_Y <- sum(prob_df$prob * prob_df$raw_y)
    expected_df <- data.frame(
      Fenestration = input$fen_level,
      Expected_Y   = expected_Y
    )
    
    # Convert probabilities to percentage (0–100)
    prob_df$prob <- round(prob_df$prob * 100, 0)
    
    output$tbl_probs     <- renderTable(prob_df, digits = 0)
    output$tbl_expectedY <- renderTable(expected_df, digits = 4)
    output$status_text   <- renderText("Prediction updated.")
    
  })
}

# ============================================================
# 4) RUN APP
# ============================================================

shinyApp(ui, server)
