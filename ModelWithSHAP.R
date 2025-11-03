library(data.table)
library(broom)
library(ResourceSelection)
library(DT)
library(MASS)
library(ggplot2)
library(ggpubr)
library(caTools)
library(pROC)
library(vip)
library(caret)
library(readr) 
library(fastshap)
library(plotly)

# -------------------------------
# Load dataset
# -------------------------------

df <- read_csv("./thyroid_diff.csv")
setDT(df)

# Convert character columns to factors
cols_to_factor <- names(df)[sapply(df, is.character)]
df[, (cols_to_factor) := lapply(.SD, as.factor), .SDcols = cols_to_factor]

summary(df)

# Re-leveling descriptive factors
df$Gender <- relevel(df$Gender, ref = "F")
df$Smoking <- relevel(df$Smoking, ref = "No")
df$`Hx Smoking` <- relevel(df$`Hx Smoking`, ref = "No")
df$`Hx Radiothreapy` <- relevel(df$`Hx Radiothreapy`, ref = "No")

# 'Euthyroid' is the most common state
df[, Thyroid_Function_Simple := fcase(
  `Thyroid Function` == "Euthyroid", "Euthyroid",
  `Thyroid Function` %like% "Hypo", "Hypothyroidism",
  `Thyroid Function` %like% "Hyper", "Hyperthyroidism"
)]
df$Thyroid_Function_Simple <- as.factor(df$Thyroid_Function_Simple)
df$Thyroid_Function_Simple <- relevel(
  df$Thyroid_Function_Simple,
  ref = "Euthyroid"
)

# 'Normal' is the logical clinical baseline
df[, Physical_Exam_Simple := fcase(
  `Physical Examination` == "Normal", "Normal",
  `Physical Examination` %like% "Single nodular", "Single Nodule",
  `Physical Examination` == "Multinodular goiter", "Multinodular Goiter",
  `Physical Examination` == "Diffuse goiter", "Diffuse Goiter"
)]
df$Physical_Exam_Simple <- as.factor(df$Physical_Exam_Simple)
df$Physical_Exam_Simple <- relevel(df$Physical_Exam_Simple, ref = "Normal")

# 'No' involvement is the logical baseline
df[, Adenopathy_Binary := fcase(
  Adenopathy == "No", "No",
  default = "Yes"
)]
df$Adenopathy_Binary <- as.factor(df$Adenopathy_Binary)
df$Adenopathy_Binary <- relevel(df$Adenopathy_Binary, ref = "No")

# 'Papillary' is the most common type
# https://www.ncbi.nlm.nih.gov/books/NBK536943/
df[, Pathology_Simple := fcase(
  Pathology == "Follicular", "Follicular",
  Pathology == "Hurthel cell", "Hurthel cell",
  Pathology == "Papillary", "Papillary",
  Pathology == "Micropapillary", "Papillary"
)]
df$Pathology_Simple <- as.factor(df$Pathology_Simple)
df$Pathology_Simple <- relevel(df$Pathology_Simple, ref = "Papillary")

# Focality
df$Focality <- relevel(df$Focality, ref = "Uni-Focal")

# 'Low' is the logical baseline and most common
df$Risk <- relevel(df$Risk, ref = "Low")

# 'T1a' is the lowest/earliest T stage
df$T <- relevel(df$T, ref = "T1a")

# 'N0' is no node involvement, the logical baseline
df$N <- relevel(df$N, ref = "N0")

# 'M0' is no metastasis, the logical baseline
df$M <- relevel(df$M, ref = "M0")

# 'I' is the earliest stage and the logical baseline
df$Stage <- relevel(df$Stage, ref = "I")

# 'Excellent' is the logical baseline
df$Response <- relevel(df$Response, ref = "Excellent")

# Set 'No' as the baseline for 'Recurred'
df$Recurred <- relevel(df$Recurred, ref = "No")


# -------------------------------
# Split data
# -------------------------------
set.seed(42)
split <- sample.split(df$Recurred, SplitRatio = 0.8)
df_train <- df[split == TRUE, ]
df_test <- df[split == FALSE, ]

# -------------------------------
# Define model 
# -------------------------------
formula <- Recurred ~ Age + Gender +
  Adenopathy_Binary + Pathology_Simple + Focality

# -------------------------------
# Fit model
# -------------------------------
model <- glm(
  formula,
  data = df_train,
  family = binomial(link = "logit")
)

# ----------------------------------------
# Age vs. Recurrence, split by Adenopathy 
# ----------------------------------------
plot_curve_adenopathy <- ggplot(
  df,
  aes(x = Age, y = as.numeric(Recurred) - 1, color = Adenopathy_Binary)
) +
  geom_smooth(
    method = "glm",
    method.args = list(family = "binomial"),
    se = TRUE,
    fullrange = TRUE,
    fill = "grey80",
    alpha = 0.3
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25),
    name = "Probability of Recurrence"
  ) +
  scale_color_manual(
    values = c("No" = "#D73027", "Yes" = "#2E86AB"),
    name = "Lymph Node Involvement"
  ) +
  labs(
    title = "Prob. of Recurrence by Age & Node Status",
    x = "Age at Diagnosis"
  ) +
  theme(
    plot.title = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )

# ----------------------------------------
# Age vs. Recurrence, split by Gender 
# ----------------------------------------
plot_curve_gender <- ggplot(
  df,
  aes(x = Age, y = as.numeric(Recurred) - 1, color = Gender)
) +
  geom_smooth(
    method = "glm",
    method.args = list(family = "binomial"),
    se = TRUE,
    fullrange = TRUE,
    fill = "grey80",
    alpha = 0.3
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25),
    name = "Probability of Recurrence"
  ) +
  scale_color_manual(
    values = c("F" = "#D73027", "M" = "#2E86AB"),
    name = "Gender"
  ) +
  labs(
    title = "Prob. of Recurrence by Age & Gender",
    x = "Age at Diagnosis"
  ) +
  theme(
    plot.title = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )

# ----------------------------------------
# Age vs. Recurrence, split by Focality
# ----------------------------------------
plot_curve_focality <- ggplot(
  df,
  aes(x = Age, y = as.numeric(Recurred) - 1, color = Focality)
) +
  geom_smooth(
    method = "glm",
    method.args = list(family = "binomial"),
    se = TRUE,
    fullrange = TRUE,
    fill = "grey80",
    alpha = 0.3
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25),
    name = "Probability of Recurrence"
  ) +
  scale_color_manual(
    values = c("Uni-Focal" = "#D73027", "Multi-Focal" = "#2E86AB"),
    name = "Focality"
  ) +
  labs(
    title = "Prob. of Recurrence by Age & Focality",
    x = "Age at Diagnosis"
  ) +
  theme(
    plot.title = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )

# ----------------------------------------
# Age vs. Recurrence, split by Pathology
# ----------------------------------------
plot_curve_pathology <- ggplot(
  df,
  aes(x = Age, y = as.numeric(Recurred) - 1, color = Pathology_Simple)
) +
  geom_smooth(
    method = "glm",
    method.args = list(family = "binomial"),
    se = TRUE,
    fullrange = TRUE,
    fill = "grey80",
    alpha = 0.3
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25),
    name = "Probability of Recurrence"
  ) +
  scale_color_manual(
    values = c(
      "Follicular" = "#2E86AB",
      "Papillary" = "#D73027",
      "Hurthel cell" = "#2ECC71"
    ),
    name = "Pathology"
  ) +
  labs(
    title = "Prob. of Recurrence by Age & Pathology",
    x = "Age at Diagnosis"
  ) +
  theme(
    plot.title = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )

# ----------------------------------------
# Combine Plots
# ----------------------------------------
combined_plot <- ggarrange(
  plot_curve_adenopathy,
  plot_curve_gender,
  plot_curve_pathology,
  plot_curve_focality,
  ncol = 2,
  nrow = 2,
  common.legend = FALSE,
  align = "hv"
)
print(combined_plot)

# ----------------------------------------
# ROC Curve
# ----------------------------------------
pred_prob_test <- predict(model, newdata = df_test, type = "response")
roc_obj <- roc(df_test$Recurred, pred_prob_test)
auc_value <- auc(roc_obj)
print(auc_value)
plot_roc <- ggroc(roc_obj,color = "#2E86AB",, size = 1) +
  geom_abline(slope = 1, intercept = 1, linetype = "dashed", color = "grey",size=0.5) +
  labs(
    title = paste("ROC Curve (Test Set) - AUC =", round(auc_value, 4)),
    x = "1 - Specificity (False Positive Rate)",
    y = "Sensitivity (True Positive Rate)"
  )+
  theme_classic() +  # removes background and grid
  theme(
    plot.title = element_text(size = 12)
  )

print(plot_roc)

# ----------------------------------------
# Confusion Matrix
# ----------------------------------------
# Our primary goal was inference to understand the relationships and calculate
# valid odds ratios. This is why we build the model on the real imbalanced data
# (to get the true relationships) and then find the optimal threshold to
# correct for the imbalance when making predictions.
optimal_threshold <- coords(roc_obj, "best", ret = "threshold")$threshold
pred_class <- factor(
  ifelse(pred_prob_test > optimal_threshold, "Yes", "No"),
  levels = c("No", "Yes")
)
cm <- confusionMatrix(pred_class, df_test$Recurred, positive = "Yes")
plot_cm <- ggplot(
  as.data.frame(cm$table), aes(x = Reference, y = Prediction, fill = Freq)
) +
  geom_tile() +
  geom_text(aes(label = Freq), size = 6) +
  scale_fill_gradient(low = "grey98", high = "#2E86AB") +
  labs(
    title = paste(
      "Confusion Matrix (Threshold =", round(optimal_threshold, 4), ")"
    ),
    x = "Actual Outcome",
    y = "Predicted Outcome"
  ) +
  theme_classic() +  # white background, no grid
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    legend.position = "none"
  )

print(plot_cm)
print(cm)




# check levels
# levels(df$Adenopathy_Binary)
# levels(df$Gender)
# levels(df$Focality)
# levels(df$Age)
# levels(df$Pathology_Simple)


# -------------------------------
# Feature Importance (SHAP) 
# -------------------------------
# Ranking variables by influence

# Prediction Function
pred_fun <- function(object, newdata) {
  predict(object, newdata, type = "response")
}

# Prepare Predictors
X_test <- df_test[, c("Age", "Gender", "Adenopathy_Binary", 
                      "Pathology_Simple", "Focality"), with=FALSE]


# Calculate SHAP values
set.seed(42)
shap_values <- fastshap::explain(
  object = model,
  X = X_test,
  pred_wrapper = pred_fun,
  nsim = 300
)

# Summarize SHAP by feature
shap_mean <- data.table(
  Variable = colnames(shap_values),
  MeanAbsSHAP = apply(abs(shap_values), 2, mean)
)


# Rename variables for plotting
shap_mean[Variable == "Adenopathy_Binary", Variable := "Adenopathy"]
shap_mean[Variable == "Pathology_Simple", Variable := "Pathology"]

# Sort by importance
shap_mean <- shap_mean[order(-MeanAbsSHAP)]

# Create ggplot
p <- ggplot(shap_mean, aes(x = reorder(Variable, MeanAbsSHAP), 
                           y = MeanAbsSHAP,
                           text = paste("Feature:", Variable, 
                                        "<br>Mean |SHAP| (importance):", round(MeanAbsSHAP, 3)))) +
  geom_bar(stat = "identity", fill = "#2E86AB") +
  coord_flip() +
  labs(
    title = "Feature Importance (SHAP)",
    x = "Feature",
    y = "Mean |SHAP Value|"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey95"),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(hjust = 0.4)
  )

# Convert ggplot to interactive plot
interactive_shap <- ggplotly(p, tooltip = "text") %>%
  layout(
    yaxis = list(title = list(standoff = 1)),
    xaxis = list(title = list(standoff = 10))
  )

interactive_shap


# ---------------------------------------------------
# Feature Importance and Direction of Effect (SHAP)
# ---------------------------------------------------
#For understanding both importance and effect direction

# Prediction Function 
pred_fun <- function(object, newdata) {
  predict(object, newdata, type = "response")
}

# Predictors
X_test <- df_test[, c("Age", "Gender", "Adenopathy_Binary", 
                      "Pathology_Simple", "Focality"), with=FALSE]

# Calculate SHAP
set.seed(42)
shap_values <- fastshap::explain(
  object = model,
  X = X_test,
  pred_wrapper = pred_fun,
  nsim = 300
)

# SHAP with sign
shap_signed <- data.table(
  Variable = colnames(shap_values),
  MeanSHAP = apply(shap_values, 2, mean)
)

# Sort by absolute impact
shap_signed[, AbsMean := abs(MeanSHAP)]
shap_signed <- shap_signed[order(-AbsMean)]

# Rename variables for plotting
shap_signed[Variable == "Adenopathy_Binary", Variable := "Adenopathy"]
shap_signed[Variable == "Pathology_Simple", Variable := "Pathology"]

# Base ggplot with tooltip text
p <- ggplot(shap_signed, aes(
  x = reorder(Variable, AbsMean),
  y = AbsMean,
  fill = MeanSHAP,
  text = paste0("Feature: ", Variable, "<br>Mean |SHAP| (effect): ", round(MeanSHAP, 3))
)) +
  geom_bar(stat = "identity", width = 0.7) +
  coord_flip() +
  scale_fill_gradient2(
    low = "#D73027", mid = "white", high = "#2E86AB", midpoint = 0,
    name = "Effect direction\n(mean SHAP)"
  ) +
  labs(
    title = "Feature Importance and Direction of Effect (SHAP)",
    x = "Feature",
    y = "Mean |SHAP Value| (effect)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(size = 12),  
    axis.text.x = element_blank(),   # remove X axis labels
    axis.ticks.x = element_blank(),  # remove X axis ticks
    legend.title = element_text(size = 10),
    legend.text  = element_text(size = 9),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),  # remove vertical grid lines
    panel.grid.minor.x = element_blank(),  # remove vertical minor lines
    plot.title = element_text(hjust = 0.4)
  )

# Convert to interactive Plotly plot
interactive_shap <- ggplotly(p, tooltip = "text") %>%
  layout(
    yaxis = list(title = list(standoff = 1)),  
    xaxis = list(title = list(standoff = 10))   
  )

interactive_shap