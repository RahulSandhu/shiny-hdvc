# ui.R
ui <- dashboardPage(
  dashboardHeader(title = "HDVC"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    useShinyjs(),
    
    # ---- Custom Styles ----
    tags$head(
      tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css2?family=Poppins:wght@500;600&family=Open+Sans:wght@400;500&display=swap"),
      tags$link(rel="stylesheet", type="text/css", href="custom.css")
    ),
    
    # ---- Navigation Buttons ----
    fluidRow(
      column(12, align="center",
             actionButton("goto_scope", "Scope"),
             actionButton("goto_eda", "Data EDA"),
             actionButton("goto_ml", "ML Methods"),
             actionButton("goto_results", "Results"),
             actionButton("goto_about", "About")
      )
    ),
    
    # ---- Scope Section ----
    div(id="scope_section",
        fluidRow(
          column(5,
                 h2("Thyroid Cancer Recurrence: Scope & Challenge"),
                 p(HTML("Thyroid cancer affects over <b>821,000 people</b> per year.<sup>1</sup> 
                         This study analyzes 383 patients over 10–15 years to assess 
                         how demographic and clinical factors affect recurrence probability."))
          ),
          column(7, plotlyOutput("thyroid_overlay", height = "350px"))
        )
    ),
    
    # ---- EDA Section ----
    div(id="eda_section",
        fluidRow(
          column(12, h2("Patient Recurrence Data")),
          column(12,
                 tabBox(width = 12, 
                        tabPanel("Age & Gender Histogram", plotlyOutput("age_gender_hist", height = "300px")),
                        tabPanel("Tumor T Stage Boxplot", plotlyOutput("t_stage_boxplot", height = "300px")),
                        tabPanel("Risk by Age Violin", plotlyOutput("risk_age_violin", height = "300px")),
                        tabPanel("Smoking Status Bar", plotlyOutput("smoking_bar", height = "300px"))
                 )
          )
        )
    ),
    
    # ---- ML Section ----
    div(id="ml_section",
        fluidRow(
          column(6, h2("Machine Learning Methods"),
                 p("We used [model type], chosen for its ability to capture complex feature interactions 
                    and provide interpretable outputs for clinical relevance.")),
          column(6, img(src="ml_architecture_diagram.png", height="250px", style="border-radius:10px;"))
        )
    ),
    
    # ---- Results Section ----
    div(id="results_section",
        fluidRow(
          column(12, h2("Results: Predicting Recurrence")),
          column(12,
                 tabBox(width = 12, 
                        tabPanel("AUC Panel", plotlyOutput("auc_panel", height = "300px")),
                        tabPanel("Confusion Matrix", plotlyOutput("confmat_panel", height = "300px")),
                        tabPanel("Other Metrics", plotlyOutput("other_metric_panel", height = "300px"))
                 )
          ),
          column(12, h3("Feature Importance (SHAP values)"), plotlyOutput("shap_bar"))
        )
    ),
    
    # ---- About Section ----
    div(id="about_section",
        fluidRow(
          column(12, h2("About the Study"),
                 p("Data: 383 patients, 10+ years follow-up."),
                 p("Authors, methods, and references below:"),
                 p("1. Lyu Z, Zhang Y, Sheng C, Huang Y, Zhang Q, Chen K. Global burden of thyroid cancer in 2022. Chin Med J (Engl). 2024."),
                 p("2. Roh, J.-L., Park, J. Y., Park, C. I., & Kim, J. H. (2017). Post-treatment surveillance of thyroid cancer. Cancer Medicine."),
                 p("3. Li, Y., Tang, Z., Ren, A., Tian, G., Zhang, J., Wang, Y., Liu, J., & Ming, J. (2025). ML-based recurrence prediction model. Frontiers in Endocrinology."),
                 p("4. Yang, Y., Qiu, T., Wang, Z., Li, Y., Deng, Y., He, Q., & Luo, D. (2024). Global thyroid cancer burden. Frontiers in Oncology."),
                 p("5. Donangelo I, Suh SY, Grewal RK, et al. (2019). Gender differences in recurrence risk. Thyroid.")
          )
        )
    )
  )
)
