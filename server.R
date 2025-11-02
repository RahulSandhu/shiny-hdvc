# server.R
server <- function(input, output, session) {
  
  # --- Smooth navigation ---
  observeEvent(input$goto_scope,  { runjs("document.getElementById('scope_section').scrollIntoView({behavior: 'smooth'});") })
  observeEvent(input$goto_eda,    { runjs("document.getElementById('eda_section').scrollIntoView({behavior: 'smooth'});") })
  observeEvent(input$goto_ml,     { runjs("document.getElementById('ml_section').scrollIntoView({behavior: 'smooth'});") })
  observeEvent(input$goto_results,{ runjs("document.getElementById('results_section').scrollIntoView({behavior: 'smooth'});") })
  observeEvent(input$goto_about,  { runjs("document.getElementById('about_section').scrollIntoView({behavior: 'smooth'});") })
  
  # ---- Overlay Plot ----
  dot_data <- data.frame(
    x = c(0.35, 0.6, 0.65, 0.4),
    y = c(0.6, 0.5, 0.6, 0.5),
    label = c("Follow-up Intensity", "Predicting Risk", "Incidence Trend", "Gender"),
    info = c(
      "ML scores inform risk-adapted surveillance strategies<br>targeting resources to high-risk patients.<sup>2</sup>",
      "ML models integrate demographic and clinical features<br>to predict individualized recurrence risk<br>and see which features drive the risk.<sup>3</sup>",
      "Incidence has increased greatly over the last 20 years<br>requiring better risk recurrence assessment.<sup>4</sup>",
      "Thyroid cancer is 3x more common in women,<br>but men tend to have 2-3x higher risk of recurrence.<sup>5</sup>")
  )
  dot_data$label_html <- paste0(
    '<span style=\"background: rgba(255,255,255,0.8); padding:3px 8px; border-radius:6px; color:#333;\">', 
    dot_data$label, 
    '</span>'
  )
  
  output$thyroid_overlay <- renderPlotly({
    plot_ly(
      data = dot_data, x = ~x, y = ~y,
      type = "scatter", mode = "markers+text",
      marker = list(size=18, color = "#FF6F61", symbol = "circle"),
      text = ~label_html,
      textfont = list(size = 14),
      textposition = "top center",
      hoverinfo = "text", hovertext = ~info
    ) %>%
      layout(
        images = list(
          list(
            source = "thyroid_search.jpg",
            xref = "x", yref = "y", x = 0, y = 1, sizex = 1, sizey = 1,
            sizing = "stretch", opacity = 1, layer = "below"
          )
        ),
        xaxis = list(showgrid = FALSE, zeroline=FALSE, visible=FALSE, range=c(0,1)),
        yaxis = list(showgrid = FALSE, zeroline=FALSE, visible=FALSE, range=c(0,1)),
        margin = list(l=0, r=0, b=0, t=0)
      )
  })
  
  output$age_gender_hist <- renderPlotly({ 
    plot_ly(data = data, x = ~age, color = ~gender, 
            colors = c("F" = "#FF6F61", "M" = "#2E86AB"), 
            type = "histogram", xbins = list(size = 10), opacity = 0.75, 
            hovertemplate = "Age: %{x}<br>Count: %{y}<br>Gender: %{data.name}<extra></extra>") %>% 
      layout(barmode = "stack", 
             xaxis = list(title = "Age (years)"), 
             yaxis = list(title = "Number of Patients"), 
             margin = list(t = 50, b = 50, l = 60, r = 40), 
             showlegend = TRUE) 
  })
  
  # Tumor T Stage Boxplot
  output$t_stage_boxplot <- renderPlotly({
    plot_ly(data = data, y = ~age, x = ~t, type = "box",
            color = ~t, boxpoints = "outliers",
            hoverinfo = "y+x+name") %>%
      layout(xaxis = list(title = "Tumor Stage (T)"),
             yaxis = list(title = "Age (years)"),
             margin = list(t = 50, b = 50, l = 60, r = 40))
  })
  
  # ---- Risk by Age Violin (keep existing palette) ----
  output$risk_age_violin <- renderPlotly({
    plot_ly(data = summary_data_prep,
            x = ~Risk, y = ~Age,
            type = 'violin',
            box = list(visible = TRUE),
            meanline = list(visible = TRUE),
            line = list(color = "#2E86AB"),
            fillcolor = "rgba(44,127,184,0.3)",
            opacity = 0.7) %>%
      add_trace(
        data = summary_data_prep,
        x = ~Risk,
        y = ~Age,
        type = 'scatter',
        mode = 'markers',
        marker = list(size = 6, color = "black", opacity = 0.65),
        inherit = FALSE,
        showlegend = FALSE
      ) %>%
      layout(xaxis = list(title = "Risk Group"),
             yaxis = list(title = "Age (years)"))
  })
}
