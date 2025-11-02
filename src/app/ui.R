ui <- page_navbar(
  title = "HDVC",

  # Set Navbar Colors
  bg = "#90292A",
  inverse = TRUE,

  # Use a basic theme: https://rstudio.github.io/shinythemes/
  theme = bs_theme(version = 5, preset = "shiny"),

  # Link to CSS file
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "theme.css")
  ),

  # Panels
  nav_panel(
    "Scope",
    div(
      id = "scope",
      card(
        card_header("Scope"),
        card_body("Dummy placeholder.")
      )
    )
  ),
  nav_panel(
    "EDA",
    div(
      id = "eda",
      card(
        card_header("Exploratory Data Analysis"),
        card_body("Dummy placeholder.")
      )
    )
  ),
  nav_panel(
    "Machine Learning",
    div(
      id = "machine_learning",
      card(
        card_header("Machine Learning"),
        card_body("Dummy placeholder.")
      )
    )
  ),
  nav_panel(
    "Results",
    div(
      id = "results",
      card(
        card_header("Results"),
        card_body("Dummy placeholder.")
      )
    )
  ),
  nav_panel(
    "About",
    div(
      id = "about",
      card(
        card_header("Data Source"),
        card_body(
          "Borzooei, S. & Tarokhian, A. (2023). Differentiated Thyroid Cancer
                Recurrence [Dataset]. UCI Machine Learning Repository.",
          tags$a(
            href = "https://doi.org/10.24432/C5632J",
            "https://doi.org/10.24432/C5632J",
            target = "_blank"
          )
        )
      ),
      card(
        card_header("Source Code & License"),
        card_body(
          "The complete source code for this application is openly available
          for inspection and use on GitHub under the MIT License.",
          tags$p(
            tags$a(
              href = "https://github.com/RahulSandhu/shiny-hdvc",
              "View Code on GitHub",
              target = "_blank",
              class = "btn btn-primary"
            )
          )
        )
      )
    )
  ),

  # Search box
  nav_spacer(),
  nav_item(
    tags$form(
      class = "d-flex",
      textInput("search", label = NULL, placeholder = "Search...")
    ),
    class = "search-nav-item"
  )
)
