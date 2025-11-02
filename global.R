library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(readr)
library(shinyjs)

# Load your data
data <- read_csv("./data.csv")
summary_data_prep <- read_csv("./summary_data_prep.csv")