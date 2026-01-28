library(shiny)
library(ggplot2)

# Load Modules
source("mod_ex1.R")
source("mod_ex2.R")
source("mod_ex3.R")
source("mod_ex4.R")

# Main UI
ui <- navbarPage(
  title = "Proiect Probabilități și Statistică",
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  tabPanel(
    "Acasă",
    fluidPage(
      jumbotron <- div(
        class = "jumbotron",
        style = "background-color: #ecf0f1; padding: 2rem; border-radius: 10px;",
        h1("Proiect Statistică", style = "color: #2c3e50;"),
        p("Simulări interactive pentru exercițiile 1-4.", style = "font-size: 1.2rem;"),
        hr(),
        p("Autor: Rareș Tudor"),
        a(class = "btn btn-primary btn-lg", href = "#", "Începeți explorarea (folosiți meniul de sus)")
      )
    )
  ),

  # Exercise 1 Tab
  tabPanel(
    "Exercițiul 1",
    ui_ex1("ex1")
  ),

  # Exercise 2 Tab
  tabPanel(
    "Exercițiul 2",
    ui_ex2("ex2")
  ),

  # Exercise 3 Tab
  tabPanel(
    "Exercițiul 3",
    ui_ex3("ex3")
  ),

  # Exercise 4 Tab
  tabPanel(
    "Exercițiul 4",
    ui_ex4("ex4")
  )
)

# Main Server
server <- function(input, output, session) {
  # Call Modules
  server_ex1("ex1")
  server_ex2("ex2")
  server_ex3("ex3")
  server_ex4("ex4")
}

# Run App
shinyApp(ui, server)
