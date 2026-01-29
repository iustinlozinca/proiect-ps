library(shiny)
library(ggplot2)

ui <- navbarPage(
  id = "nav",
  title = actionLink("app_title", "Proiect la Probabilitati si Statistica",
    style = paste0(
      "color: white; text-decoration: none; ",
      "font-weight: bold; font-size: 1.25rem;"
    )
  ),
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  header = tags$style(HTML(
    ".navbar-nav > li > a[data-value='home'] { display: none; }"
  )),
  tabPanel(
    "Home",
    value = "home",
    fluidPage(
      jumbotron <- div(
        class = "jumbotron",
        style = paste0(
          "background-color: #ecf0f1; padding: 2rem; ",
          "border-radius: 10px;"
        ),
        h3("Documentatie", style = "color: #2c3e50;"),
        hr(),
      )
    )
  ),
  navbarMenu(
    "Cerinte",
    tabPanel(
      "1. Modelarea traficului zilnic",
      ui_ex1("ex1")
    ),
    tabPanel(
      "2. Modelarea timpilor de raspuns",
      ui_ex2("ex2")
    ),
    tabPanel(
      "3. Cereri, retry-uri si evenimente",
      ui_ex3("ex3")
    ),
    tabPanel(
      "4. V.A bidimensionale discrete",
      ui_ex4("ex4")
    ),
    tabPanel(
      "5. V.A bidimensionale (discrete si continue)",
      ui_ex5("ex5")
    ),
    tabPanel(
      "6. Probabilitati conditionate si conditionari",
      ui_ex6("ex6")
    ),
    tabPanel(
      "7. Independenta vs dependenta",
      ui_ex7("ex7")
    ),
    tabPanel(
      "8. Inegalitati probabilistice (garantii worst-case)",
      ui_ex8("ex8")
    ),
    tabPanel(
      "9. Aproximare normala si agregare",
      ui_ex9("ex9")
    ),
    tabPanel(
      "10. Churn (pierderea utilizatorilor)",
      ui_ex10("ex10")
    ),
    tabPanel(
      "11. Impact economic",
      ui_ex11("ex11")
    ),
    tabPanel(
      "12. Vizualizare statistica",
      ui_ex12("ex12")
    ),
    tabPanel(
      "13. Analiza de sinteza",
      ui_ex13("ex13")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$app_title, {
    updateNavbarPage(session, "nav", selected = "home")
  })

  server_ex1("ex1") # nolint
  server_ex2("ex2") # nolint
  server_ex3("ex3") # nolint
  server_ex4("ex4") # nolint
  server_ex5("ex5") # nolint
  server_ex6("ex6") # nolint
  server_ex7("ex7") # nolint
  server_ex8("ex8") # nolint
  server_ex9("ex9") # nolint
  server_ex10("ex10") # nolint
  server_ex11("ex11") # nolint
  server_ex12("ex12") # nolint
  server_ex13("ex13") # nolint
}

shinyApp(ui, server)
