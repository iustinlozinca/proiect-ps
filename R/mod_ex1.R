library(shiny)
library(ggplot2)

ui_ex1 <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Modelarea Traficului Zilnic"),
    sidebarLayout(
      sidebarPanel(
        helpText("Parametri pentru modelarea traficului (Kd)"),

        # Slider pentru distributia Poisson
        sliderInput(ns("lambda"), "Media Poisson (lambda):",
          min = 1, max = 200, value = 50
        ),
        hr(),

        # Slidere pentru distributia Binomiala
        sliderInput(ns("n_binom"), "Nr. maxim clienți (n - Binomial):",
          min = 100, max = 1000, value = 500
        ),
        sliderInput(ns("p_binom"), "Probabilitatea de activare (p):",
          min = 0.01, max = 1, value = 0.1
        ),
        hr(),

        # Selectare unitate de timp (Ani sau Luni)
        radioButtons(ns("unitate_timp"), "Unitate de timp:",
          choices = c("Ani", "Luni"), selected = "Ani", inline = TRUE
        ),

        # Slider conditionat pentru Ani
        conditionalPanel(
          condition = sprintf("input['%s'] == 'Ani'", ns("unitate_timp")),
          sliderInput(ns("ani"), "Număr de ani:", value = 1, min = 1, max = 10)
        ),

        # Slider conditionat pentru Luni
        conditionalPanel(
          condition = sprintf("input['%s'] == 'Luni'", ns("unitate_timp")),
          sliderInput(ns("luni"), "Număr de luni:",
            value = 1, min = 1, max = 12
          )
        )
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Histograme",
            plotOutput(ns("plotPoisson")),
            plotOutput(ns("plotBinomial"))
          ),
          tabPanel(
            "Statistici Comparative",
            tableOutput(ns("tabelStatistici")),
            textOutput(ns("interpretare"))
          )
        )
      )
    )
  )
}

server_ex1 <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Generam datele in mod "reactiv"
    date_simulatre <- reactive({
      if (input$unitate_timp == "Ani") {
        n_zile <- input$ani * 365
      } else {
        n_zile <- input$luni * 30
      }

      list(
        poiss = rpois(n_zile, lambda = input$lambda),
        binom = rbinom(n_zile, size = input$n_binom, prob = input$p_binom)
      )
    })

    # Grafic 1: Distributia Poisson
    output$plotPoisson <- renderPlot({
      date <- date_simulatre()$poiss
      ggplot(data.frame(valoare = date), aes(x = .data$valoare)) +
        geom_histogram(binwidth = 1, fill = "skyblue", color = "white") +
        labs(
          title = "Distribuția Poisson (Trafic Nelimitat)",
          x = "Nr. Clienți / Zi",
          y = "Frecvență"
        ) +
        theme_minimal()
    })

    # Grafic 2: Distributia Binomiala
    output$plotBinomial <- renderPlot({
      date <- date_simulatre()$binom
      ggplot(data.frame(valoare = date), aes(x = .data$valoare)) +
        geom_histogram(binwidth = 1, fill = "salmon", color = "white") +
        labs(
          title = "Distribuția Binomială (Trafic Plafonat)",
          x = "Nr. Clienți / Zi",
          y = "Frecvență"
        ) +
        theme_minimal()
    })

    # Tabelul cu Media si Varianta
    output$tabelStatistici <- renderTable(
      {
        d <- date_simulatre()

        data.frame(
          Model = c("Poisson", "Binomial"),
          Media_Teoretica = c(input$lambda, input$n_binom * input$p_binom),
          Media_Empirica = c(mean(d$poiss), mean(d$binom)),
          Varianta_Teoretica = c(
            input$lambda,
            input$n_binom * input$p_binom * (1 - input$p_binom)
          ),
          Varianta_Empirica = c(var(d$poiss), var(d$binom))
        )
      },
      digits = 2
    )

    output$interpretare <- renderText({
      if (input$unitate_timp == "Ani") {
        zile_total <- input$ani * 365
        text_timp <- paste(input$ani, "ani")
      } else {
        zile_total <- input$luni * 30
        text_timp <- paste(input$luni, "luni")
      }

      paste(
        "Simularea a fost realizată pentru", text_timp,
        "(aprox.", zile_total, "zile). ",
        "Observați cum Media Empirică este foarte aproape de cea Teoretică",
        "datorită Legii Numerelor Mari."
      )
    })
  })
}
