library(shiny)
library(ggplot2)

# 1. UI - Interfața cu utilizatorul
ui <- fluidPage(
  titlePanel("Simulare Trafic Zilnic - Proiect Probabilități și Statistică"),
  sidebarLayout(
    sidebarPanel(
      helpText("Parametri pentru modelarea traficului (Kd)"),

      # Slider pentru distribuția Poisson
      sliderInput("lambda", "Media Poisson (lambda):", min = 1, max = 200, value = 50),
      hr(), # Linie separatoare

      # Slidere pentru distribuția Binomială
      sliderInput("n_binom", "Nr. maxim clienți (n - Binomial):", min = 100, max = 1000, value = 500),
      sliderInput("p_binom", "Probabilitatea de activare (p):", min = 0.01, max = 1, value = 0.1),
      hr(),

      # Selectare unitate de timp (Ani sau Luni)
      radioButtons("unitate_timp", "Unitate de timp:",
        choices = c("Ani", "Luni"), selected = "Ani", inline = TRUE
      ),

      # Slider conditionat pentru Ani
      conditionalPanel(
        condition = "input.unitate_timp == 'Ani'",
        sliderInput("ani", "Număr de ani:", value = 1, min = 1, max = 10)
      ),

      # Slider conditionat pentru Luni
      conditionalPanel(
        condition = "input.unitate_timp == 'Luni'",
        sliderInput("luni", "Număr de luni:", value = 1, min = 1, max = 12)
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Histograme",
          plotOutput("plotPoisson"),
          plotOutput("plotBinomial")
        ),
        tabPanel(
          "Statistici Comparative",
          tableOutput("tabelStatistici"),
          textOutput("interpretare")
        )
      )
    )
  )
)

# 2. SERVER - Logica matematică
server <- function(input, output) {
  # Generăm datele în mod "reactiv" (se refac când miști slider-ul)
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

  # Grafic 1: Distribuția Poisson
  output$plotPoisson <- renderPlot({
    date <- date_simulatre()$poiss
    ggplot(data.frame(x = date), aes(x = x)) +
      geom_histogram(binwidth = 1, fill = "skyblue", color = "white") +
      labs(title = "Distribuția Poisson (Trafic Nelimitat)", x = "Nr. Clienți / Zi", y = "Frecvență") +
      theme_minimal()
  })

  # Grafic 2: Distribuția Binomială
  output$plotBinomial <- renderPlot({
    date <- date_simulatre()$binom
    ggplot(data.frame(x = date), aes(x = x)) +
      geom_histogram(binwidth = 1, fill = "salmon", color = "white") +
      labs(title = "Distribuția Binomială (Trafic Plafonat)", x = "Nr. Clienți / Zi", y = "Frecvență") +
      theme_minimal()
  })

  # Tabelul cu Media și Varianța (Cerința 1.c)
  output$tabelStatistici <- renderTable(
    {
      d <- date_simulatre()

      data.frame(
        Model = c("Poisson", "Binomial"),
        Media_Teoretica = c(input$lambda, input$n_binom * input$p_binom),
        Media_Empirica = c(mean(d$poiss), mean(d$binom)),
        Varianta_Teoretica = c(input$lambda, input$n_binom * input$p_binom * (1 - input$p_binom)),
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
      "Simularea a fost realizată pentru", text_timp, "(aprox.", zile_total, "zile). ",
      "Observați cum Media Empirică este foarte aproape de cea Teoretică datorită Legii Numerelor Mari."
    )
  })
}

# 3. Lansarea aplicației
shinyApp(ui = ui, server = server)
