library(shiny)
library(ggplot2)

# 2. UI - Interfața cu utilizatorul
ui <- fluidPage(
  titlePanel("Modelarea Timpilor de Răspuns (Latență) - Exercițiul 2"),
  sidebarLayout(
    sidebarPanel(
      helpText(
        "Parametri pentru distribuțiile timpului de răspuns (S) - Milisecunde"
      ),

      # Setări generale
      sliderInput(
        "n_sim", "Număr de cereri simulate:",
        min = 100, max = 10000, value = 1000, step = 100
      ),
      hr(),

      # Distribuția Exponențială
      h4("1. Distribuția Exponențială (Asimetrică)"),
      helpText(
        "Introducem Media dorită (în ms), iar aplicația calculează rata lambda."
      ),
      sliderInput(
        "media_exp_ms", "Media (ms):",
        min = 50, max = 1000, value = 200, step = 10
      ),
      hr(),

      # Distribuția Normală
      h4("2. Distribuția Normală (Simetrică)"),
      helpText("Trunchiată la 0 (valori pozitive)."),
      sliderInput(
        "mu_norm_ms", "Media (mu - ms):",
        min = 50, max = 1000, value = 200, step = 10
      ),
      sliderInput(
        "sigma_norm_ms", "Deviația Standard (sigma - ms):",
        min = 10, max = 300, value = 50, step = 5
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Distribuții (Histograme)",
          plotOutput("plotExponential"),
          plotOutput("plotNormal")
        ),
        tabPanel(
          "Statistici Comparative",
          tableOutput("tabelStatistici"),
          uiOutput("textDiscutie")
        )
      )
    )
  )
)

# 2. SERVER - Logica matemetică
server <- function(input, output) {
  # Generare date reactive
  date_simulate <- reactive({
    n <- input$n_sim

    # Exponențială: lambda = 1 / mean
    lambda <- 1 / input$media_exp_ms
    s_exp <- rexp(n, rate = lambda)

    # Normală
    s_norm_raw <- rnorm(n, mean = input$mu_norm_ms, sd = input$sigma_norm_ms)
    s_norm <- s_norm_raw[s_norm_raw > 0]

    list(exp = s_exp, norm = s_norm, lambda = lambda)
  })

  # Helper function pentru mod
  get_mode <- function(v) {
    if (length(v) == 0) {
      return(NA)
    }
    d <- density(v)
    d$x[which.max(d$y)]
  }

  # Grafic Exponențială
  output$plotExponential <- renderPlot({
    dat <- date_simulate()
    df <- data.frame(x = dat$exp)
    lambda <- dat$lambda
    media <- input$media_exp_ms

    ggplot(df, aes(x = x)) +
      geom_histogram(
        aes(y = after_stat(density)),
        binwidth = 10,
        fill = "orange",
        color = "white",
        alpha = 0.7
      ) +
      stat_function(
        fun = dexp,
        args = list(rate = lambda),
        color = "red",
        lwd = 1.2
      ) +
      labs(
        title = paste(
          "Distribuția Exponențială (Media =", media,
          "ms, Lambda =", round(lambda, 5), ")"
        ),
        x = "Timp de răspuns (milisecunde)", y = "Densitate"
      ) +
      xlim(0, max(1000, media * 5)) +
      theme_minimal()
  })

  # Grafic Normală
  output$plotNormal <- renderPlot({
    dat <- date_simulate()
    df <- data.frame(x = dat$norm)
    mu <- input$mu_norm_ms
    sigma <- input$sigma_norm_ms

    ggplot(df, aes(x = x)) +
      geom_histogram(
        aes(y = after_stat(density)),
        binwidth = 10,
        fill = "purple",
        color = "white",
        alpha = 0.7
      ) +
      stat_function(
        fun = dnorm,
        args = list(mean = mu, sd = sigma),
        color = "blue",
        lwd = 1.2
      ) +
      labs(
        title = paste(
          "Distribuția Normală (Mu =", mu, "ms, Sigma =", sigma, "ms)"
        ),
        x = "Timp de răspuns (milisecunde)", y = "Densitate"
      ) +
      xlim(0, max(1000, mu + 4 * sigma)) +
      theme_minimal()
  })

  # Tabel Statistici
  output$tabelStatistici <- renderTable(
    {
      dat <- date_simulate()

      # Calcule Exponențială
      exp_mean <- mean(dat$exp)
      exp_med <- median(dat$exp)
      exp_var <- var(dat$exp)
      exp_mod <- get_mode(dat$exp)

      lambda <- dat$lambda

      # Calcule Normală
      norm_mean <- mean(dat$norm)
      norm_med <- median(dat$norm)
      norm_var <- var(dat$norm)
      norm_mod <- get_mode(dat$norm)

      mu <- input$mu_norm_ms
      sigma <- input$sigma_norm_ms

      data.frame(
        Metrica = c(
          "Media (Mean)", "Mediana (Median)",
          "Valoarea Modală (Mode)", "Varianța (Variance)"
        ),
        Exponentiala_Empirica = c(exp_mean, exp_med, exp_mod, exp_var),
        Exponentiala_Teoretica = c(1 / lambda, log(2) / lambda, 0, 1 / (lambda^2)),
        Normala_Empirica = c(norm_mean, norm_med, norm_mod, norm_var),
        Normala_Teoretica = c(mu, mu, mu, sigma^2)
      )
    },
    digits = 2
  )

  # Discuție
  output$textDiscutie <- renderUI({
    dat <- date_simulate()
    dif_exp <- mean(dat$exp) - median(dat$exp)

    HTML(paste0(
      "<h3>Discuție: Media vs Mediana în Latență</h3>",
      "<p>",
      "Observați că <b>Media > Mediana</b> (diferență aprox: ",
      round(dif_exp, 1), " ms). <br>",
      "Acest lucru indică o asimetrie la dreapta. ",
      "Câteva cereri lente ('coada lungă') cresc media semnificativ, ",
      "în timp ce mediana rămâne mai stabilă și mai reprezentativă ",
      "pentru utilizatorul 'tipic'.</p>"
    ))
  })
}

shinyApp(ui = ui, server = server)
