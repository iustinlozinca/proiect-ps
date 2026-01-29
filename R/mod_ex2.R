library(shiny)
library(ggplot2)

ui_ex2 <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Modelarea Timpilor de Raspuns"),
    sidebarLayout(
      sidebarPanel(
        helpText(
          "Parametri pentru distributiile timpului de raspuns (S) - Milisecunde"
        ),

        # Setari generale
        sliderInput(ns("n_sim"), "Numar de cereri simulate:",
          min = 100, max = 10000, value = 1000, step = 100
        ),
        hr(),

        # Distributia Exponentiala
        h4("1. Distributia Exponentiala (Asimetrica)"),
        helpText(
          "Introducem Media dorita (in ms), iar aplicatia",
          "calculeaza rata lambda."
        ),
        sliderInput(ns("media_exp_ms"), "Media (ms):",
          min = 50, max = 1000, value = 200, step = 10
        ),
        hr(),

        # Distributia Normala
        h4("2. Distributia Normala (Simetrica)"),
        helpText("Trunchiata la 0 (valori pozitive)."),
        sliderInput(ns("mu_norm_ms"), "Media (mu - ms):",
          min = 50, max = 1000, value = 200, step = 10
        ),
        sliderInput(ns("sigma_norm_ms"), "Deviatia Standard (sigma - ms):",
          min = 10, max = 300, value = 50, step = 5
        )
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Distributii (Histograme)",
            plotOutput(ns("plotExponential")),
            plotOutput(ns("plotNormal"))
          ),
          tabPanel(
            "Statistici Comparative",
            tableOutput(ns("tabelStatistici")),
            uiOutput(ns("textDiscutie"))
          )
        )
      )
    )
  )
}

server_ex2 <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Generare date reactive
    date_simulate <- reactive({
      n <- input$n_sim

      # Exponentiala: lambda = 1 divided by mean
      lambda <- 1 / input$media_exp_ms
      s_exp <- rexp(n, rate = lambda)

      # Normala
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

    # Grafic Exponentiala
    output$plotExponential <- renderPlot({
      dat <- date_simulate()
      df <- data.frame(Timp = dat$exp)
      lambda <- dat$lambda
      media <- input$media_exp_ms

      limit <- max(1000, media * 5)
      df <- df[df$Timp <= limit, , drop = FALSE]

      # Dynamic binwidth for visibility
      bw <- limit / 40

      ggplot(df, aes(x = .data$Timp)) +
        geom_histogram(
          aes(y = after_stat(density)),
          binwidth = bw,
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
            "Distributia Exponentiala (Media =", media,
            "ms, Lambda =", round(lambda, 5), ")"
          ),
          x = "Timp de raspuns (milisecunde)", y = "Densitate"
        ) +
        coord_cartesian(xlim = c(0, limit)) +
        theme_minimal()
    })

    # Grafic Normala
    output$plotNormal <- renderPlot({
      dat <- date_simulate()
      df <- data.frame(Timp = dat$norm)
      mu <- input$mu_norm_ms
      sigma <- input$sigma_norm_ms

      limit <- max(1000, mu + 4 * sigma)
      df <- df[df$Timp <= limit, , drop = FALSE]

      bw <- limit / 40

      ggplot(df, aes(x = .data$Timp)) +
        geom_histogram(
          aes(y = after_stat(density)),
          binwidth = bw,
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
            "Distributia Normala (Mu =", mu, "ms, Sigma =", sigma, "ms)"
          ),
          x = "Timp de raspuns (milisecunde)", y = "Densitate"
        ) +
        coord_cartesian(xlim = c(0, limit)) +
        theme_minimal()
    })

    # Tabel Statistici
    output$tabelStatistici <- renderTable(
      {
        dat <- date_simulate()

        # Calcule Exponentiala
        exp_mean <- mean(dat$exp)
        exp_med <- median(dat$exp)
        exp_var <- var(dat$exp)
        exp_mod <- get_mode(dat$exp)

        lambda <- dat$lambda

        # Calcule Normala
        norm_mean <- mean(dat$norm)
        norm_med <- median(dat$norm)
        norm_var <- var(dat$norm)
        norm_mod <- get_mode(dat$norm)

        mu <- input$mu_norm_ms
        sigma <- input$sigma_norm_ms

        data.frame(
          Metrica = c(
            "Media (Mean)", "Mediana (Median)",
            "Valoarea Modala (Mode)", "Varianta (Variance)"
          ),
          Exponentiala_Empirica = c(exp_mean, exp_med, exp_mod, exp_var),
          Exponentiala_Teoretica = c(
            1 / lambda, log(2) / lambda, 0, 1 / (lambda^2)
          ),
          Normala_Empirica = c(norm_mean, norm_med, norm_mod, norm_var),
          Normala_Teoretica = c(mu, mu, mu, sigma^2)
        )
      },
      digits = 2
    )

    # Discutie
    output$textDiscutie <- renderUI({
      dat <- date_simulate()
      dif_exp <- mean(dat$exp) - median(dat$exp)

      HTML(paste0(
        "<h3>Discutie: Media vs Mediana in Latenta</h3>",
        "<p>",
        "Observati ca <b>Media > Mediana</b> (diferenta aprox: ",
        round(dif_exp, 1), " ms). <br>",
        "Acest lucru indica o asimetrie la dreapta. ",
        "Cateva cereri lente ('coada lunga') cresc media semnificativ, ",
        "in timp ce mediana ramane mai stabila si mai reprezentativa ",
        "pentru utilizatorul 'tipic'.</p>"
      ))
    })
  })
}
