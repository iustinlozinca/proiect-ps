library(shiny)
library(ggplot2)

ui_ex8 <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Inegalitati Probabilistice"),
    sidebarLayout(
      sidebarPanel(
        h4("1. Parametri Markov & Cebisev (Var T)"),
        helpText("T ~ Gamma(3, 2). Media = 1.5, Var = 0.75"),
        sliderInput(ns("n_sim"), "Numar simulari:", 1000, 10000, 5000,
          step = 1000
        ),
        sliderInput(ns("markov_a"), "Markov Limit (a):", 2.0, 5.0, 3.0,
          step = 0.1
        ),
        sliderInput(ns("cheby_k"), "Cebisev k (sigma):", 1.5, 5.0, 2.0,
          step = 0.5
        ),
        hr(),
        h4("2. Parametri Chernoff (Var X)"),
        helpText("X ~ Binomial(n, p) - Suma de Bernoulli"),
        sliderInput(ns("bin_n"), "Nr. incercari (n):", 10, 100, 50, step = 10),
        sliderInput(ns("bin_p"), "Prob. Succes (p):", 0.1, 0.9, 0.5,
          step = 0.1
        ),
        sliderInput(ns("cher_delta"), "Delta (deviatie medie):", 0.1, 1.0, 0.3,
          step = 0.1
        ),
        helpText("Verificam P(X >= (1+delta)mu)"),
        hr(),
        h4("3. Jensen (Functie Convexa)"),
        selectInput(ns("func_phi"), "Functia phi(x):",
          choices = c("x^2" = "sq", "exp(x)" = "exp")
        )
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Inegalitati (a, b)",
            h4("Inegalitatea lui Markov (T)"),
            verbatimTextOutput(ns("resMarkov")),
            hr(),
            h4("Inegalitatea lui Cebisev (T)"),
            verbatimTextOutput(ns("resCheby")),
            hr(),
            h4("Inegalitatea lui Chernoff (X ~ Binom)"),
            verbatimTextOutput(ns("resChernoff")),
            plotOutput(ns("plotChernoff"), height = "250px")
          ),
          tabPanel(
            "Jensen (d, e)",
            h3("Inegalitatea lui Jensen"),
            verbatimTextOutput(ns("resJensen")),
            plotOutput(ns("plotJensen"), height = "300px"),
            uiOutput(ns("jensenInterp"))
          ),
          tabPanel(
            "Interpretare (c, e)",
            uiOutput(ns("interpLimits"))
          )
        )
      )
    )
  )
}

server_ex8 <- function(id) {
  moduleServer(id, function(input, output, session) {
    # --- SIMULATIONS ---

    # 1. T ~ Gamma (Continuous) for Markov/Cebisev/Jensen
    sim_t <- reactive({
      n <- input$n_sim
      vec <- rgamma(n, shape = 3, rate = 2)
      list(val = vec, mean = mean(vec), sd = sd(vec), theor_mean = 1.5)
    })

    # 2. X ~ Binomial (Discrete Sum) for Chernoff
    sim_x <- reactive({
      n_sim <- input$n_sim
      size <- input$bin_n
      prob <- input$bin_p
      vec <- rbinom(n_sim, size = size, prob = prob)
      list(val = vec, n = size, p = prob, mu = size * prob)
    })

    # --- OUTPUTS ---

    # Markov
    output$resMarkov <- renderPrint({
      d <- sim_t()
      a <- input$markov_a
      prob_empiric <- mean(d$val >= a)
      bound <- d$mean / a

      cat(sprintf("P(T >= %.2f)            = %.4f\n", a, prob_empiric))
      cat(sprintf("Markov Bound (E[T]/a)  = %.4f\n", bound))
      cat(sprintf("Verificat: %.4f <= %.4f\n", prob_empiric, bound))
    })

    # Cebisev
    output$resCheby <- renderPrint({
      d <- sim_t()
      k <- input$cheby_k
      threshold <- k * d$sd
      prob_empiric <- mean(abs(d$val - d$mean) >= threshold)
      bound <- 1 / (k^2)

      cat(sprintf("P(|T-mu| >= %.2f*sigma) = %.4f\n", k, prob_empiric))
      cat(sprintf("Cebisev Bound (1/k^2) = %.4f\n", bound))
      cat(sprintf("Verificat: %.4f <= %.4f\n", prob_empiric, bound))
    })

    # Chernoff
    output$resChernoff <- renderPrint({
      d <- sim_x()
      delta <- input$cher_delta
      mu <- d$mu

      limit_val <- (1 + delta) * mu
      prob_empiric <- mean(d$val >= limit_val)

      # Chernoff Bound (Simplified Form for Upper Tail)
      # P(X >= (1+d)u) <= exp( - (d^2 * u) / (2+d) ) is one form
      # Or standard: (e^d / (1+d)^(1+d))^u

      term <- (exp(delta) / ((1 + delta)^(1 + delta)))^mu

      cat(sprintf("X ~ Binom(n=%d, p=%.2f), Mu = %.1f\n", d$n, d$p, mu))
      cat(sprintf("Target Limit: X >= (1+%.1f)mu = %.2f\n", delta, limit_val))
      cat(sprintf("P(X >= %.2f) [Empiric] = %.4f\n", limit_val, prob_empiric))
      cat(sprintf("Chernoff Bound           = %.4f\n", term))
      cat(sprintf("Verificat: %.4f <= %.4f\n", prob_empiric, term))
    })

    output$plotChernoff <- renderPlot({
      d <- sim_x()
      delta <- input$cher_delta
      limit_val <- (1 + delta) * d$mu

      df <- data.frame(X = d$val)
      ggplot(df, aes(x = .data$X)) +
        geom_histogram(
          binwidth = 1, fill = "orange", color = "black", alpha = 0.6
        ) +
        geom_vline(
          xintercept = limit_val, color = "red", linetype = "dashed", size = 1
        ) +
        labs(
          title = paste(
            "Distributia X si Limita Chernoff:",
            round(limit_val, 1)
          )
        ) +
        theme_minimal()
    })

    # Jensen
    output$resJensen <- renderPrint({
      d <- sim_t()
      choice <- input$func_phi
      phi <- if (choice == "sq") function(x) x^2 else exp

      lhs <- phi(d$mean) # nolint phi(E[T])
      rhs <- mean(phi(d$val)) # nolint E[phi(T)]

      cat(sprintf("phi(E[T]) = %.4f\n", lhs))
      cat(sprintf("E[phi(T)] = %.4f\n", rhs))
      cat(sprintf("Jensen: %.4f <= %.4f -> TRUE\n", lhs, rhs))
    })

    output$plotJensen <- renderPlot({
      d <- sim_t()
      choice <- input$func_phi

      x_grid <- seq(0, max(d$val) * 1.1, length.out = 100)
      y_grid <- if (choice == "sq") x_grid^2 else exp(x_grid)

      phi <- if (choice == "sq") function(x) x^2 else exp

      pt_x <- d$mean
      pt_low <- phi(d$mean)
      pt_high <- mean(phi(d$val))

      ggplot() +
        geom_line(aes(x_grid, y_grid), size = 1) +
        geom_point(aes(pt_x, pt_low), color = "blue", size = 4) +
        geom_point(aes(pt_x, pt_high), color = "red", size = 4) +
        geom_segment(
          aes(x = pt_x, y = pt_low, xend = pt_x, yend = pt_high),
          linetype = "dotted"
        ) +
        annotate(
          "text",
          x = pt_x, y = pt_low, label = "phi(E[T])", vjust = 1.5,
          color = "blue"
        ) +
        annotate(
          "text",
          x = pt_x, y = pt_high, label = "E[phi(T)]", vjust = -1,
          color = "red"
        ) +
        labs(
          title = paste(
            "Jensen: Media Penalizarilor (Rosu) >=",
            "Penalizarea Mediei (Albastru)"
          ),
          x = "T", y = "phi(T)"
        ) +
        theme_minimal()
    })

    # Interpretation
    output$interpLimits <- renderUI({
      HTML(paste0(
        "<h3>c) Utilitatea Limitelor (Markov, Cebisev, Chernoff)</h3>",
        "<p>Aceste inegalitati ofera 'garantii' asupra probabilitatii ca o",
        "variabila sa devieze mult de la medie, ",
        "folosind doar cunostinte limitate (Media, Varianta), fara a sti ",
        "distributia exacta.</p>",
        "<ul>",
        "<li><b>Markov:</b> Ne da o limita superioara simpla pentru valorile ",
        "extreme pozitive.</li>",
        "<li><b>Cebisev:</b> Ne spune ca valorile foarte departate de medie ",
        "sunt improbabile (de ex. e greu sa fii la 3 deviatii standard ",
        "distanta).</li>",
        "<li><b>Chernoff:</b> Este mult mai 'puternica' (scade exponential) ",
        "pentru sume de variabile independente (ca numarul de esecuri). ",
        "Observati ca limita (Bound) este mult mai mica decat la Cebisev.</li>",
        "</ul>",
        "<hr>",
        "<h3>e) Riscul si Jensen</h3>",
        "<p>Inegalitatea lui Jensen (E[phi(T)] >= phi(E[T])) ne avertizeaza ",
        "asupra <b>Costului Variantei</b>.</p>",
        "<p>Daca functia de cost este convexa (ex: intarzierea mare ",
        "penalizeaza disproportionat de mult), ",
        "atunci un sistem oscilant este mai costisitor decat unul constant, ",
        "chiar daca au aceeasi medie!.</p>"
      ))
    })
  })
}
