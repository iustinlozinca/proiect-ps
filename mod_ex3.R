# Module Exercise 3: Requests, Retries, and Events
# Adapted from 3.R

ui_ex3 <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Cereri, Retry-uri și Evenimente"),
    sidebarLayout(
      sidebarPanel(
        h4("Parametri Simulare"),
        sliderInput(ns("n_sim"), "Număr utilizatori:", 1000, 10000, 5000),
        sliderInput(
          ns("p_succes"), "Probabilitate succes (per încercare):", 0.1, 0.9, 0.7
        ),
        sliderInput(ns("max_retry"), "Nr. Maxim retry:", 1, 10, 3),
        hr(),
        h4("Praguri Evenimente"),
        sliderInput(
          ns("t0_SLA"), "Prag Timp (t0) - SLA:", 0.5, 10.0, 2.0,
          step = 0.5
        ),
        sliderInput(ns("n0_at"), "Prag Încercări (n0) - Eficiență:", 1, 5, 1)
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Simulare",
            fluidRow(
              column(6, plotOutput(ns("distrTimp"))),
              column(6, plotOutput(ns("distrIncercari")))
            ),
            helpText(
              "Histograma timpului total (T) și a numărului de încercări (N)."
            )
          ),
          tabPanel(
            "Probabilități (3.a)",
            tableOutput(ns("tabProb")),
            helpText(
              "Probabilitățile empirice calculate prin",
              "numărarea cazurilor favorabile / total cazuri."
            )
          ),
          tabPanel(
            "Verificare Formule (3.b)",
            verbatimTextOutput(ns("verificareFormula")),
            helpText("Verificăm egalitatea P(A ∪ D) = P(A) + P(D) - P(A ∩ D).")
          ),
          tabPanel(
            "Explicatii (3.c)",
            wellPanel(
              h4(
                paste(
                  "De ce probabilitatea empirică aproximează bine",
                  "probabilitatea teoretică?"
                )
              ),
              p(
                "Aceasta este o aplicație directă a ",
                strong("Legii Numerelor Mari (Law of Large Numbers)"), "."
              ),
              p(
                paste(
                  "Legea spune că media eșantionului (frecvența relativă a",
                  "evenimentului) converge probabilistic către media populației",
                  "(probabilitate teoretică) atunci când dimensiunea",
                  "eșantionului (n) crește."
                )
              ),
              p(
                paste(
                  "Astfel, cu n=5000 sau n=10000 simulări, estimarea noastră,",
                  "este foarte precisă."
                )
              )
            )
          )
        )
      )
    )
  )
}

server_ex3 <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Logica de simulare
    sim_data <- reactive({
      n <- input$n_sim
      max_ret <- input$max_retry
      p <- input$p_succes

      # Vectori
      t_total <- numeric(n)
      i_final <- numeric(n)
      n_total <- numeric(n)

      # Simulare Monte Carlo
      for (i in 1:n) {
        att <- 0
        timp <- 0
        succes <- FALSE

        while (att < max_ret && !succes) {
          att <- att + 1
          # Timp de răspuns pentru o încercare
          # (Exponential cu rate=2 => medie 0.5s)
          timp <- timp + rexp(1, rate = 2)

          if (runif(1) < p) {
            succes <- TRUE
          } else {
            # Backoff penalty (0.2s) dacă eșuează
            timp <- timp + 0.2
          }
        }

        t_total[i] <- timp
        i_final[i] <- as.numeric(succes)
        n_total[i] <- att
      }

      data.frame(T = t_total, I = i_final, N = n_total)
    })

    # 3.a) Calcul Probabilități
    output$tabProb <- renderTable({
      df <- sim_data()
      t0 <- input$t0_SLA
      n0 <- input$n0_at

      ev_a <- df$I == 1 # Succes
      ev_b <- df$T <= t0 # SLA Met
      ev_c <- df$N <= n0 # Efficiency
      # Cel puțin un eșec
      ev_d_logic <- !(df$N == 1 & df$I == 1)

      data.frame(
        Simbol = c("P(A)", "P(B)", "P(C)", "P(A ∩ B)", "P(A ∪ D)"),
        Eveniment = c(
          "Succes Final", "Timp <= t0 (SLA)", "Încercări <= n0",
          "Succes rapid", "Succes SAU Eșecuri"
        ),
        Probabilitate = c(
          mean(ev_a), mean(ev_b), mean(ev_c),
          mean(ev_a & ev_b), mean(ev_a | ev_d_logic)
        )
      )
    })

    # 3.b) Verificare Formula
    output$verificareFormula <- renderText({
      df <- sim_data()

      ev_a <- df$I == 1
      # D: Cel puțin un eșec
      ev_d <- !(df$N == 1 & df$I == 1)

      # P(A U D)
      p_reun_empiric <- mean(ev_a | ev_d)

      # Formula: P(A) + P(D) - P(A n D)
      p_a_val <- mean(ev_a)
      p_d_val <- mean(ev_d)
      p_inter <- mean(ev_a & ev_d)

      p_formula <- p_a_val + p_d_val - p_inter

      paste0(
        "Verificăm: P(A ∪ D) = P(A) + P(D) - P(A ∩ D)\n\n",
        "P(A ∪ D) [Empiric]:   ", round(p_reun_empiric, 6), "\n",
        "Calcul Formula:       ", round(p_formula, 6), "\n",
        "  -> P(A) = ", round(p_a_val, 4), "\n",
        "  -> P(D) = ", round(p_d_val, 4), "\n",
        "  -> P(A ∩ D) = ", round(p_inter, 4), "\n\n",
        "Diferența: ", abs(p_reun_empiric - p_formula), "\n",
        "Concluzie: Formula este verificată numeric!"
      )
    })

    # Grafice
    output$distrTimp <- renderPlot({
      hist(
        sim_data()$T,
        col = "lightblue",
        main = "Distribuția Timpului Total (T)",
        xlab = "Secunde (s)"
      )
      abline(v = input$t0_SLA, col = "red", lwd = 2, lty = 2)
    })

    output$distrIncercari <- renderPlot({
      barplot(
        table(sim_data()$N),
        col = "orange",
        main = "Frecvența nr. încercări (N)",
        xlab = "Nr. Încercări"
      )
      abline(v = input$n0_at, col = "blue", lwd = 2, lty = 2)
    })
  })
}
