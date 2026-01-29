library(shiny)
library(ggplot2)

ui_ex3 <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Cereri, Retry-uri si Evenimente"),
    sidebarLayout(
      sidebarPanel(
        h4("Parametri Simulare"),
        sliderInput(ns("n_sim"), "Numar utilizatori:", 1000, 10000, 5000),
        sliderInput(
          ns("p_succes"), "Probabilitate succes (per incercare):", 0.1, 0.9, 0.7
        ),
        sliderInput(ns("max_retry"), "Nr. Maxim retry:", 1, 10, 3),
        hr(),
        h4("Praguri Evenimente"),
        sliderInput(
          ns("t0_SLA"), "Prag Timp (t0) - SLA:", 0.5, 10.0, 2.0,
          step = 0.5
        ),
        sliderInput(ns("n0_at"), "Prag incercari (n0) - Eficienta:", 1, 5, 1)
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
              "Histograma timpului total (T) si a numarului de incercari (N)."
            )
          ),
          tabPanel(
            "Probabilitati (a)",
            tableOutput(ns("tabProb")),
            helpText(
              "Probabilitatile empirice calculate prin",
              "numararea cazurilor favorabile / total cazuri."
            )
          ),
          tabPanel(
            "Verificare Formule (b)",
            verbatimTextOutput(ns("verificareFormula")),
            helpText(
              "Verificam egalitatea P(A ∪ D) = P(A) + P(D) - P(A ∩ D)."
            )
          ),
          tabPanel(
            "Explicatii (c)",
            wellPanel(
              h4(
                paste(
                  "De ce probabilitatea empirica aproximeaza bine",
                  "probabilitatea teoretica?"
                )
              ),
              p(
                "Aceasta este o aplicatie directa a ",
                strong("Legii Numerelor Mari (Law of Large Numbers)"), "."
              ),
              p(
                paste(
                  "Legea spune ca media esantionului (frecventa relativa a",
                  "evenimentului) converge probabilistic catre media",
                  "populatiei",
                  "(probabilitate teoretica) atunci cand dimensiunea",
                  "esantionului (n) creste."
                )
              ),
              p(
                paste(
                  "Astfel, cu n=5000 sau n=10000 simulari, estimarea noastra,",
                  "este foarte precisa."
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
          # Timp de raspuns pentru o incercare
          # (Exponential cu rate=2 => medie 0.5s)
          timp <- timp + rexp(1, rate = 2)

          if (runif(1) < p) {
            succes <- TRUE
          } else {
            # Backoff penalty (0.2s) daca esueaza
            timp <- timp + 0.2
          }
        }

        t_total[i] <- timp
        i_final[i] <- as.numeric(succes)
        n_total[i] <- att
      }

      data.frame(T = t_total, I = i_final, N = n_total)
    })

    # 3.a) Calcul Probabilitati
    output$tabProb <- renderTable({
      df <- sim_data()
      t0 <- input$t0_SLA
      n0 <- input$n0_at

      ev_a <- df$I == 1 # Succes
      ev_b <- df$T <= t0 # SLA Met
      ev_c <- df$N <= n0 # Efficiency
      # Cel putin un esec
      ev_d_logic <- !(df$N == 1 & df$I == 1)

      data.frame(
        Simbol = c("P(A)", "P(B)", "P(C)", "P(A ∩ B)", "P(A ∪ D)"),
        Eveniment = c(
          "Succes Final", "Timp <= t0 (SLA)", "incercari <= n0",
          "Succes rapid", "Succes SAU Esecuri"
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
      # D: Cel putin un esec
      ev_d <- !(df$N == 1 & df$I == 1)

      # P(A U D)
      p_reun_empiric <- mean(ev_a | ev_d)

      # Formula: P(A) + P(D) - P(A n D)
      p_a_val <- mean(ev_a)
      p_d_val <- mean(ev_d)
      p_inter <- mean(ev_a & ev_d)

      p_formula <- p_a_val + p_d_val - p_inter

      paste0(
        "Verificam: P(A ∪ D) = P(A) + P(D) - P(A ∩ D)\n\n",
        "P(A ∪ D) [Empiric]:   ", round(p_reun_empiric, 6), "\n",
        "Calcul Formula:       ", round(p_formula, 6), "\n",
        "  -> P(A) = ", round(p_a_val, 4), "\n",
        "  -> P(D) = ", round(p_d_val, 4), "\n",
        "  -> P(A ∩ D) = ", round(p_inter, 4), "\n\n",
        "Diferenta: ", abs(p_reun_empiric - p_formula), "\n",
        "Concluzie: Formula este verificata numeric!"
      )
    })

    # Grafice
    output$distrTimp <- renderPlot({
      hist(
        sim_data()$T,
        col = "lightblue",
        main = "Distributia Timpului Total (T)",
        xlab = "Secunde (s)"
      )
      abline(v = input$t0_SLA, col = "red", lwd = 2, lty = 2)
    })

    output$distrIncercari <- renderPlot({
      barplot(
        table(sim_data()$N),
        col = "orange",
        main = "Frecventa nr. incercari (N)",
        xlab = "Nr. incercari"
      )
      abline(v = input$n0_at, col = "blue", lwd = 2, lty = 2)
    })
  })
}
