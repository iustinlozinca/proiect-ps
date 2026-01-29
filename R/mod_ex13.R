library(shiny)
library(ggplot2)

ui_ex13 <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Analiza de Sinteza"),
    sidebarLayout(
      sidebarPanel(
        h4("Parametri Sistem"),
        sliderInput(ns("n_sim"), "Simulari:",
          min = 1000, max = 10000, value = 5000, step = 1000
        ),
        sliderInput(ns("p_succes"), "Probabilitate succes (p):",
          min = 0.1, max = 1.0, value = 0.8, step = 0.05
        ),
        sliderInput(ns("max_retry"), "Nr. maxim retry:",
          min = 2, max = 10, value = 5
        ),
        hr(),
        h4("Parametri Economici"),
        numericInput(ns("castig"), "Castig/succes (RON):", value = 5),
        numericInput(ns("cost_churn"), "Cost churn (RON):", value = 500),
        numericInput(ns("penalitate"), "Penalitate SLA (RON):", value = 10)
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Probabilitate Empirica (a)",
            h4("Rolul Probabilitatii Empirice"),
            plotOutput(ns("plotConvergenta")),
            verbatimTextOutput(ns("textEmpirica"))
          ),
          tabPanel(
            "Conditionari (b)",
            h4("Ce Informatii Aduc Conditionarile"),
            tableOutput(ns("tabConditionari")),
            plotOutput(ns("plotConditionari")),
            verbatimTextOutput(ns("textConditionari"))
          ),
          tabPanel(
            "Inegalitati (c)",
            h4("Utilitatea Inegalitatilor Probabilistice"),
            tableOutput(ns("tabInegalitati")),
            verbatimTextOutput(ns("textInegalitati"))
          ),
          tabPanel(
            "Tehnic → Economic (d)",
            h4("Legatura Performanta Tehnica - Impact Economic"),
            plotOutput(ns("plotTehnicEconomic")),
            verbatimTextOutput(ns("textTehnicEconomic"))
          ),
          tabPanel(
            "Sensibilitate (e)",
            h4("Parametrii cu Cea Mai Mare Influenta"),
            plotOutput(ns("plotSensibilitate")),
            uiOutput(ns("textSensibilitate"))
          )
        )
      )
    )
  )
}

server_ex13 <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Simulare de baza
    sim_data <- reactive({
      n <- input$n_sim
      p <- input$p_succes
      max_r <- input$max_retry

      t_vec <- numeric(n)
      i_vec <- numeric(n)
      n_vec <- numeric(n)

      for (i in 1:n) {
        att <- 0
        timp <- 0
        succes <- FALSE
        while (att < max_r && !succes) {
          att <- att + 1
          timp <- timp + rexp(1, rate = 2)
          if (runif(1) < p) {
            succes <- TRUE
          } else {
            timp <- timp + 0.2
          }
        }
        t_vec[i] <- timp
        i_vec[i] <- as.numeric(succes)
        n_vec[i] <- att
      }
      data.frame(Timp = t_vec, Succes = i_vec, Nr_incercari = n_vec)
    })

    # 13.a) Convergenta probabilitatii empirice
    output$plotConvergenta <- renderPlot({
      df <- sim_data()
      n <- nrow(df)

      # Calculam P(A) empiric pentru diferite dimensiuni
      sizes <- seq(100, n, by = 100)
      p_emp <- sapply(sizes, function(k) mean(df$Succes[1:k]))
      p_teor <- 1 - (1 - input$p_succes)^input$max_retry

      plot_df <- data.frame(n = sizes, P_empiric = p_emp, P_teoretic = p_teor)

      ggplot(plot_df, aes(x = n)) +
        geom_line(aes(y = .data$P_empiric),
          color = "steelblue", linewidth = 1
        ) +
        geom_hline(
          yintercept = p_teor, color = "red",
          linetype = "dashed", linewidth = 1
        ) +
        labs(
          title = "Convergenta P(A) Empiric → P(A) Teoretic",
          subtitle = paste("P(A) teoretic =", round(p_teor, 4)),
          x = "Numar de simulari (n)", y = "P(Succes)"
        ) +
        theme_minimal()
    })

    output$textEmpirica <- renderText({
      df <- sim_data()
      p_emp <- mean(df$Succes)
      p_teor <- 1 - (1 - input$p_succes)^input$max_retry

      paste0(
        "ROLUL PROBABILITATII EMPIRICE\n",
        "=============================\n\n",
        "P(A) empiric = ", round(p_emp, 4), "\n",
        "P(A) teoretic = ", round(p_teor, 4), "\n",
        "Diferenta = ", round(abs(p_emp - p_teor), 5), "\n\n",
        "Conform Legii Numerelor Mari, probabilitatea empirica\n",
        "converge catre cea teoretica cand n → ∞.\n\n",
        "Utilitate: Cand nu cunoastem distributia exacta,\n",
        "putem estima probabilitatile prin simulare/observatie."
      )
    })

    # 13.b) Conditionari
    output$tabConditionari <- renderTable(
      {
        df <- sim_data()

        p_a <- mean(df$Succes)
        p_a_n1 <- mean(df$Succes[df$Nr_incercari == 1])
        p_a_n_max <- mean(df$Succes[df$Nr_incercari == input$max_retry])

        e_t <- mean(df$Timp)
        e_t_succes <- mean(df$Timp[df$Succes == 1])
        e_t_esec <- if (sum(df$Succes == 0) > 0) {
          mean(df$Timp[df$Succes == 0])
        } else {
          NA
        }

        data.frame(
          Masura = c(
            "P(A)", "P(A | N=1)", paste0("P(A | N=", input$max_retry, ")"),
            "E(T)", "E(T | succes)", "E(T | esec)"
          ),
          Valoare = c(
            p_a, p_a_n1, p_a_n_max, e_t, e_t_succes,
            if (is.na(e_t_esec)) NA else e_t_esec
          ),
          Interpretare = c(
            "Rata globala de succes",
            "Succes din prima = 100%",
            "Include si esecurile totale",
            "Timp mediu global",
            "Timp mediu pentru succese",
            "Timp mediu pentru esecuri"
          )
        )
      },
      digits = 4,
      na = "N/A"
    )

    output$plotConditionari <- renderPlot({
      df <- sim_data()

      df$Rezultat <- ifelse(df$Succes == 1, "Succes", "Esec")

      ggplot(df, aes(
        x = .data$Rezultat, y = .data$Timp, fill = .data$Rezultat
      )) +
        geom_boxplot(alpha = 0.7) +
        scale_fill_manual(
          values = c("Succes" = "#2ecc71", "Esec" = "#e74c3c")
        ) +
        labs(
          title = "E(T | I) - Timp Conditionat de Rezultat",
          x = "", y = "Timp (s)"
        ) +
        theme_minimal() +
        theme(legend.position = "none")
    })

    output$textConditionari <- renderText({
      df <- sim_data()
      e_t_s <- mean(df$Timp[df$Succes == 1])
      e_t_e <- if (sum(df$Succes == 0) > 0) {
        mean(df$Timp[df$Succes == 0])
      } else {
        NA
      }

      paste0(
        "CE INFORMATII ADUC CONDITIONARILE\n",
        "==================================\n\n",
        "1. P(A|N=1) = 1 deoarece daca N=1, cererea s-a oprit\n",
        "   la prima incercare, deci a reusit.\n\n",
        "2. E(T|succes) = ", round(e_t_s, 2), "s\n",
        "   E(T|esec) = ",
        if (!is.na(e_t_e)) round(e_t_e, 2) else "N/A", "s\n\n",
        if (!is.na(e_t_e)) {
          paste0(
            "   Esecurile dureaza mai mult pentru ca parcurg\n",
            "   toate cele ", input$max_retry, " incercari.\n\n"
          )
        } else {
          ""
        },
        "Conditionarile permit segmentarea analizei si\n",
        "identificarea comportamentelor diferite pe subgrupuri."
      )
    })

    # 13.c) Inegalati probabilistice
    output$tabInegalitati <- renderTable(
      {
        df <- sim_data()
        t_vals <- df$Timp

        mu <- mean(t_vals)
        sigma <- sd(t_vals)

        # Markov Inequality
        a_markov <- 2 * mu
        p_markov_emp <- mean(t_vals >= a_markov)
        p_markov_bound <- mu / a_markov

        # Cebisev: P(|T - μ| >= kσ) <= 1/k²
        k <- 2
        p_ceb_emp <- mean(abs(t_vals - mu) >= k * sigma)
        p_ceb_bound <- 1 / k^2

        data.frame(
          Inegalitate = c("Markov", "Cebisev (k=2)"),
          `P_empiric` = c(p_markov_emp, p_ceb_emp),
          `Limita_sup` = c(p_markov_bound, p_ceb_bound),
          Verificat = c(
            p_markov_emp <= p_markov_bound,
            p_ceb_emp <= p_ceb_bound
          )
        )
      },
      digits = 4
    )

    output$textInegalitati <- renderText({
      df <- sim_data()
      mu <- mean(df$Timp)
      sigma <- sd(df$Timp)

      paste0(
        "UTILITATEA INEGALITATILOR PROBABILISTICE\n",
        "========================================\n\n",
        "Markov: P(T ≥ a) ≤ E(T)/a\n",
        "  → Ofera o limita superioara fara a cunoaste distributia\n\n",
        "Cebisev: P(|T - μ| ≥ kσ) ≤ 1/k²\n",
        "  → Garanteaza ca valorile extreme sunt rare\n\n",
        "Pentru T: μ = ", round(mu, 2), "s, σ = ", round(sigma, 2), "s\n\n",
        "UTILITATE PRACTICA:\n",
        "- Garantii worst-case pentru SLA\n",
        "- Dimensionarea buffer-elor de timp\n",
        "- Evaluarea riscului fara distributie exacta"
      )
    })

    # 13.d) Tehnic → Economic
    output$plotTehnicEconomic <- renderPlot({
      p_vals <- seq(0.5, 0.95, by = 0.05)
      results <- data.frame(p = p_vals, Profit = NA, SLA_Met = NA)

      for (j in seq_along(p_vals)) {
        profits <- numeric(200)
        sla <- numeric(200)

        for (i in 1:200) {
          # Simulare simplificata
          n_cereri <- 100
          succese <- rbinom(1, n_cereri, p_vals[j])
          timpi <- rexp(n_cereri, rate = 2)
          violations <- sum(timpi > 2)

          profits[i] <- succese * input$castig - violations * input$penalitate
          sla[i] <- 1 - violations / n_cereri
        }
        results$Profit[j] <- mean(profits)
        results$SLA_Met[j] <- mean(sla) * 100
      }

      ggplot(results, aes(x = p)) +
        geom_line(aes(y = .data$Profit), color = "steelblue", linewidth = 1.2) +
        geom_point(aes(y = .data$Profit), color = "steelblue", size = 3) +
        labs(
          title = "Legatura: Probabilitate Succes → Profit",
          x = "Probabilitate Succes (p)", y = "Profit Mediu (RON)"
        ) +
        theme_minimal()
    })

    output$textTehnicEconomic <- renderText({
      paste0(
        "LEGATURA PERFORMANTA TEHNICA - IMPACT ECONOMIC\n",
        "===============================================\n\n",
        "Parametri tehnici → Rezultate economice:\n\n",
        "1. p (prob. succes) ↑ → Profit ↑\n",
        "   Mai multe cereri reusite = mai mult castig\n\n",
        "2. Latenta ↑ → Penalitati SLA ↑ → Profit ↓\n",
        "   Timpi mari de raspuns genereaza costuri\n\n",
        "3. Churn ↑ → Pierderi mari\n",
        "   Un client pierdut = ", input$cost_churn, " RON\n",
        "   Echivalent cu ", input$cost_churn / input$castig,
        " cereri reusite\n\n",
        "Concluzie: imbunatatirile tehnice au impact\n",
        "economic direct si cuantificabil."
      )
    })

    # 13.e) Analiza de sensibilitate
    output$plotSensibilitate <- renderPlot({
      # Variaza fiecare parametru si masoara impactul
      base_profit <- function(p, pen, churn_rate) {
        n <- 100
        succese <- n * p
        violations <- n * 0.14 # ~14% depasesc 2s pentru exp(rate=2)
        churn <- churn_rate
        succese * input$castig - violations * pen - churn * input$cost_churn
      }

      # Baza
      p_base <- 0.8
      pen_base <- 10
      churn_base <- 0.02
      profit_base <- base_profit(p_base, pen_base, churn_base)

      # Variatii ±20%
      impact <- data.frame(
        Parametru = c(
          "p_succes\n(+10%)", "p_succes\n(-10%)",
          "Penalitate SLA\n(+50%)", "Penalitate SLA\n(-50%)",
          "Churn rate\n(+50%)", "Churn rate\n(-50%)"
        ),
        Impact = c(
          base_profit(0.88, pen_base, churn_base) - profit_base,
          base_profit(0.72, pen_base, churn_base) - profit_base,
          base_profit(p_base, 15, churn_base) - profit_base,
          base_profit(p_base, 5, churn_base) - profit_base,
          base_profit(p_base, pen_base, 0.03) - profit_base,
          base_profit(p_base, pen_base, 0.01) - profit_base
        )
      )

      impact$Culoare <- ifelse(impact$Impact > 0, "pozitiv", "negativ")

      ggplot(impact, aes(
        x = reorder(.data$Parametru, .data$Impact),
        y = .data$Impact,
        fill = .data$Culoare
      )) +
        geom_bar(stat = "identity", alpha = 0.8) +
        scale_fill_manual(
          values = c("pozitiv" = "#2ecc71", "negativ" = "#e74c3c")
        ) +
        coord_flip() +
        labs(
          title = "Analiza de Sensibilitate: Impact pe Profit",
          x = "", y = "Variatie Profit (RON)"
        ) +
        theme_minimal() +
        theme(legend.position = "none")
    })

    output$textSensibilitate <- renderUI({
      HTML(paste0(
        "<h4>Parametrii cu Cea Mai Mare Influenta</h4>",
        "<ol>",
        "<li><b>Probabilitatea de succes (p)</b> - ",
        "impact direct pe venituri</li>",
        "<li><b>Rata de churn</b> - pierderi mari per eveniment</li>",
        "<li><b>Penalitatile SLA</b> - impact moderat dar constant</li>",
        "</ol>",
        "<h4>Recomandari pentru imbunatatire</h4>",
        "<ul>",
        "<li>Prioritate 1: Reducerea churn-ului (cost mare per eveniment)</li>",
        "<li>Prioritate 2: Cresterea ratei de succes (p)</li>",
        "<li>Prioritate 3: Optimizarea latentei pentru SLA</li>",
        "</ul>",
        "<h4>Compromisuri</h4>",
        "<p>Cresterea lui p poate necesita ",
        "infrastructura mai costisitoare.<br>",
        "Reducerea churn-ului poate necesita investitii in UX.<br>",
        "Analiza cost-beneficiu determina prioritatile.</p>"
      ))
    })
  })
}
