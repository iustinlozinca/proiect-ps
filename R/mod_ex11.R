library(shiny)
library(ggplot2)

ui_ex11 <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Impact Economic"),
    sidebarLayout(
      sidebarPanel(
        h4("Parametri Simulare"),
        sliderInput(ns("n_zile"), "Număr de zile:",
          min = 100, max = 1000, value = 365, step = 50
        ),
        sliderInput(ns("cereri_zi"), "Cereri medii per zi:",
          min = 50, max = 500, value = 200, step = 50
        ),
        hr(),
        h4("Parametri Tehnici"),
        sliderInput(ns("p_succes"), "Probabilitate succes (p):",
          min = 0.5, max = 0.99, value = 0.85, step = 0.01
        ),
        sliderInput(ns("t0_SLA"), "Prag SLA (secunde):",
          min = 0.5, max = 5, value = 2, step = 0.5
        ),
        sliderInput(ns("prob_churn"), "Probabilitate churn per zi:",
          min = 0, max = 0.1, value = 0.02, step = 0.005
        ),
        hr(),
        h4("Parametri Economici (RON)"),
        numericInput(ns("castig_succes"), "Câștig per succes:", value = 5),
        numericInput(ns("pierdere_churn"), "Pierdere per churn:", value = 500),
        numericInput(ns("penalitate_SLA"), "Penalitate SLA:", value = 10)
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Profitul Zilnic (a)",
            h4("Definiția Variabilei Aleatoare"),
            uiOutput(ns("definitieProfit")),
            hr(),
            h4("Distribuția Profitului Zilnic"),
            plotOutput(ns("plotProfit")),
            verbatimTextOutput(ns("detaliiZi"))
          ),
          tabPanel(
            "Estimări Statistice (b)",
            h4("Statistici pentru Profitul Zilnic"),
            tableOutput(ns("tabStatistici")),
            hr(),
            h4("Interval de Încredere 95%"),
            verbatimTextOutput(ns("intervalIncredere")),
            hr(),
            h4("Evoluția Profitului Cumulat"),
            plotOutput(ns("plotCumulat"))
          ),
          tabPanel(
            "Compromisuri (c)",
            h4("Analiza Compromisurilor Tehnico-Economice"),
            plotOutput(ns("plotTradeoff")),
            hr(),
            uiOutput(ns("analizaCompromisuri"))
          )
        )
      )
    )
  )
}

server_ex11 <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Simulare profit zilnic
    sim_data <- reactive({
      n_zile <- input$n_zile
      cereri <- input$cereri_zi
      p <- input$p_succes
      t0 <- input$t0_SLA
      q_churn <- input$prob_churn

      g <- input$castig_succes
      c_churn <- input$pierdere_churn
      pen_sla <- input$penalitate_SLA

      profit_zi <- numeric(n_zile)
      succese_zi <- numeric(n_zile)
      sla_violations <- numeric(n_zile)
      churns_zi <- numeric(n_zile)

      for (i in 1:n_zile) {
        # Simulam cererile zilei
        n_cereri <- rpois(1, cereri) # Nr cereri variabil (Poisson)

        # Pentru fiecare cerere: succes/esec si timp
        succese <- rbinom(1, n_cereri, p)

        # Timpul per cerere (exponential)
        timpi <- rexp(n_cereri, rate = 2)
        violations <- sum(timpi > t0)

        # Churn (utilizatori pierduti)
        n_churn <- rbinom(1, 1, q_churn) # Max 1 churn/zi simplificat

        # Calculul profitului
        profit <- succese * g - n_churn * c_churn - violations * pen_sla

        profit_zi[i] <- profit
        succese_zi[i] <- succese
        sla_violations[i] <- violations
        churns_zi[i] <- n_churn
      }

      data.frame(
        Zi = 1:n_zile,
        Profit = profit_zi,
        Succese = succese_zi,
        ViolariSLA = sla_violations,
        Churns = churns_zi
      )
    })

    # 11.a) Definitia profitului
    output$definitieProfit <- renderUI({
      HTML(paste0(
        "<div style='background:#f8f9fa; padding:15px; border-radius:5px;'>",
        "<p><b>Profit zilnic (P):</b></p>",
        "<p style='font-size:1.1em;'>",
        "P = (Nr. Succese × ", input$castig_succes, " RON) − ",
        "(Nr. Churns × ", input$pierdere_churn, " RON) − ",
        "(Penalizari SLA × ", input$penalitate_SLA, " RON)</p>",
        "<hr>",
        "<p><b>Componente:</b></p>",
        "<ul>",
        "<li><b>Câștig per succes:</b> ", input$castig_succes, " RON</li>",
        "<li><b>Pierdere per churn:</b> ", input$pierdere_churn, " RON</li>",
        "<li><b>Penalitate SLA:</b> ", input$penalitate_SLA, " RON (când T > ",
        input$t0_SLA, "s)</li>",
        "</ul>",
        "</div>"
      ))
    })

    # Histograma profit
    output$plotProfit <- renderPlot({
      df <- sim_data()

      ggplot(df, aes(x = .data$Profit)) +
        geom_histogram(aes(y = after_stat(density)),
          bins = 30,
          fill = "steelblue", color = "white", alpha = 0.7
        ) +
        geom_vline(
          xintercept = mean(df$Profit), color = "red",
          linewidth = 1.2, linetype = "dashed"
        ) +
        geom_vline(xintercept = 0, color = "black", linewidth = 1) +
        labs(
          title = "Distribuția Profitului Zilnic",
          subtitle = paste0(
            "Linia roșie = media (", round(mean(df$Profit), 0),
            " RON) | Linia neagră = breakeven (profit = 0)"
          ),
          x = "Profit (RON)", y = "Densitate"
        ) +
        theme_minimal()
    })

    output$detaliiZi <- renderText({
      df <- sim_data()
      paste0(
        "Medie succese/zi: ", round(mean(df$Succese), 1), "\n",
        "Medie violări SLA/zi: ", round(mean(df$ViolariSLA), 1), "\n",
        "Total churns: ", sum(df$Churns), " în ", input$n_zile, " zile\n",
        "Zile cu pierdere (profit < 0): ", sum(df$Profit < 0)
      )
    })

    # 11.b) Statistici
    output$tabStatistici <- renderTable(
      {
        df <- sim_data()
        x <- df$Profit

        data.frame(
          Statistică = c(
            "Media (E[P])", "Varianța (Var[P])", "Deviația Standard",
            "Minim", "Maxim", "Mediana"
          ),
          Valoare = c(mean(x), var(x), sd(x), min(x), max(x), median(x)),
          Unitate = rep("RON", 6)
        )
      },
      digits = 2
    )

    output$intervalIncredere <- renderText({
      df <- sim_data()
      x <- df$Profit
      n <- length(x)

      # IC 95% pentru medie
      m <- mean(x)
      se <- sd(x) / sqrt(n)
      t_crit <- qt(0.975, df = n - 1)

      lower <- m - t_crit * se
      upper <- m + t_crit * se

      paste0(
        "Interval de încredere 95% pentru E[Profit]:\n",
        "[", round(lower, 2), " ; ", round(upper, 2), "] RON\n\n",
        "Interpretare: Cu 95% încredere, profitul mediu zilnic\n",
        "se află în acest interval.\n\n",
        "Eroare standard: ", round(se, 2), " RON\n",
        "Semiamplitudine IC: ±", round(t_crit * se, 2), " RON"
      )
    })

    # Profit cumulat
    output$plotCumulat <- renderPlot({
      df <- sim_data()
      df$ProfitCumulat <- cumsum(df$Profit)

      ggplot(df, aes(x = .data$Zi, y = .data$ProfitCumulat)) +
        geom_line(color = "steelblue", linewidth = 0.8) +
        geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
        labs(
          title = "Evoluția Profitului Cumulat",
          x = "Ziua", y = "Profit Cumulat (RON)"
        ) +
        theme_minimal()
    })

    # 11.c) Compromisuri
    output$plotTradeoff <- renderPlot({
      # Simulam pentru diferite valori de p
      p_vals <- seq(0.6, 0.99, by = 0.05)
      results <- data.frame(p = p_vals, MedieProfit = NA, RiscPierdere = NA)

      for (j in seq_along(p_vals)) {
        p <- p_vals[j]
        profits <- numeric(200)

        for (i in 1:200) {
          n_cereri <- rpois(1, input$cereri_zi)
          succese <- rbinom(1, n_cereri, p)
          timpi <- rexp(n_cereri, rate = 2)
          violations <- sum(timpi > input$t0_SLA)
          n_churn <- rbinom(1, 1, input$prob_churn)

          profits[i] <- succese * input$castig_succes -
            n_churn * input$pierdere_churn -
            violations * input$penalitate_SLA
        }
        results$MedieProfit[j] <- mean(profits)
        results$RiscPierdere[j] <- mean(profits < 0) * 100
      }

      ggplot(results, aes(x = p)) +
        geom_line(aes(y = .data$MedieProfit, color = "Profit Mediu"),
          linewidth = 1.2
        ) +
        geom_point(aes(y = .data$MedieProfit, color = "Profit Mediu"),
          size = 3
        ) +
        geom_line(aes(y = .data$RiscPierdere * 10, color = "Risc Pierdere (%)"),
          linewidth = 1.2, linetype = "dashed"
        ) +
        scale_y_continuous(
          name = "Profit Mediu (RON)",
          sec.axis = sec_axis(~ . / 10, name = "Risc Pierdere (%)")
        ) +
        scale_color_manual(values = c(
          "Profit Mediu" = "steelblue",
          "Risc Pierdere (%)" = "red"
        )) +
        labs(
          title = "Compromis: Probabilitate Succes vs Profit/Risc",
          x = "Probabilitate Succes (p)", color = ""
        ) +
        theme_minimal() +
        theme(legend.position = "bottom")
    })

    output$analizaCompromisuri <- renderUI({
      df <- sim_data()

      profit_mediu <- mean(df$Profit)
      risc <- mean(df$Profit < 0) * 100

      # Contributii
      contrib_succes <- mean(df$Succese) * input$castig_succes
      contrib_churn <- mean(df$Churns) * input$pierdere_churn
      contrib_sla <- mean(df$ViolariSLA) * input$penalitate_SLA

      HTML(paste0(
        "<h4>Descompunerea Profitului Mediu</h4>",
        "<table style='width:100%; border-collapse:collapse;'>",
        "<tr style='background:#e9ecef;'><th>Componentă</th>",
        "<th>Contribuție (RON/zi)</th></tr>",
        "<tr><td>+ Câștig din succese</td><td style='color:green;'>+",
        round(contrib_succes, 0), "</td></tr>",
        "<tr><td>− Pierdere din churn</td><td style='color:red;'>−",
        round(contrib_churn, 0), "</td></tr>",
        "<tr><td>− Penalități SLA</td><td style='color:orange;'>−",
        round(contrib_sla, 0), "</td></tr>",
        "<tr style='background:#d4edda;'><td><b>= Profit mediu</b></td>",
        "<td><b>", round(profit_mediu, 0), "</b></td></tr>",
        "</table>",
        "<hr><h4>Indicatori de Risc</h4>",
        "<p><b>Probabilitatea unei zile cu pierdere:</b> ",
        round(risc, 1), "%</p>",
        "<p><b>Coeficient de variație:</b> ",
        round(sd(df$Profit) / abs(mean(df$Profit)) * 100, 1),
        "% (variabilitate relativă)</p>",
        "<hr><h4>Observații</h4>",
        "<ul>",
        "<li>Câștigul din succese este componenta principală (+",
        round(contrib_succes, 0), " RON/zi)</li>",
        "<li>Un singur churn costă ", input$pierdere_churn,
        " RON, echivalent cu ",
        round(input$pierdere_churn / input$castig_succes, 0),
        " cereri reușite</li>",
        "<li>Creșterea lui p de la 0.85 la 0.95 crește profitul și ",
        "reduce riscul de pierdere</li>",
        "</ul>"
      ))
    })
  })
}
