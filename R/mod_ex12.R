library(shiny)
library(ggplot2)

ui_ex12 <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Vizualizare Statistică"),
    sidebarLayout(
      sidebarPanel(
        h4("1. Parametri Simulare"),
        sliderInput(ns("n_sim"), "Număr Simulări (N):", 1000, 10000, 5000,
          step = 500
        ),
        sliderInput(ns("p_succes"), "Prob. Succes (p):", 0.1, 0.9, 0.6,
          step = 0.1
        ),
        sliderInput(ns("max_retry"), "Max Retries:", 1, 10, 5),
        sliderInput(ns("lambda"), "Rata (Lambda):", 0.5, 5.0, 2.0),
        hr(),
        h4("2. Parametri Profit"),
        numericInput(ns("reward"), "Recompensă Succes:", value = 50),
        numericInput(ns("cost_time"), "Cost per Secundă (T):", value = 5),
        helpText("Profit = Reward * I(Succes) - Cost * T"),
        hr(),
        h4("3. Scenarii Comparate"),
        radioButtons(ns("scenario_type"), "Compară:",
          choices = c(
            "Succes vs Eșec (Același Scenariu)" = "outcome",
            "Scenariul A vs Scenariul B (Parametri Diferiți)" = "scenario"
          )
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'scenario'", ns("scenario_type")),
          h5("Parametri Scenariul B:"),
          sliderInput(ns("p_succes_b"), "Prob. Succes B:", 0.1, 0.9, 0.4,
            step = 0.1
          ),
          sliderInput(ns("lambda_b"), "Rata B:", 0.5, 5.0, 1.0)
        )
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Histograme (a)",
            h4("Distribuția Timpului Total (T)"),
            plotOutput(ns("histT")),
            hr(),
            h4("Distribuția Profitului"),
            plotOutput(ns("histProfit"))
          ),
          tabPanel(
            "Boxplot-uri (b)",
            h4("Boxplot T condiționat"),
            plotOutput(ns("boxT")),
            helpText(
              "Analizăm distribuția T în funcție de rezultatul simulării."
            )
          ),
          tabPanel(
            "Interpretare (c)",
            uiOutput(ns("statsOutput")),
            uiOutput(ns("interpretationText"))
          )
        )
      )
    )
  )
}

server_ex12 <- function(id) {
  moduleServer(id, function(input, output, session) {
    # --- SIMULATION LOGIC ---
    sim_data <- reactive({
      n <- input$n_sim

      # Scenario A params
      p_a <- input$p_succes
      max_ret <- input$max_retry
      lam_a <- input$lambda
      rew <- input$reward
      cost <- input$cost_time

      simulate_scenario <- function(n_runs, p, lam, scenario_label) {
        t_vals <- numeric(n_runs)
        is_success <- logical(n_runs)

        for (i in 1:n_runs) {
          attempts <- 0
          curr_t <- 0
          succ <- FALSE
          while (attempts < max_ret && !succ) {
            attempts <- attempts + 1
            curr_t <- curr_t + rexp(1, rate = lam)
            if (runif(1) < p) {
              succ <- TRUE
            }
          }
          t_vals[i] <- curr_t
          is_success[i] <- succ
        }

        profit <- (as.numeric(is_success) * rew) - (cost * t_vals)
        outcome_label <- ifelse(is_success, "Succes", "Eșec")

        data.frame(
          Scenario = scenario_label,
          TimeVal = t_vals,
          Outcome = outcome_label,
          Profit = profit
        )
      }

      df_a <- simulate_scenario(n, p_a, lam_a, "Scenariul A")

      if (input$scenario_type == "scenario") {
        # Simulate Scenario B
        p_b <- input$p_succes_b
        lam_b <- input$lambda_b
        df_b <- simulate_scenario(n, p_b, lam_b, "Scenariul B")
        rbind(df_a, df_b)
      } else {
        # Only Scenario A
        df_a
      }
    })

    # --- PLOTS ---

    # Histogram T
    output$histT <- renderPlot({
      df <- sim_data()
      ggplot(df, aes(x = .data$TimeVal, fill = .data$Scenario)) +
        geom_histogram(
          bins = 50, alpha = 0.6, position = "identity",
          color = "white"
        ) +
        labs(
          title = "Histograma Timpului Total (T)", x = "T (secunde)",
          y = "Frecvență"
        ) +
        theme_minimal()
    })

    # Histogram Profit
    output$histProfit <- renderPlot({
      df <- sim_data()
      ggplot(df, aes(x = .data$Profit, fill = .data$Scenario)) +
        geom_histogram(
          bins = 50, alpha = 0.6, position = "identity",
          color = "white"
        ) +
        labs(
          title = "Histograma Profitului", x = "Profit (Unități Monetare)",
          y = "Frecvență"
        ) +
        theme_minimal()
    })

    # Boxplot T
    output$boxT <- renderPlot({
      df <- sim_data()

      if (input$scenario_type == "outcome") {
        # Conditioned by Success/Failure within Scenario A
        ggplot(df, aes(
          x = .data$Outcome, y = .data$TimeVal,
          fill = .data$Outcome
        )) +
          geom_boxplot() +
          labs(
            title = "Boxplot T: Succes vs Eșec",
            x = "Rezultat",
            y = "T (secunde)"
          ) +
          theme_minimal()
      } else {
        # Conditioned by Scenario A vs B
        ggplot(df, aes(
          x = .data$Scenario, y = .data$TimeVal,
          fill = .data$Scenario
        )) +
          geom_boxplot() +
          labs(
            title = "Boxplot T: Scenariul A vs B", x = "Scenariu",
            y = "T (secunde)"
          ) +
          theme_minimal()
      }
    })

    # --- STATISTICS & INTERPRETATION ---

    output$statsOutput <- renderUI({
      df <- sim_data()

      # Grouping variable
      grp_var <- if (input$scenario_type == "outcome") {
        df$Outcome
      } else {
        df$Scenario
      }

      stats <- aggregate(df$TimeVal,
        by = list(Grup = grp_var),
        FUN = function(x) {
          c(
            Median = median(x),
            IQR = IQR(x),
            SD = sd(x),
            Min = min(x),
            Max = max(x)
          )
        }
      )

      # Format table manually for UI
      res <- do.call(data.frame, stats) # flatten
      colnames(res) <- c("Grup", "Mediana", "IQR", "DevStd", "Min", "Max")

      tagList(
        h4("Statistici Descriptive pentru T"),
        renderTable(res, digits = 4)
      )
    })

    output$interpretationText <- renderUI({
      HTML(paste0(
        "<h3>c) Interpretare</h3>",
        "<ul>",
        "<li><b>Mediana:</b> Indică valoarea 'centrală' a timpului. ",
        "Spre deosebire de medie, nu e afectată de valorile extreme.</li>",
        "<li><b>IQR (Interquartile Range):</b> ",
        "Măsoară împrăștierea mijlocului ",
        "distribuției (diferența dintre percentila 75 și 25). ",
        "Un IQR mare înseamnă impredictibilitate ridicată.</li>",
        "<li><b>Outlieri:</b> Punctele din afara 'mustăților' boxplot-urilor. ",
        "În simulările de latență (Exponențială), outlierii superiori sunt ",
        "frecvenți (coada lungă), reprezentând utilizatorii care așteaptă ",
        "foarte mult.</li>",
        "<li><b>Profit:</b> Observați cum Profitul are o distribuție bimodală ",
        "sau asimetrică, fiind determinat puternic de succes/eșec (Reward) ",
        "și apoi erodat de timp (Cost).</li>",
        "</ul>"
      ))
    })
  })
}
