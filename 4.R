library(shiny)
library(ggplot2)

# UI
ui <- fluidPage(
  titlePanel("Exercițiul 4: Variabile Aleatoare (N, F) cu Churn"),
  sidebarLayout(
    sidebarPanel(
      helpText(
        "Analiza perechii (N, F) unde N = Nr. Încercări, F = Nr. Eșecuri."
      ),
      sliderInput(
        "n_sim", "Număr de simulări:", 100, 5000, 1000,
        step = 100
      ),
      sliderInput(
        "p_success", "Probabilitate succes (p):", 0.1, 0.9, 0.5,
        step = 0.1
      ),
      sliderInput("max_retry", "Număr maxim încercări (N_max):", 2, 10, 5),
      hr(),
      h4("Setări Churn (Abandon)"),

      # Churn Aleator (Unconditional)
      sliderInput(
        "prob_abandon", "Probabilitate abandon aleator (q):",
        0, 0.5, 0,
        step = 0.01
      ),
      helpText(
        "Probabilitatea ca utilizatorul să renunțe după orice încercare,",
        "indiferent de rezultat."
      ),
      hr(),

      # Churn Conditionat (Conditional)
      checkboxInput(
        "enable_churn", "Activează Churn condiționat de eșecuri",
        value = FALSE
      ),
      conditionalPanel(
        condition = "input.enable_churn == true",
        sliderInput(
          "churn_threshold", "Eșecuri consecutive până la abandon:", 1, 5, 2
        ),
        helpText(
          "Dacă utilizatorul are acest număr de eșecuri consecutive,",
          "renunță imediat."
        )
      ),
      hr(),
      helpText(
        "Observați cum F depinde de N și cum Churn-ul modifică distribuția."
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Distribuția Comună (Heatmap)",
          plotOutput("jointPlot"),
          tableOutput("jointTable"),
          helpText("Valorile reprezintă P(N=n, F=f).")
        ),
        tabPanel(
          "Marginale & Independență",
          h4("Distribuții Marginale"),
          fluidRow(
            column(6, plotOutput("margN")),
            column(6, plotOutput("margF"))
          ),
          hr(),
          h4("Test de Independență"),
          verbatimTextOutput("indepTest"),
          uiOutput("indepInterp")
        )
      )
    )
  )
)

# SERVER
server <- function(input, output) {
  # Simulare date
  sim_data <- reactive({
    n <- input$n_sim
    p <- input$p_success
    max_r <- input$max_retry

    q_abandon <- input$prob_abandon

    use_churn_cond <- input$enable_churn
    churn_limit <- input$churn_threshold

    n_vec <- numeric(n)
    f_vec <- numeric(n)

    for (i in 1:n) {
      attempts <- 0
      failures <- 0
      consecutive_failures <- 0
      success <- FALSE
      abandoned <- FALSE

      while (attempts < max_r && !success && !abandoned) {
        attempts <- attempts + 1

        # Simulare încercare
        if (runif(1) < p) {
          success <- TRUE
          consecutive_failures <- 0
        } else {
          failures <- failures + 1
          consecutive_failures <- consecutive_failures + 1

          # Verificare Churn Condiționat (Eșecuri)
          if (use_churn_cond && consecutive_failures >= churn_limit) {
            abandoned <- TRUE
          }
        }

        # Verificare Churn Aleator (q)
        # Doar dacă nu a reușit deja sau nu a abandonat deja
        if (!success && !abandoned) {
          if (runif(1) < q_abandon) {
            abandoned <- TRUE
          }
        }
      }
      n_vec[i] <- attempts
      f_vec[i] <- failures
    }

    data.frame(Trials = n_vec, Failures = f_vec)
  })

  # Tabel Distribuție Comună
  output$jointTable <- renderTable(
    {
      df <- sim_data()
      # Tabel de contingență normalizat (frecvențe relative)
      tab <- table(df$Trials, df$Failures) / nrow(df)
      as.data.frame.matrix(tab)
    },
    rownames = TRUE
  )

  # Heatmap
  output$jointPlot <- renderPlot({
    df <- sim_data()

    # Agregăm datele folosind Base R (table -> data.frame)
    tbl <- table(
      factor(df$Trials, levels = sort(unique(df$Trials))),
      factor(df$Failures, levels = sort(unique(df$Failures)))
    )

    counts <- as.data.frame(tbl)
    names(counts) <- c("Trials", "Failures", "Count")
    counts$Prob <- counts$Count / sum(counts$Count)

    ggplot(counts, aes(x = Failures, y = Trials, fill = Prob)) +
      geom_tile(color = "white") +
      geom_text(aes(label = round(Prob, 3)), color = "black") +
      scale_fill_gradient(low = "white", high = "red") +
      labs(
        title = "Distribuția Comună P(N, F)",
        x = "F (Eșecuri)", y = "N (Încercări)"
      ) +
      theme_minimal()
  })

  # Marginale
  output$margN <- renderPlot({
    df <- sim_data()
    ggplot(df, aes(x = factor(Trials))) +
      geom_bar(
        aes(y = after_stat(count) / sum(after_stat(count))),
        fill = "skyblue"
      ) +
      labs(title = "Marginala P(N)", x = "N", y = "Probabilitate") +
      ylim(0, 1)
  })

  output$margF <- renderPlot({
    df <- sim_data()
    ggplot(df, aes(x = factor(Failures))) +
      geom_bar(
        aes(y = after_stat(count) / sum(after_stat(count))),
        fill = "lightgreen"
      ) +
      labs(title = "Marginala P(F)", x = "F", y = "Probabilitate") +
      ylim(0, 1)
  })

  # Test Independență
  output$indepTest <- renderPrint({
    df <- sim_data()
    tbl <- table(df$Trials, df$Failures)
    chisq.test(tbl)
  })

  output$indepInterp <- renderUI({
    has_churn <- input$enable_churn || input$prob_abandon > 0

    HTML(paste0(
      "<br><p><b>Interpretare:</b></p>",
      "<p>Valoarea <b>p-value</b> extrem de mică indică faptul că ",
      "respingem ipoteza nulă de independență.</p>",
      "<p><b>N și F sunt dependente</b>.</p>",
      if (has_churn) {
        paste0(
          "<p><b>Notă:</b> Prezența Churn-ului (aleator sau condiționat) ",
          "reduce lungimea medie a sesiunilor (N scade).</p>"
        )
      } else {
        ""
      }
    ))
  })
}

shinyApp(ui, server)
