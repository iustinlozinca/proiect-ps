library(shiny)
library(ggplot2)

ui_ex10 <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Churn (Pierderea Utilizatorilor)"),
    sidebarLayout(
      sidebarPanel(
        h4("Parametri Simulare"),
        sliderInput(ns("n_users"), "Numar de Utilizatori (N):",
          value = 1000, min = 100, max = 5000, step = 100
        ),
        sliderInput(ns("horizon"), "Orizont de Timp (H - Request-uri):",
          value = 100, min = 10, max = 500
        ),
        hr(),
        h4("1. Churn Aleator (Mecanism 1)"),
        sliderInput(ns("prob_q"), "Probabilitate Churn Aleator (q):",
          min = 0, max = 0.05, value = 0.005, step = 0.001
        ),
        helpText(
          "Probabilitatea ca un user sa plece 'din senin' la orice pas."
        ),
        hr(),
        h4("2. Churn Conditionat (Mecanism 2)"),
        sliderInput(ns("prob_p"), "Probabilitate Esec Request (p):",
          min = 0, max = 0.5, value = 0.1, step = 0.05
        ),
        sliderInput(ns("window_m"), "Fereastra de Monitorizare (m):",
          min = 1, max = 20, value = 5, step = 1
        ),
        sliderInput(ns("threshold_k"), "Prag de Esecuri (k):",
          min = 1, max = 20, value = 3, step = 1
        ),
        helpText(
          "Userul pleaca daca are >= k esecuri in ultimele m request-uri."
        ),
        hr(),
        actionButton(ns("run_sim"), "Actualizeaza Simularea",
          class = "btn-primary", width = "100%"
        )
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Vizualizare (a, b)",
            h4("Curba de Supravietuire"),
            plotOutput(ns("survivalPlot")),
            hr(),
            h4("Distributia Cauzelor de Churn"),
            plotOutput(ns("reasonPlot"))
          ),
          tabPanel(
            "Interpretare & Comparatie (c)",
            h4("Estimari Statistice"),
            tableOutput(ns("statsTable")),
            hr(),
            uiOutput(ns("interpretation"))
          )
        )
      )
    )
  )
}

server_ex10 <- function(id) {
  moduleServer(id, function(input, output, session) {
    sim_data <- reactive({
      input$run_sim

      n_total <- input$n_users
      h_steps <- input$horizon
      q <- input$prob_q
      p <- input$prob_p
      m <- input$window_m
      k <- input$threshold_k

      # Simulation logic
      req_failures <- matrix(rbinom(n_total * h_steps, 1, p),
        nrow = n_total, ncol = h_steps
      )
      rand_churns <- matrix(rbinom(n_total * h_steps, 1, q),
        nrow = n_total, ncol = h_steps
      )

      churn_step <- rep(h_steps + 1, n_total)
      churn_reason <- rep("Survived", n_total)
      active_users <- rep(TRUE, n_total)

      for (t in 1:h_steps) {
        if (!any(active_users)) break

        # 1. Random Churn
        is_rand_churn <- active_users & (rand_churns[, t] == 1)

        if (any(is_rand_churn)) {
          churn_step[is_rand_churn] <- t
          churn_reason[is_rand_churn] <- "Aleator (q)"
          active_users[is_rand_churn] <- FALSE
        }

        # 2. Conditional Churn
        if (any(active_users)) {
          start_idx <- max(1, t - m + 1)
          window_fails <- rowSums(req_failures[, start_idx:t, drop = FALSE])

          is_cond_churn <- active_users & (window_fails >= k)

          if (any(is_cond_churn)) {
            churn_step[is_cond_churn] <- t
            churn_reason[is_cond_churn] <- "Conditionat (p, k, m)"
            active_users[is_cond_churn] <- FALSE
          }
        }
      }

      list(
        churn_step = churn_step,
        churn_reason = churn_reason,
        N = n_total, H = h_steps, q = q, p = p, m = m, k = k
      )
    })

    # Plot 1: Survival
    output$survivalPlot <- renderPlot({
      res <- sim_data()
      times <- 1:res$H
      active_count <- sapply(times, function(t) sum(res$churn_step > t))
      survival_prob <- active_count / res$N

      df_surv <- data.frame(Time = times, Survival = survival_prob)

      ggplot(df_surv, aes(x = .data$Time, y = .data$Survival)) +
        geom_line(color = "#2c3e50", size = 1.2) +
        geom_area(fill = "#3498db", alpha = 0.3) +
        labs(
          title = "Curba de Supravietuire (Retentie)",
          subtitle = paste("Cati useri raman activi in timp? (N =", res$N, ")"),
          x = "Timp (Request-uri)", y = "Proportie Activi"
        ) +
        theme_minimal(base_size = 14) +
        ylim(0, 1)
    })

    # Plot 2: Breakdown
    output$reasonPlot <- renderPlot({
      res <- sim_data()
      df <- data.frame(Reason = res$churn_reason)

      # Ensure levels order
      df$Reason <- factor(df$Reason,
        levels = c("Survived", "Aleator (q)", "Conditionat (p, k, m)")
      )

      # Pre-calculate counts to avoid after_stat(count) linter warning
      counts <- as.data.frame(table(Reason = df$Reason))

      ggplot(counts, aes(
        x = .data$Reason, y = .data$Freq, fill = .data$Reason
      )) +
        geom_col() +
        geom_text(
          aes(label = .data$Freq),
          vjust = -0.5
        ) +
        scale_fill_manual(values = c(
          "Survived" = "#27ae60", "Aleator (q)" = "#e67e22",
          "Conditionat (p, k, m)" = "#c0392b"
        )) +
        labs(
          title = "Cauzele Pierderii Utilizatorilor",
          subtitle = "Comparatie intre cele doua mecanisme",
          x = "Motiv", y = "Numar Utilizatori"
        ) +
        theme_minimal()
    })

    # Stats Table
    output$statsTable <- renderTable(
      {
        res <- sim_data()
        total <- res$N
        surv <- sum(res$churn_reason == "Survived")
        rand <- sum(res$churn_reason == "Aleator (q)")
        cond <- sum(res$churn_reason == "Conditionat (p, k, m)")

        churn_total <- total - surv
        churn_rate <- churn_total / total

        data.frame(
          Indicator = c(
            "Total Utilizatori (N)", "Supravietuitori", "Pierduti Aleator (q)",
            "Pierduti Conditionat (fail)", "Probabilitate Totala Churn"
          ),
          Valoare = c(
            as.integer(total), as.integer(surv), as.integer(rand),
            as.integer(cond), sprintf("%.2f%%", churn_rate * 100)
          )
        )
      },
      digits = 0
    )

    # Interpretation
    output$interpretation <- renderUI({
      res <- sim_data()
      churn_rate <- (res$N - sum(res$churn_reason == "Survived")) / res$N

      HTML(paste0(
        "<h3>c) Interpretare si Comparatie</h3>",
        "<ul>",
        "<li><b>Probabilitatea de Churn Estimat:</b> ",
        sprintf("%.1f%%", churn_rate * 100), ". ",
        "Asta inseamna ca sistemul pierde ",
        sprintf("%.1f%%", churn_rate * 100), " din useri in ", res$H,
        " pasi.</li>",
        "<li><b>Comparatie Scenarii:</b>",
        "<ul>",
        "<li><i>Aleator (q=", res$q,
        "):</i> Reprezinta zgomotul de fond sau competitia externa. ",
        "Afecteaza constant userii.</li>",
        "<li><i>Conditionat (p=", res$p,
        "):</i> Reprezinta calitatea tehnica a serviciului. ",
        "Daca serverul da erori dese (p mare), userii pleaca rapid ",
        "(conditionat).</li>",
        "</ul></li>",
        "<li><b>Impact:</b> Observam din graficul de bare care mecanism ",
        "domina. Daca domina cel conditionat, trebuie imbunatatit ",
        "infrastructura (scazut p). ",
        "Daca domina cel aleator, trebuie imbunatatit ",
        "produsul/marketingul.</li>"
      ))
    })
  })
}
