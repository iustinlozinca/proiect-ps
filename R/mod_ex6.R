library(shiny)
library(ggplot2)

ui_ex6 <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Probabilitati Conditionate si Conditionari"),
    sidebarLayout(
      sidebarPanel(
        h4("Parametri Simulare"),
        sliderInput(ns("n_sim"), "Numar de cereri simulate:",
          min = 1000, max = 20000, value = 10000, step = 1000
        ),
        sliderInput(ns("p_succes"), "Probabilitate succes (p):",
          min = 0.1, max = 0.95, value = 0.7, step = 0.05
        ),
        sliderInput(ns("max_retry"), "Nr. Maxim retry (N_max):",
          min = 2, max = 10, value = 5
        ),
        hr(),
        h4("Praguri Evenimente"),
        sliderInput(ns("t0_SLA"), "Prag Timp SLA (t0):",
          min = 0.5, max = 10.0, value = 2.0, step = 0.5
        ),
        sliderInput(ns("n0_threshold"), "Prag incercari (n0):",
          min = 1, max = 5, value = 2
        ),
        hr(),
        helpText(
          "A = {I = 1} - Succes final", br(),
          "B = {T ≤ t0} - SLA respectat", br(),
          "C = {N ≤ n0} - Putine incercari"
        )
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Probabilitati Conditionate (a)",
            h4("Estimari Empirice"),
            tableOutput(ns("tabCondProb")),
            hr(),
            fluidRow(
              column(6, plotOutput(ns("plotCondA"))),
              column(6, plotOutput(ns("plotCondB")))
            ),
            verbatimTextOutput(ns("explainCondProb"))
          ),
          tabPanel(
            "Sperante Conditionate (b)",
            h4("E(T | I) - Speranta Timpului Conditionata"),
            tableOutput(ns("tabCondExp")),
            hr(),
            plotOutput(ns("plotCondExp")),
            hr(),
            plotOutput(ns("boxplotCondExp")),
            verbatimTextOutput(ns("explainCondExp"))
          ),
          tabPanel(
            "Interpretare (c)",
            wellPanel(
              h3("Interpretarea Rezultatelor"),
              uiOutput(ns("interpretareUX"))
            )
          )
        )
      )
    )
  )
}

server_ex6 <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Simulare Monte Carlo
    sim_data <- reactive({
      n <- input$n_sim
      max_ret <- input$max_retry
      p <- input$p_succes

      t_total <- numeric(n)
      i_final <- numeric(n)
      n_total <- numeric(n)

      for (i in 1:n) {
        att <- 0
        timp <- 0
        succes <- FALSE

        while (att < max_ret && !succes) {
          att <- att + 1
          timp <- timp + rexp(1, rate = 2)
          if (runif(1) < p) {
            succes <- TRUE
          } else {
            timp <- timp + 0.2
          }
        }
        t_total[i] <- timp
        i_final[i] <- as.numeric(succes)
        n_total[i] <- att
      }
      data.frame(Timp = t_total, I = i_final, N = n_total)
    })

    # 6.a) Tabel probabilitati conditionate
    output$tabCondProb <- renderTable(
      {
        df <- sim_data()
        t0 <- input$t0_SLA
        n0 <- input$n0_threshold

        ev_a <- df$I == 1
        ev_b <- df$Timp <= t0
        ev_c <- df$N <= n0

        p_a_given_c <- mean(df$I[ev_c] == 1)
        p_b_given_a <- mean(df$Timp[ev_a] <= t0)

        data.frame(
          Probabilitate = c(
            "P(A)", "P(B)", "P(C)", "P(A | N ≤ n0)", "P(B | A)"
          ),
          Valoare = c(
            mean(ev_a), mean(ev_b), mean(ev_c), p_a_given_c, p_b_given_a
          ),
          Descriere = c(
            "Succes final", "SLA respectat", "Putine incercari",
            "Succes dat fiind N ≤ n0", "SLA dat fiind succes"
          )
        )
      },
      digits = 4
    )

    # Grafic rata succes per N
    output$plotCondA <- renderPlot({
      df <- sim_data()
      n0 <- input$n0_threshold
      success_by_n <- aggregate(I ~ N, data = df, FUN = mean)

      ggplot(success_by_n, aes(x = factor(.data$N), y = .data$I)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        geom_hline(
          yintercept = mean(df$I[df$N <= n0]), color = "red",
          linetype = "dashed", linewidth = 1
        ) +
        geom_vline(xintercept = n0 + 0.5, color = "orange", linewidth = 1.2) +
        labs(
          title = "Rata de Succes per Numar de incercari",
          x = "N (incercari)", y = "P(A | N)"
        ) +
        ylim(0, 1) +
        theme_minimal()
    })

    # Grafic distributie T | succes
    output$plotCondB <- renderPlot({
      df <- sim_data()
      t0 <- input$t0_SLA
      df_s <- df[df$I == 1, ]

      ggplot(df_s, aes(x = .data$Timp)) +
        geom_histogram(
          aes(y = after_stat(density)),
          bins = 30, fill = "lightgreen",
          color = "white"
        ) +
        geom_vline(
          xintercept = t0, color = "red", linewidth = 1.2, linetype = "dashed"
        ) +
        labs(
          title = "Distributia T | Succes", x = "Timp (s)", y = "Densitate"
        ) +
        theme_minimal()
    })

    output$explainCondProb <- renderText({
      df <- sim_data()
      n0 <- input$n0_threshold
      t0 <- input$t0_SLA

      p_a <- mean(df$I == 1)
      p_a_c <- mean(df$I[df$N <= n0] == 1)
      p_b_a <- mean(df$Timp[df$I == 1] <= t0)

      paste0(
        "P(A | N ≤ ", n0, ") = ", round(p_a_c, 4),
        " vs P(A) = ", round(p_a, 4), "\n",
        "P(B | A) = ", round(p_b_a, 4),
        " (fractiunea cererilor reusite care respecta SLA)"
      )
    })

    # 6.b) Tabel sperante conditionate
    output$tabCondExp <- renderTable(
      {
        df <- sim_data()
        df_s <- df[df$I == 1, ]
        df_f <- df[df$I == 0, ]

        data.frame(
          Conditie = c("I = 1 (Succes)", "I = 0 (Esec)", "Total"),
          Nr_Obs = c(nrow(df_s), nrow(df_f), nrow(df)),
          `E(T)` = c(
            mean(df_s$Timp), if (nrow(df_f) > 0) mean(df_f$Timp) else NA,
            mean(df$Timp)
          ),
          Mediana = c(
            median(df_s$Timp), if (nrow(df_f) > 0) median(df_f$Timp) else NA,
            median(df$Timp)
          ),
          Var = c(
            var(df_s$Timp), if (nrow(df_f) > 1) var(df_f$Timp) else NA,
            var(df$Timp)
          )
        )
      },
      digits = 4
    )

    # Histograma suprapusa
    output$plotCondExp <- renderPlot({
      df <- sim_data()
      df$Status <- ifelse(df$I == 1, "Succes", "Esec")

      ggplot(df, aes(x = .data$Timp, fill = .data$Status)) +
        geom_histogram(
          aes(y = after_stat(density)),
          bins = 40, alpha = 0.6,
          position = "identity"
        ) +
        scale_fill_manual(
          values = c("Succes" = "#2ecc71", "Esec" = "#e74c3c")
        ) +
        labs(
          title = "Distributia T Conditionata de I", x = "Timp (s)",
          y = "Densitate"
        ) +
        theme_minimal()
    })

    # Boxplot
    output$boxplotCondExp <- renderPlot({
      df <- sim_data()
      df$Status <- ifelse(df$I == 1, "Succes", "Esec")

      ggplot(df, aes(x = .data$Status, y = .data$Timp, fill = .data$Status)) +
        geom_boxplot(alpha = 0.7) +
        scale_fill_manual(
          values = c("Succes" = "#2ecc71", "Esec" = "#e74c3c")
        ) +
        stat_summary(fun = mean, geom = "point", shape = 18, size = 3) +
        labs(title = "Boxplot: T | I", x = "", y = "Timp (s)") +
        theme_minimal() +
        theme(legend.position = "none")
    })

    output$explainCondExp <- renderText({
      df <- sim_data()
      e_s <- mean(df$Timp[df$I == 1])
      n_failures <- sum(df$I == 0)
      e_f <- if (n_failures > 0) mean(df$Timp[df$I == 0]) else NA

      if (!is.na(e_f)) {
        paste0(
          "E(T | I=1) = ", round(e_s, 4), " secunde\n",
          "E(T | I=0) = ", round(e_f, 4), " secunde\n",
          "Diferenta: ", round(e_f - e_s, 4), " secunde"
        )
      } else {
        p_fail <- (1 - input$p_succes)^input$max_retry
        paste0(
          "E(T | I=1) = ", round(e_s, 4), " secunde\n",
          "E(T | I=0) = N/A (nu exista esecuri in esantion)\n\n",
          "Explicatie: P(esec total) = (1-", input$p_succes, ")^",
          input$max_retry, " = ",
          format(p_fail, scientific = FALSE, digits = 6), "\n",
          "Cu ", input$n_sim, " simulari, nr. asteptat de esecuri ≈ ",
          round(p_fail * input$n_sim, 2)
        )
      }
    })

    # 6.c) Interpretare obiectiva
    output$interpretareUX <- renderUI({
      df <- sim_data()
      t0 <- input$t0_SLA
      n0 <- input$n0_threshold
      max_r <- input$max_retry

      p_a <- mean(df$I == 1)
      p_a_c <- mean(df$I[df$N <= n0] == 1)
      p_b_a <- mean(df$Timp[df$I == 1] <= t0)
      e_s <- mean(df$Timp[df$I == 1])
      e_f <- if (sum(df$I == 0) > 0) mean(df$Timp[df$I == 0]) else NA

      HTML(paste0(
        "<h4>1. Relatia dintre numarul de incercari si succes</h4>",
        "<p>P(A | N ≤ ", n0, ") = <b>", round(p_a_c, 3), "</b>, ",
        "in timp ce P(A) = <b>", round(p_a, 3), "</b>.</p>",
        "<p>", if (p_a_c > p_a) {
          paste0(
            "Cererile cu putine incercari au o rata de succes mai mare decat ",
            "media."
          )
        } else {
          "Nu exista o diferenta semnificativa intre cele doua probabilitati."
        }, "</p>",
        "<hr><h4>2. Calitatea serviciului pentru cereri reusite</h4>",
        "<p>P(B | A) = <b>", round(p_b_a, 3), "</b></p>",
        "<p>Din cererile care au reusit, ", round(p_b_a * 100, 1),
        "% au respectat pragul SLA de ", t0, " secunde.</p>",
        "<hr><h4>3. Timpul de asteptare: succes vs esec</h4>",
        "<p>E(T | I=1) = <b>", round(e_s, 2), "</b> secunde</p>",
        if (!is.na(e_f)) {
          paste0(
            "<p>E(T | I=0) = <b>", round(e_f, 2), "</b> secunde</p>",
            "<p>Cererile esuate au un timp mediu de asteptare cu <b>",
            round(e_f - e_s, 2), " secunde</b> mai mare. ",
            "Aceasta se datoreaza faptului ca parcurg toate cele ", max_r,
            " incercari inainte de abandon.</p>"
          )
        } else {
          p_fail_all <- (1 - input$p_succes)^max_r
          paste0(
            "<p>E(T | I=0) = <b>N/A</b></p>",
            "<p><i>Nu exista cereri esuate in acest esantion. ",
            "Cu p = ", input$p_succes, " si N_max = ", max_r, ", ",
            "probabilitatea de a esua toate incercarile este ",
            "(1-", input$p_succes, ")<sup>", max_r, "</sup> = ",
            format(p_fail_all, scientific = FALSE, digits = 4),
            " (≈ ", round(p_fail_all * 100, 4), "%). ",
            "Pentru ", input$n_sim,
            " simulari, numarul asteptat de esecuri este ~",
            round(p_fail_all * input$n_sim, 2), ".</i></p>"
          )
        }
      ))
    })
  })
}
