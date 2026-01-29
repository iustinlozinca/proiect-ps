library(shiny)
library(ggplot2)

ui_ex5 <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Variabila Bidimensionala (N, T)"),
    sidebarLayout(
      sidebarPanel(
        h4("Parametri Simulare"),
        sliderInput(
          ns("n_sim"), "Numar de simulari:", 1000, 10000, 5000,
          step = 500
        ),
        sliderInput(
          ns("p_succes"), "Probabilitate succes (p):", 0.1, 0.9, 0.5,
          step = 0.1
        ),
        sliderInput(
          ns("max_retry"), "Numar maxim incercari (N_max):",
          2, 10, 5
        ),
        hr(),
        sliderInput(
          ns("penalty"), "Penalizare Retry (secunde):", 0, 1.0, 0.2,
          step = 0.1
        ),
        helpText("Se adauga la timpul total (T) dupa fiecare esec."),
        hr(),
        h4("Explicatie"),
        helpText(
          "N = Variabila discreta (Nr. incercari).",
          "T = Variabila continua (Timp total)."
        )
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Vizualizare (a)",
            plotOutput(ns("plotScat")),
            plotOutput(ns("plotBox")),
            helpText(
              "Observam relatia dintre N (discret) si T (continuu)."
            )
          ),
          tabPanel(
            "Statistici & Corelatie (b, c)",
            h4("Statistici Descriptive"),
            tableOutput(ns("tabStats")),
            hr(),
            h4("Matricea de Covarianta si Corelatie"),
            verbatimTextOutput(ns("matCovCor")),
            hr(),
            uiOutput(ns("interpretare"))
          )
        )
      )
    )
  )
}

server_ex5 <- function(id) {
  moduleServer(id, function(input, output, session) {
    # 1. Simulare (N, T)
    sim_data <- reactive({
      n <- input$n_sim
      max_ret <- input$max_retry
      p <- input$p_succes
      pen <- input$penalty

      n_vec <- numeric(n)
      t_vec <- numeric(n)

      for (i in 1:n) {
        attempts <- 0
        timp <- 0
        succes <- FALSE

        while (attempts < max_ret && !succes) {
          attempts <- attempts + 1
          # Timp raspuns: Exponential (rate = 2 => media 0.5s)
          t_req <- rexp(1, rate = 2)
          timp <- timp + t_req

          if (runif(1) < p) {
            succes <- TRUE
          } else {
            # Penalizare backoff
            timp <- timp + pen
          }
        }
        n_vec[i] <- attempts
        t_vec[i] <- timp
      }

      data.frame(Nr_incercari = n_vec, Timp = t_vec)
    })

    # 2. Vizualizare
    # Scatterplot cu Jitter (ca sa vedem punctele suprapuse la N)
    output$plotScat <- renderPlot({
      df <- sim_data()
      ggplot(df, aes(x = .data$Nr_incercari, y = .data$Timp)) +
        geom_jitter(width = 0.2, alpha = 0.3, color = "darkblue") +
        geom_smooth(method = "lm", color = "red", se = FALSE) +
        labs(
          title = "Scatterplot (N, T) cu Jitter + Regresie Liniara",
          subtitle = paste(
            "Arata tendinta generala de crestere a timpului odata cu N"
          ),
          x = "N (Numar incercari)", y = "Timp (secunde)"
        ) +
        theme_minimal()
    })

    # Boxplot (Analiza distributiei T pentru fiecare N)
    output$plotBox <- renderPlot({
      df <- sim_data()
      ggplot(df, aes(
        x = factor(.data$Nr_incercari), y = .data$Timp,
        fill = factor(.data$Nr_incercari)
      )) +
        geom_boxplot(alpha = 0.7, show.legend = FALSE) +
        labs(
          title = "Boxplot: Distributia lui T conditionata de N",
          subtitle = "Cu cat avem mai multe incercari (N), cu atat T creste",
          x = "N (Numar incercari)", y = "Timp Total"
        ) +
        theme_minimal() +
        scale_fill_brewer(palette = "Blues")
    })

    # 3. Statistici
    output$tabStats <- renderTable(
      {
        df <- sim_data()

        data.frame(
          Variabila = c("N (Nr. incercari)", "Timp Total"),
          Media_E = c(mean(df$Nr_incercari), mean(df$Timp)),
          Varianta_Var = c(var(df$Nr_incercari), var(df$Timp)),
          Min = c(min(df$Nr_incercari), min(df$Timp)),
          Max = c(max(df$Nr_incercari), max(df$Timp))
        )
      },
      digits = 4
    )

    output$matCovCor <- renderPrint({
      df <- sim_data()
      cat("Covarianta Cov(N, T):\n")
      print(cov(df$Nr_incercari, df$Timp))
      cat("\nCoeficientul de Corelatie Cor(N, T):\n")
      print(cor(df$Nr_incercari, df$Timp))
    })

    # 4. Interpretare
    output$interpretare <- renderUI({
      df <- sim_data()
      val_cor <- cor(df$Nr_incercari, df$Timp)

      grad <- if (val_cor > 0.7) {
        "puternica"
      } else if (val_cor > 0.3) {
        "moderata"
      } else {
        "slaba"
      }

      HTML(paste0(
        "<h3>c) Interpretare Corelatie</h3>",
        "<p>Coeficientul de corelatie este <b>", round(val_cor, 4), "</b>.</p>",
        "<ul>",
        "<li>Valoarea este <b>pozitiva</b>: Acest lucru confirma logic ca un",
        "numar mai mare de ",
        "incercari (N) duce la un timp total (T) mai mare (se aduna timpii de",
        "procesare + backoff).</li>",
        "<li>Corelatia este <b>", grad,
        "</b>: Desi relatia este directa, exista variabilitate ",
        "datorita naturii aleatoare a timpului de raspuns (distributie",
        "Exponentiala). ",
        "Chiar si cu N=1, timpul poate varia mult.</li>",
        "</ul>"
      ))
    })
  })
}
