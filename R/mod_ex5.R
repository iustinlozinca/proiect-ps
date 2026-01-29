library(shiny)
library(ggplot2)

ui_ex5 <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Variabila Bidimensională (N, T)"),
    sidebarLayout(
      sidebarPanel(
        h4("Parametri Simulare"),
        sliderInput(
          ns("n_sim"), "Număr de simulări:", 1000, 10000, 5000,
          step = 500
        ),
        sliderInput(
          ns("p_succes"), "Probabilitate succes (p):", 0.1, 0.9, 0.5,
          step = 0.1
        ),
        sliderInput(
          ns("max_retry"), "Număr maxim încercări (N_max):",
          2, 10, 5
        ),
        hr(),
        sliderInput(
          ns("penalty"), "Penalizare Retry (secunde):", 0, 1.0, 0.2,
          step = 0.1
        ),
        helpText("Se adaugă la timpul total (T) după fiecare eșec."),
        hr(),
        h4("Explicație"),
        helpText(
          "N = Variabilă discretă (Nr. încercări).",
          "T = Variabilă continuă (Timp total)."
        )
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Vizualizare (a)",
            plotOutput(ns("plotScat")),
            plotOutput(ns("plotBox")),
            helpText(
              "Observăm relația dintre N (discret) și T (continuu)."
            )
          ),
          tabPanel(
            "Statistici & Corelație (b, c)",
            h4("Statistici Descriptive"),
            tableOutput(ns("tabStats")),
            hr(),
            h4("Matricea de Covarianță și Corelație"),
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
          title = "Scatterplot (N, T) cu Jitter + Regresie Liniară",
          subtitle = paste(
            "Arată tendința generală de creștere a timpului odată cu N"
          ),
          x = "N (Număr Încercări)", y = "Timp (secunde)"
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
          title = "Boxplot: Distribuția lui T condiționată de N",
          subtitle = "Cu cât avem mai multe încercări (N), cu atât T crește",
          x = "N (Număr Încercări)", y = "Timp Total"
        ) +
        theme_minimal() +
        scale_fill_brewer(palette = "Blues")
    })

    # 3. Statistici
    output$tabStats <- renderTable(
      {
        df <- sim_data()

        data.frame(
          Variabila = c("N (Nr. Încercări)", "Timp Total"),
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
      cat("Covarianța Cov(N, T):\n")
      print(cov(df$Nr_incercari, df$Timp))
      cat("\nCoeficientul de Corelație Cor(N, T):\n")
      print(cor(df$Nr_incercari, df$Timp))
    })

    # 4. Interpretare
    output$interpretare <- renderUI({
      df <- sim_data()
      val_cor <- cor(df$Nr_incercari, df$Timp)

      grad <- if (val_cor > 0.7) {
        "puternică"
      } else if (val_cor > 0.3) {
        "moderată"
      } else {
        "slabă"
      }

      HTML(paste0(
        "<h3>c) Interpretare Corelație</h3>",
        "<p>Coeficientul de corelație este <b>", round(val_cor, 4), "</b>.</p>",
        "<ul>",
        "<li>Valoarea este <b>pozitivă</b>: Acest lucru confirmă logic că un",
        "număr mai mare de ",
        "încercări (N) duce la un timp total (T) mai mare (se adună timpii de",
        "procesare + backoff).</li>",
        "<li>Corelația este <b>", grad,
        "</b>: Deși relația este directă, există variabilitate ",
        "datorită naturii aleatoare a timpului de răspuns (distribuție",
        "Exponențială). ",
        "Chiar și cu N=1, timpul poate varia mult.</li>",
        "</ul>"
      ))
    })
  })
}
