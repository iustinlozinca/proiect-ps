# Module Exercise 9: Aproximare Normală și Agregare

ui_ex9 <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Aproximare Normală și Agregare"),
    sidebarLayout(
      sidebarPanel(
        h4("Parametri Simulare"),
        sliderInput(ns("n_zile"), "Număr de zile simulate:",
          min = 100, max = 2000, value = 500, step = 100
        ),
        sliderInput(ns("cereri_zi"), "Cereri per zi:",
          min = 10, max = 500, value = 100, step = 10
        ),
        hr(),
        h4("Distribuția Latențelor Individuale"),
        selectInput(ns("dist_type"), "Tipul distribuției:",
          choices = c("Exponențială" = "exp", "Gamma" = "gamma", "Uniformă" = "unif")
        ),
        sliderInput(ns("media_latenta"), "Media latenței (ms):",
          min = 50, max = 500, value = 200, step = 10
        ),
        hr(),
        helpText(
          "Teorema Limită Centrală (TLC):", br(),
          "Suma a n v.a. i.i.d. tinde către", br(),
          "o distribuție normală când n → ∞"
        )
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("9.a) Agregare Zilnică",
            h4("Distribuția Latenței Totale pe Zi"),
            plotOutput(ns("plotAgregat")),
            hr(),
            tableOutput(ns("tabStatistici")),
            verbatimTextOutput(ns("explainAgregat"))
          ),
          tabPanel("9.b) Comparație cu Normala",
            h4("Histogramă vs Densitate Normală"),
            plotOutput(ns("plotComparatie")),
            hr(),
            h4("Q-Q Plot (Cuantile-Cuantile)"),
            plotOutput(ns("plotQQ")),
            hr(),
            h4("Test de Normalitate"),
            verbatimTextOutput(ns("testNormalitate")),
            hr(),
            uiOutput(ns("interpretare"))
          )
        )
      )
    )
  )
}

server_ex9 <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Simulare agregări zilnice
    sim_data <- reactive({
      n_zile <- input$n_zile
      cereri <- input$cereri_zi
      media <- input$media_latenta
      dist <- input$dist_type

      # Generăm latența totală pentru fiecare zi
      latenta_zi <- numeric(n_zile)

      for (i in 1:n_zile) {
        # Generăm 'cereri' latențe individuale
        if (dist == "exp") {
          latente <- rexp(cereri, rate = 1 / media)
        } else if (dist == "gamma") {
          # Gamma cu shape=2, scale calculat pentru media dorită
          latente <- rgamma(cereri, shape = 2, scale = media / 2)
        } else {
          # Uniformă între 0 și 2*media (pentru a avea media corectă)
          latente <- runif(cereri, min = 0, max = 2 * media)
        }
        latenta_zi[i] <- sum(latente)
      }

      data.frame(
        Zi = 1:n_zile,
        LatentaTotala = latenta_zi
      )
    })

    # 9.a) Histogramă agregat
    output$plotAgregat <- renderPlot({
      df <- sim_data()

      ggplot(df, aes(x = LatentaTotala)) +
        geom_histogram(aes(y = after_stat(density)), bins = 30,
                       fill = "steelblue", color = "white", alpha = 0.7) +
        geom_density(color = "darkblue", linewidth = 1) +
        labs(
          title = paste("Distribuția Latenței Totale pe Zi (n =", input$cereri_zi, "cereri/zi)"),
          x = "Latență Totală (ms)", y = "Densitate"
        ) +
        theme_minimal()
    })

    # Tabel statistici
    output$tabStatistici <- renderTable({
      df <- sim_data()
      x <- df$LatentaTotala

      # Valori teoretice pentru suma
      media_ind <- input$media_latenta
      n <- input$cereri_zi

      if (input$dist_type == "exp") {
        var_ind <- media_ind^2
      } else if (input$dist_type == "gamma") {
        var_ind <- 2 * (media_ind / 2)^2
      } else {
        var_ind <- (2 * media_ind)^2 / 12
      }

      data.frame(
        Măsură = c("Media", "Deviația Standard", "Asimetria (Skewness)", "Curtoza (Kurtosis)"),
        Empirică = c(mean(x), sd(x), 
                     sum((x - mean(x))^3) / (length(x) * sd(x)^3),
                     sum((x - mean(x))^4) / (length(x) * sd(x)^4) - 3),
        Teoretică_Normal = c(n * media_ind, sqrt(n * var_ind), 0, 0)
      )
    }, digits = 2)

    output$explainAgregat <- renderText({
      paste0(
        "Latența totală zilnică = suma a ", input$cereri_zi, " latențe individuale.\n",
        "Conform TLC, această sumă tinde către o distribuție normală.\n",
        "Media sumei = n × μ, Varianța sumei = n × σ²"
      )
    })

    # 9.b) Comparație cu normala
    output$plotComparatie <- renderPlot({
      df <- sim_data()
      x <- df$LatentaTotala
      m <- mean(x)
      s <- sd(x)

      ggplot(df, aes(x = LatentaTotala)) +
        geom_histogram(aes(y = after_stat(density)), bins = 30,
                       fill = "lightgreen", color = "white", alpha = 0.7) +
        stat_function(fun = dnorm, args = list(mean = m, sd = s),
                      color = "red", linewidth = 1.2) +
        labs(
          title = "Histogramă vs Distribuție Normală Ajustată",
          subtitle = paste0("N(μ = ", round(m, 0), ", σ = ", round(s, 0), ")"),
          x = "Latență Totală (ms)", y = "Densitate"
        ) +
        theme_minimal()
    })

    # Q-Q Plot
    output$plotQQ <- renderPlot({
      df <- sim_data()
      x <- df$LatentaTotala

      ggplot(data.frame(x = x), aes(sample = x)) +
        stat_qq(color = "steelblue", size = 2, alpha = 0.6) +
        stat_qq_line(color = "red", linewidth = 1) +
        labs(
          title = "Q-Q Plot: Date vs Distribuție Normală",
          x = "Cuantile Teoretice (Normală)", y = "Cuantile Empirice"
        ) +
        theme_minimal()
    })

    # Test Shapiro-Wilk
    output$testNormalitate <- renderText({
      df <- sim_data()
      x <- df$LatentaTotala

      # Shapiro-Wilk (max 5000 observații)
      x_test <- if (length(x) > 5000) sample(x, 5000) else x
      test <- shapiro.test(x_test)

      paste0(
        "Test Shapiro-Wilk pentru normalitate:\n",
        "W = ", round(test$statistic, 4), "\n",
        "p-value = ", format(test$p.value, digits = 4), "\n\n",
        "Interpretare:\n",
        "H0: Datele provin dintr-o distribuție normală\n",
        "Dacă p-value > 0.05, nu respingem H0 (aproximarea e adecvată)\n",
        "Dacă p-value < 0.05, respingem H0 (aproximarea e mai puțin bună)"
      )
    })

    # Interpretare
    output$interpretare <- renderUI({
      df <- sim_data()
      x <- df$LatentaTotala
      n <- input$cereri_zi

      x_test <- if (length(x) > 5000) sample(x, 5000) else x
      test <- shapiro.test(x_test)
      p_val <- test$p.value

      # Skewness
      skew <- sum((x - mean(x))^3) / (length(x) * sd(x)^3)

      HTML(paste0(
        "<h4>Evaluarea Aproximării Normale</h4>",
        "<p><b>Număr de termeni în sumă:</b> n = ", n, "</p>",
        "<p><b>Distribuția originală:</b> ", input$dist_type, "</p>",
        "<p><b>Asimetria agregatului:</b> ", round(skew, 3), 
        " (normala are skewness = 0)</p>",
        "<p><b>p-value Shapiro-Wilk:</b> ", format(p_val, digits = 4), "</p>",
        "<hr>",
        "<p><b>Concluzie:</b> ",
        if (p_val > 0.05) {
          paste0("Aproximarea normală este <b>adecvată</b> pentru n = ", n, " termeni.")
        } else if (p_val > 0.01) {
          paste0("Aproximarea normală este <b>acceptabilă</b>, dar nu perfectă.")
        } else {
          paste0("Aproximarea normală este <b>slabă</b>. ",
                 "Creșteți numărul de cereri/zi pentru o aproximare mai bună.")
        },
        "</p>",
        "<p><i>Conform TLC, aproximarea se îmbunătățește odată cu creșterea lui n.</i></p>"
      ))
    })
  })
}
