library(shiny)
library(ggplot2)

ui_ex7 <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Independență vs Dependență"),
    sidebarLayout(
      sidebarPanel(
        h4("Parametri Simulare"),
        sliderInput(ns("n_sim"), "Număr de simulări:", 1000, 10000, 2000,
          step = 500
        ),
        sliderInput(
          ns("p_succes"), "Probabilitate succes (per încercare):",
          0.1, 0.9, 0.6
        ),
        sliderInput(
          ns("max_retry"), "Număr maxim încercări (N_max):",
          2, 10, 5
        ),
        hr(),
        h4("Parametri Latență (Exponențială)"),
        sliderInput(ns("base_lambda"), "Lambda de bază (rate):", 0.5, 5.0, 2.0),
        helpText("Lambda = 2 => Medie = 0.5 secunde."),
        hr(),
        h4("Factor Dependență (Scenario B)"),
        sliderInput(
          ns("degrad_factor"), "Factor degradare (k):",
          0.1, 0.9, 0.7
        ),
        helpText(
          "În scenariul Dependent, după fiecare eșec, rata scade:",
          "lambda_nou = lambda * k.",
          "O rată mai mică înseamnă un timp mediu mai MARE (congestie/backoff)."
        )
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Comparație Distribuții (a)",
            plotOutput(ns("plotComp")),
            helpText(
              "Histograme suprapuse pentru Timpul Total (T).",
              "Verde = Independent (Stabil).",
              "Roșu = Dependent (Riscuri/Congestie)."
            )
          ),
          tabPanel(
            "Varianță și Statistici (b)",
            h4("Tabel Comparativ (Medie și Varianță)"),
            tableOutput(ns("tabComp")),
            hr(),
            h4("Analiza Volatilității"),
            uiOutput(ns("analizaVar"))
          ),
          tabPanel(
            "Concluzii (c)",
            uiOutput(ns("concluziiText"))
          )
        )
      )
    )
  )
}

server_ex7 <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Simulare
    sim_data <- reactive({
      n <- input$n_sim
      max_ret <- input$max_retry
      p <- input$p_succes
      lambda_0 <- input$base_lambda
      k <- input$degrad_factor

      t_indep <- numeric(n) # Scenario A
      t_dep <- numeric(n) # Scenario B

      # Simulare Scenario A (Independent)
      # Simulare Scenario B (Dependent)

      for (i in 1:n) {
        # --- SCENARIO A: INDEPENDENT ---
        attempts_a <- 0
        timp_a <- 0
        succes_a <- FALSE
        while (attempts_a < max_ret && !succes_a) {
          attempts_a <- attempts_a + 1
          # Lambda constant
          timp_a <- timp_a + rexp(1, rate = lambda_0)

          if (runif(1) < p) succes_a <- TRUE
        }
        t_indep[i] <- timp_a

        # --- SCENARIO B: DEPENDENT ---
        attempts_b <- 0
        timp_b <- 0
        succes_b <- FALSE
        current_lambda <- lambda_0

        while (attempts_b < max_ret && !succes_b) {
          attempts_b <- attempts_b + 1
          # Lambda se degradeaza doar DUPA esec,
          # Presupunem ca incercarea 1 e standard. Incercarea 2 e mai lenta.

          timp_b <- timp_b + rexp(1, rate = current_lambda)

          if (runif(1) < p) {
            succes_b <- TRUE
          } else {
            # Esec -> Degradam performanta pentru urmatoarea
            current_lambda <- current_lambda * k
          }
        }
        t_dep[i] <- timp_b
      }

      data.frame(
        Tip = c(rep("Independent", n), rep("Dependent", n)),
        Timp = c(t_indep, t_dep)
      )
    })

    # Grafic Comparativ
    output$plotComp <- renderPlot({
      df <- sim_data()

      # Calculam mediile pentru linii verticale
      mu_indep <- mean(df$Timp[df$Tip == "Independent"])
      mu_dep <- mean(df$Timp[df$Tip == "Dependent"])

      ggplot(df, aes(x = .data$Timp, fill = .data$Tip)) +
        geom_density(alpha = 0.5) +
        geom_vline(
          xintercept = mu_indep, color = "darkgreen", linetype = "dashed",
          size = 1
        ) +
        geom_vline(
          xintercept = mu_dep, color = "darkred", linetype = "dashed", size = 1
        ) +
        scale_fill_manual(
          values = c("Dependent" = "red", "Independent" = "green")
        ) +
        labs(
          title = "Distribuția Timpului Total: Independent vs Dependent",
          subtitle = "Liniile punctate indică media.",
          x = "Timp Total (T)",
          y = "Densitate"
        ) +
        theme_minimal()
    })

    # Tabel Statistici
    output$tabComp <- renderTable(
      {
        df <- sim_data()

        aggregate(Timp ~ Tip, data = df, FUN = function(x) {
          c(Media = mean(x), Varianta = var(x), Max = max(x))
        }) |>
          # Flatten matrix column from aggregate
          do.call(data.frame, args = _) |>
          setNames(c(
            "Scenario", "Media (E[T])", "Varianta (Var(T))",
            "Maxim (Riscul Extrem)"
          ))
      },
      digits = 4
    )

    output$analizaVar <- renderUI({
      df <- sim_data()
      var_indep <- var(df$Timp[df$Tip == "Independent"])
      var_dep <- var(df$Timp[df$Tip == "Dependent"])
      ratio <- var_dep / var_indep

      HTML(paste0(
        "<p>Varianța în scenariul Dependent este de <b>", round(ratio, 2),
        " ori</b> mai mare.</p>",
        "<p>Aceasta indică o <b>instabilitate mult mai mare</b>. Utilizatorii",
        "'ghinioniști' care eșuează ",
        "de câteva ori intră într-o spirală a întârzierilor (coada lungă ",
        "a distribuției).</p>"
      ))
    })

    # Concluzii
    output$concluziiText <- renderUI({
      HTML(paste0(
        "<h3>Concluzii (c)</h3>",
        "<ul>",
        "<li><b>Risc:</b> Scenariul dependent introduce un ",
        "<i>risc sistemic</i>.",
        "Câțiva utilizatori pot experimenta timpi extrem de lungi (outliers), ",
        "ceea ce nu se întâmplă în scenariul independent.</li>",
        "<li><b>Stabilitate:</b> Sistemul independent este mai <i>stabil</i> ",
        "și <i>predictibil</i>. ",
        "Dependența de eșecuri anterioare crește ",
        "incertitudinea (varianța).</li>",
        "<li><b>Design:</b> În practică, trebuie să evităm situațiile unde ",
        "'eșecul atrage eșec' ",
        "(ex: retry storms care blochează serverul și mai tare).</li>",
        "</ul>"
      ))
    })
  })
}
