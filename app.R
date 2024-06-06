library(shiny)
library(shinyWidgets)
library(ggplot2)
library(DT)
library(latex2exp)

ui <- fluidPage(
  titlePanel(h3("Distribution de Cauchy")),
  withMathJax(),
  tags$div(HTML("<script type='text/x-mathjax-config'>
                MathJax.Hub.Config({
                tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
                });
                </script>
                ")),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(
          6,
          switchInput(
            inputId = "input_type",
            label = "Entrée",
            onLabel = "Curseur",
            offLabel = "Numérique",
            value = TRUE,
            size = "mini"
          )
        ),
        column(3, offset = 3,
               actionBttn("reset_button",
                          "Réinit",
                          icon = icon("refresh"),
                          class = "btn-danger",
                          style = "bordered",
                          size = "xs",
                          color = "default")
        )
      ),
      conditionalPanel(
        condition = "input.input_type == true",
        sliderInput("median",
                    "$x_0$:",
                    min = -10,
                    max = 10,
                    step = 0.01,
                    value = 0
        ),
        sliderInput("gamma",
                    "$\\gamma$:",
                    min = 0.1,
                    max = 20,
                    step = 0.01,
                    value = 1
        )
      ),
      conditionalPanel(
        condition = "input.input_type == false",
        numericInput("median_num",
                     "$x_0$:",
                     step = 0.01,
                     value = 0,
                     width = 150
        ),
        numericInput("gamma_num",
                     "$\\gamma$:",
                     min = 0.1,
                     step = 0.01,
                     value = 1,
                     width = 150
        )
      ),
      
      switchInput(
        inputId = "input_mode",
        label = "Données",
        onLabel = "Générées",
        offLabel = "Chargées",
        value = TRUE,
        size = "mini"),
      
      conditionalPanel(
        condition = "input.input_mode == true",
        numericInput("num_observations",
                     "# Observations à générer :",
                     value = 500,
                     min = 1,
                     max = 10000,
                     width = 150,
                     step = 50),
        fluidRow(
          column(6,
                 actionBttn(inputId = "generate_button", 
                            label = "Générer", 
                            style = "unite",
                            color = "primary",
                            icon = icon("sliders"),
                            size = "xs")
          ),
          column(6,
                 downloadBttn(outputId = "download_data", 
                              label = "Télécharger", 
                              style = "unite",
                              color = "default",
                              icon = icon("download"),
                              size = "xs")
          )
        )
      ),
      conditionalPanel(
        condition = "input.input_mode == false",
        fileInput("file1", "Fichier :",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")),
        DTOutput("fittedParams")
      ),
      
      p(),
      wellPanel(style = "background: lightblue",
                fluidRow(
                  column(4,
                         a(h4("Par Daniel Coulombe, Ph.D.")),
                         p("2024")
                  ),
                  column(4,
                         tags$a(
                           href="https://isteah.org", 
                           tags$img(src="ISTEAH_LOGO.png", 
                                    title="ISTEAH", 
                                    width="160",
                                    height="140")
                         )
                  )
                  
                )
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(strong(h4("Introduction")),
             helpText(
               h4("Introduction à la Distribution de Cauchy"),
               "La distribution de Cauchy, également connue sous le nom de distribution de Lorentz, est une distribution de probabilité continue ",
               "qui est souvent utilisée en statistique et en physique. Elle est caractérisée par sa densité de probabilité en forme de cloche, ",
               "mais avec des extrémités épaisses qui s'étendent indéfiniment.", p(),
               "Cette distribution est définie par deux paramètres: le paramètre de localisation, $\\mu$, et $\\gamma$",
               "qui détermine la largeur de la distribution.", p(),
               
               strong("Fonction de densité de probabilité (PDF)"), p(),
               "La fonction de densité de probabilité de la distribution de Cauchy est définie par :",
               "$$f(x; \\mu, \\gamma) = \\frac{1}{\\pi \\gamma \\left[1+\\left(\\frac{x - \\mu}{\\gamma}\\right)^2\\right]}=\\frac{1}{\\pi}\\left[\\frac{\\gamma}{(x-\\mu)^2+\\gamma^2} \\right]$$", p(),
               "Notons que cette distribution est équivalente à une distribution de Student non standardisée, $t_{dl=1}$ avec une moyenne $\\mu$ et un écart-type $\\sigma$. ", p(),
               
               strong("Distribution de Cauchy standardisée"), p(),
               "$$f(x; 0, 1) = \\frac{1}{\\pi(1+x^2)}$$", p(),
               "Cette distribution est alors équivalente à la distribution de Student avec $dl=1$", p(),
               
               "La fonction cumulative de la distribution de Cauchy est définie par l'expression suivante:", p(),
               "$$F(x; \\mu, \\gamma) = \\frac{1}{\\pi} \\arctan\\left(\\frac{x - \\mu}{\\gamma}\\right) + \\frac{1}{2}$$", p(),
               "et la fonction quantile [ou l'inverse de la fonction cumulative] prend la forme:", p(),
               "$$Q(p; \\mu, \\gamma) = \\mu + \\gamma \\tan\\left[\\pi \\left(p - \\frac{1}{2}\\right)\\right]$$", p(),
               
               h5("Paramètres de la Distribution :"),
               p(HTML("<b>Paramètre de Localisation [μ] :</b> Le paramètre de localisation, correspondant à la médiane, spécifie le point autour duquel la courbe de la distribution est centrée.")),
               p(HTML("<b>Paramètre d'Échelle (γ) :</b> Le paramètre d'échelle spécifie la largeur de la distribution. Il est également lié à l'étalement des données. Des valeurs plus élevées de γ entraînent une distribution plus large.")),
               
               p("Les graphiques suivants illustrent différentes formes que peut emprunter une distribution de Cauchy, en fonction de ses paramètres:"),
               p(),
               fluidRow(
                 column(6, plotOutput("scalePlot")),
                 column(6, plotOutput("locationPlot"))
               ),
               p(),
               fluidRow(
                 column(6, offset = 3, plotOutput("combinedPlot"))
               ),
               
               h5("Références pour En Savoir Plus :"),
               tags$ul(
                 tags$li("Wikipedia: ", tags$a("Distribution de Cauchy", href = "https://en.wikipedia.org/wiki/Cauchy_distribution")),
                 tags$li("NIST/SEMATECH e-Handbook of Statistical Methods: ", tags$a("Cauchy Distribution", href = "https://www.itl.nist.gov/div898/handbook/eda/section3/eda3667.htm")),
                 tags$li("Documentation MathWorks: ", tags$a("Cauchy Distribution", href = "https://www.mathworks.com/help/stats/cauchy-distribution.html"))
               ),
               p("Pour plus d'informations, veuillez consulter les références ci-dessus.")
             )
          ),
        
        tabPanel(strong(h4("Simuler")),
                 fluidRow(
                   column(width = 6, 
                          plotOutput("theoretical_density")),
                   column(width = 6, 
                          plotOutput("theoretical_cdf"))
                 ),
                 fluidRow(
                   conditionalPanel(
                     condition = "input.input_mode == true",
                     column(width = 6, 
                            plotOutput("empirical_hist")
                            ),
                     column(width = 6, 
                            plotOutput("empirical_cdf")
                   )
                 ),
                 conditionalPanel(
                   condition = "input.input_mode == false",
                   column(width = 6, 
                          plotOutput("empirical_hist_sample")
                   ),
                   column(width = 6, 
                          plotOutput("empirical_cdf_sample")
                   )
                 )
               )
               ),
               
               tabPanel(strong(h4("Calcul")),
                           fluidRow(
                             column(
                               width = 6,
                               uiOutput("pdfEquation"),
                               plotOutput("probabilityPlot") 
                               ),
                             column(
                               width = 6,
                               uiOutput("cdfEquation"),
                               plotOutput("quantilePlot") 
                               )
                             ),
                           p(),
                           p(),
                           fluidRow(
                             column(
                               width = 4,
                               numericInput("x_value", 
                                            "Valeur de X:", 
                                            value = 0, 
                                            min = 0, 
                                            step = 0.1,
                                            width = "100px")
                               ),
                             column(
                               width = 4, offset = 3,
                               numericInput("prob_value", 
                               "Rang Centile:", 
                               value = 0.5, 
                               min = 0, 
                               max = 1, 
                               step = 0.01,
                               width = "100px")
                               )
                             ),
                           fluidRow(
                             column(
                               width = 4,
                               textOutput("computed_prob")
                               ),
                             column(
                               width = 4, offset = 3,
                               textOutput("computed_quantile")
                               )
                             )
                           ),
        
        navbarMenu(strong(h4("Exercices")),
                   tabPanel("Exercise 1A",

         fluidRow(
           column(width = 4,
                  h3("Problème"),
                  h4("1A. Durée de vie des batteries"),
                  p("Un expérimentateur souhaite étudier la durée de vie des batteries d'un certain modèle. Il choisit au hasard 50 batteries et enregistre leur durée de vie en heures. Sachant que la durée de vie des batteries suit une distribution de Cauchy avec une médiane de 200 heures et un paramètre d'échelle de 20 heures, quel est le pourcentage de batteries qui dureront plus de 220 heures ?")
                  ),
           column(width = 8,
                  h3("Solutions"),
                  h4("1. Durée de vie des batteries"),
                  strong("Numériquement"), " :", br(),
                  "Pour ce problème, $X = 220$, $\\text{median} = 200$, et $\\gamma = 20$ heures.", p(),
                  "Nous devons trouver la probabilité que la durée de vie d'une batterie dépasse 220 heures, ce qui correspond à l'intégrale de la densité de probabilité de 220 à l'infini :", p(),
                  "$$ P(X > 220) = \\int_{220}^{\\infty} f(x) \\, dx $$", p(),
                  "Calculons cette intégrale en utilisant les données du problème :", p(),
                  "$$ \\int_{220}^{\\infty} f(x) \\, dx = \\int_{220}^{\\infty} \\frac{1}{\\pi \\cdot 20 \\cdot \\left(1 + \\left(\\frac{x - 200}{20}\\right)^2\\right)} \\, dx $$", p(),
                  "À défaut d'effectuer ce calcul manuellement, utilisons R : ", p(),
                  wellPanel(
                    style = "background-color: #f5f5f5; border: 1px solid #ddd; padding: 10px;",
                    tags$pre("
# Charger la librairie permettant l'intégration
library(pracma)

# Définir la fonction de densité de probabilité
f_cauchy <- function(x, median, gamma) {
  return(1 / (pi * gamma * (1 + ((x - median) / gamma)^2)))
}

median <- 200
gamma <- 20

# Calculer P(X > 220)
result <- integrate(f_cauchy, 
                    220, Inf, 
                    median = median, 
                    gamma = gamma)

# Afficher le résultat
cat('%P(X > 220) = ', round(result$value * 100, 2), '%')")
                                    ),
                                    
           strong("Sous R"), " :", br(),
           wellPanel(
             style = "background-color: #f5f5f5; border: 1px solid #ddd; padding: 10px;",
             tags$pre("gamma <- 20
median <- 200
p_cauchy <- pcauchy(220, 
                    location = median, 
                    scale = gamma, 
                    lower.tail = FALSE)
p_percentage <- p_cauchy * 100
cat('%P(X > 220) = ', round(p_percentage, 2), '%')")
           )
         )
)
),

tabPanel("Exercise 1B",
         
         fluidRow(
           column(width = 4,
                  h3("Problème"),
                  h4("1B. Durée de vie des batteries"),
                  p("Un expérimentateur souhaite étudier la durée de vie des batteries d'un certain modèle. Il choisit au hasard 50 batteries et enregistre leur durée de vie en heures. Sachant que la durée de vie des batteries suit une distribution de Cauchy avec une médiane de 200 heures et un paramètre d'échelle de 20 heures, quel est le pourcentage de batteries qui dureront moins de 175 heures ?")
           ),
           column(width = 8,
                  h3("Solutions"),
                  h4("1b. Durée de vie des batteries"),
                  strong("Numériquement"), " :", br(),
                  "Pour ce problème, $X = 175$, $\\text{median} = 200$, et $\\gamma = 20$ heures.", p(),
                  "Nous devons trouver la probabilité que la durée de vie d'une batterie ne dépasse pas 175 heures, ce qui correspond à l'intégrale de la densité de probabilité de $-\\infty$ à 175 :", p(),
                  "$$ P(X < 175) = \\int_{-\\infty}^{175} f(x) \\, dx $$", p(),
                  "Calculons cette intégrale en utilisant les données du problème :", p(),
                  "$$ \\int_{-\\infty}^{175} f(x) \\, dx = \\int_{-\\infty}^{175} \\frac{1}{\\pi \\cdot 20 \\cdot \\left(1 + \\left(\\frac{x - 200}{20}\\right)^2\\right)} \\, dx $$", p(),
                  "À défaut d'effectuer ce calcul manuellement, utilisons R : ", p(),
                  wellPanel(
                    style = "background-color: #f5f5f5; border: 1px solid #ddd; padding: 10px;",
                    tags$pre("
# Charger la librairie permettant l'intégration
library(pracma)

# Définir la fonction de densité de probabilité
f_cauchy <- function(x, median, gamma) {
  return(1 / (pi * gamma * (1 + ((x - median) / gamma)^2)))
}

median <- 200
gamma <- 20

# Calculer P(X < 175)
result <- integrate(f_cauchy, 
                    Inf, 175, 
                    median = median, 
                    gamma = gamma)

# Afficher le résultat
cat('%P(X < 175) = ', round(result$value * 100, 2), '%')")
                  ),
                  
                  strong("Sous R"), " :", br(),
                  wellPanel(
                    style = "background-color: #f5f5f5; border: 1px solid #ddd; padding: 10px;",
                    tags$pre("gamma <- 20
median <- 200
p_cauchy <- pcauchy(175, 
                    location = median, 
                    scale = gamma, 
                    lower.tail = TRUE)
p_percentage <- p_cauchy * 100
cat('%P(X < 175) = ', round(p_percentage, 2), '%')")
                  )
           )
         )
),

tabPanel("Exercise 1C",
         
         fluidRow(
           column(width = 4,
                  h3("Problème"),
                  h4("1C. Durée de vie des batteries"),
                  p("Un expérimentateur souhaite étudier la durée de vie des batteries d'un certain modèle. Il choisit au hasard 50 batteries et enregistre leur durée de vie en heures. Sachant que la durée de vie des batteries suit une distribution de Cauchy avec une médiane de 200 heures et un paramètre d'échelle de 20 heures, quel est le pourcentage de batteries qui dureront entre 175 et 220 heures ?")
           ),
           column(width = 8,
                  h3("Solutions"),
                  h4("1C. Durée de vie des batteries"),
                  strong("Numériquement"), " :", br(),
                  "Pour ce problème, $X = {175, 220}$, $\\text{median} = 200$, et $\\gamma = 20$ heures.", p(),
                  "Nous devons trouver la probabilité que la durée de vie d'une batterie soit comprise entre 175 et 220 heures, ce qui correspond à l'intégrale de la densité de probabilité de 175 à 220 :", p(),
                  "$$ P(175 < X < 220) = \\int_{175}^{220} f(x) \\, dx $$", p(),
                  "Calculons cette intégrale en utilisant les données du problème :", p(),
                  "$$ \\int_{175}^{220} f(x) \\, dx = \\int_{175}^{220} \\frac{1}{\\pi \\cdot 20 \\cdot \\left(1 + \\left(\\frac{x - 200}{20}\\right)^2\\right)} \\, dx $$", p(),
                  "À défaut d'effectuer ce calcul manuellement, utilisons R : ", p(),
                  wellPanel(
                    style = "background-color: #f5f5f5; border: 1px solid #ddd; padding: 10px;",
                    tags$pre("
# Charger la librairie permettant l'intégration
library(pracma)

# Définir la fonction de densité de probabilité
f_cauchy <- function(x, median, gamma) {
  return(1 / (pi * gamma * (1 + ((x - median) / gamma)^2)))
}

median <- 200
gamma <- 20

# Calculer P(175 < X < 220)
result <- integrate(f_cauchy, 
                    175, 220, 
                    median = median, 
                    gamma = gamma)

# Afficher le résultat
cat('%P(175 < x < 220) = ', round(result$value * 100, 2), '%')")
                  ),
                  
                  strong("Sous R"), " :", br(),
                  wellPanel(
                    style = "background-color: #f5f5f5; border: 1px solid #ddd; padding: 10px;",
                    tags$pre("gamma <- 20
median <- 200
p_cauchy <- pcauchy(c(175, 220), 
                    location = median, 
                    scale = gamma, 
                    lower.tail = TRUE)
p_percentage <- diff(p_cauchy) * 100
cat('%P(175 < X < 220) = ', round(p_percentage, 2), '%')")
                  )
           )
         )
),

tabPanel("Exercise 1D",
         fluidRow(
           column(width = 4,
                  h3("Problème"),
                  h4("1D. Durée de vie maximale des batteries"),
                  p("Un expérimentateur souhaite déterminer la durée de vie maximale des batteries d'un certain modèle. Il choisit au hasard 50 batteries et enregistre leur durée de vie en heures. Sachant que la durée de vie des batteries suit une distribution de Cauchy avec une médiane de 200 heures et un paramètre d'échelle de 20 heures, quelle est la durée de vie maximale que ne dépasserait pas 90 % des batteries ?")
           ),
           column(width = 8,
                  h3("Solutions"),
                  h4("1D. Durée de vie maximale des batteries"),
                  strong("Sous R"), " :", br(),
                  "Pour ce problème, $\\text{median} = 200$, et $\\gamma = 20$ heures.", p(),
                  "Nous devons trouver la durée de vie maximale [notée X] que 90 % des batteries ne dépasseront pas. Cela correspond à trouver le quantile de la distribution de Cauchy sous lequel on trouve 90% de la distribution:", p(),
                  "$$ P(X < x_{90}) = 0.90 $$", p(),
                  "Calculons cette durée maximale en utilisant les données du problème, et la fonction qcauchy :", p(),
                  wellPanel(
                    style = "background-color: #f5f5f5; border: 1px solid #ddd; padding: 10px;",
                    tags$pre("
# Définir les paramètres
gamma <- 20
median <- 200

# Calculer le quantile correspondant à la probabilité 0.90
quantile_90 <- qcauchy(0.90, 
                       location = median, 
                       scale = gamma)
cat('La durée de vie maximale que ne dépasseraient pas 90 % des batteries est de', round(quantile_90, 2), 'heures.')"
                    )
                  )
           )
           )
         ),
           
tabPanel("Exercice 2",
         column(width = 4,
                h3("Problème"),
                h4("2. Temps de réaction des conducteurs"),
                p("Un autre chercheur étudie le temps de réaction des conducteurs dans des conditions de visibilité réduite. Il suppose que le temps de réaction suit une distribution de Cauchy avec une médiane de 0,8 seconde et un paramètre d'échelle de 0,2 seconde. Quel est le temps de réaction qui sépare les 90 % les plus rapides des 10 % les plus lents ?")
         ),
         column(width = 8,
                h3("Solutions"),
                h4("2. Temps de réaction des conducteurs"),
                strong("Sous R"), " :", br(),
                "Pour ce problème, la médiane est de $0.8$ seconde et le paramètre d'échelle est de $0.2$ seconde.", p(),
                "Nous devons trouver le temps de réaction qui sépare les 90 % les plus rapides des 10 % les plus lents. Ce temps de réaction correspond au 90ème percentile de la distribution de Cauchy.", br(),
                "Notons que les temps de réaction courts se trouvent dans la portion inférieure de la distribution, et les temps longs dans la portion supérieure. Nous pouvons utiliser la fonction qcauchy de R pour trouver ce quantile :", p(),
                wellPanel(
                  style = "background-color: #f5f5f5; border: 1px solid #ddd; padding: 10px;",
                  tags$pre("
# Paramètres de la distribution de Cauchy
median <- 0.8
gamma <- 0.2

# Quantile pour séparer les 90% les plus rapides des 10% les plus lents
quantile_90 <- qcauchy(0.9, location = median, scale = gamma)

# Afficher le résultat
cat('Le temps de réaction qui sépare les 90 % les plus rapides des 10 % les plus lents est de', round(quantile_90, 6), 'secondes.')")
                ),
                strong("Solution analytique"), " :", br(),
                p("La distribution de Cauchy est définie par sa fonction de répartition cumulative (CDF) :", br(),
                  "$$ F(x; x_0, \\gamma) = \\frac{1}{\\pi} \\arctan\\left(\\frac{x - x_0}{\\gamma}\\right) + \\frac{1}{2} $$", br(),
                  "où $x_0$ est la médiane [ici 0,8] et $\\gamma$ est le paramètre d'échelle [ici 0,2].", br(),
                  "Nous cherchons à déterminer le 90ème percentile, noté $x_{90}$, tel que :", br(),
                  "$$ F(x_{90}; 0.8, 0.2) = 0.9 $$", br(),
                  "En résolvant pour $x_{90}$ :", br(),
                  "$$ \\frac{1}{\\pi} \\arctan\\left(\\frac{x_{90} - 0,8}{0,2}\\right) + \\frac{1}{2} = 0,9 $$", br(),
                  "Isolons l'arctangente :", br(),
                  "$$ \\frac{1}{\\pi} \\arctan\\left(\\frac{x_{90} - 0,8}{0,2}\\right) = 0.4 $$", br(),
                  "$$ \\arctan\\left(\\frac{x_{90} - 0.8}{0.2}\\right) = 0.4 \\pi $$", br(),
                  "Calculons $\\tan(0.4 \\pi)$ :", br(),
                  "$$ \\tan(0.4 \\pi) \\approx 1.19175359 $$", br(),
                  "Nous avons donc :", br(),
                  "$$ \\frac{x_{90} - 0.8}{0.2} = 1.19175359 $$", br(),
                  "D'où :", br(),
                  "$$ x_{90} - 0.8 = 0.2 \\times 1.19175359 $$", br(),
                  "$$ x_{90} - 0.8 = 0.238350718 $$", br(),
                  "$$ x_{90} = 0.8 + 0.238350718 $$", br(),
                  "$$ x_{90} \\approx 1.038350718 $$", br(),
                  "Cependant, en utilisant la fonction `qcauchy` de R, nous trouvons que la valeur exacte est environ 1.415537 secondes. La différence est due aux erreurs induites par les arrondissements."))
)
)
)
)
)
)
# https://vrcacademy.com/tutorials/cauchy-distribution-examples/

server <- function(input, output, session) {
  observeEvent(input$reset_button, {
    updateSliderInput(session, "median", value = 0)
    updateSliderInput(session, "gamma", value = 1)
    
    updateNumericInput(session, "median_num", value = 0)
    updateNumericInput(session, "gamma_num", value = 1)
    
    updateSwitchInput(session, "input_type", value = TRUE)
    updateSwitchInput(session, "input_mode", value = TRUE)
    updateNumericInput(session, "num_observations", value = 500)
    
    output$empirical_hist <- renderPlot(NULL)
    output$empirical_cdf <- renderPlot(NULL)
    output$empirical_hist_sample <- renderPlot(NULL)
    output$empirical_qq_sample <- renderPlot(NULL)
    output$statsOutput <- renderDT(NULL)
    output$fittedParams <- renderDT(NULL)
  })
  
  sample_data <- reactiveVal(NULL)
  
  parms <- reactive({
    if (input$input_type) {
      list(
        shape = input$median,
        scale = input$gamma
      )
    } else {
      list(
        shape = input$median_num,
        scale = input$gamma_num
      )
    }
  })
  
  observe({
    median <- if(input$input_type) input$median else input$median_num
    gamma <- if(input$input_type) input$gamma else input$gamma_num
 
    output$theoretical_density <- renderPlot({
      median <- if (input$input_type) input$median else input$median_num
      gamma <- if (input$input_type) input$gamma else input$gamma_num
      x <- seq(-50, 50, length.out = 1000)
      data <- data.frame(x, y = dcauchy(x, location = median, scale = gamma))
      
      density_equation <- TeX(sprintf("$f(x) = \\frac{1}{\\pi \\cdot %.2f \\left(1 + \\left(\\frac{x - %.2f}{%.2f}\\right)^2\\right)}$", gamma, median, gamma))
      
      ggplot(data, aes(x, y)) +
        geom_line() +
        labs(
          title = "Courbe de densité théorique",
          subtitle = density_equation,
          x = "x", y = "Densité"
        ) +
        theme_minimal() +
        theme(
          plot.subtitle = element_text(size = 15, hjust = 0.5, vjust = 1),
          plot.title = element_text(size = 15, hjust = 0.5, vjust = 1)
        )
    })
  })
  
  
  output$theoretical_cdf <- renderPlot({
    median <- input$median
    gamma <- input$gamma
    x <- seq(-50, 50, length.out = 1000)
    theoretical_cdf <- data.frame(x, y = pcauchy(x, location = median, scale = gamma))
    
    cdf_equation <- TeX(sprintf("$F(x) = \\frac{1}{2} + \\frac{1}{\\pi} \\cdot atan\\left(\\frac{x - %.2f}{%.2f}\\right)$", median, gamma))
    
    ggplot(theoretical_cdf, aes(x, y)) +
      geom_line(color = "red") +
      labs(
        title = "Courbe de répartition cumulative théorique",
        subtitle = cdf_equation,
        x = "x", y = "Probabilité cumulative"
      ) +
      theme_minimal() +
      theme(
        plot.subtitle = element_text(size = 15, hjust = 0.5, vjust = 1),
        plot.title = element_text(size = 15, hjust = 0.5, vjust = 1)
      )
  })
  

    observeEvent(input$generate_button, {
      median <- if(input$input_type) input$median else input$median_num
      gamma <- if(input$input_type) input$gamma else input$gamma_num
      n <- input$num_observations
      set.seed(123)
      data <- rcauchy(n, location = median, scale = gamma)
      sample_data(data)

      output$empirical_hist <- renderPlot({
        ggplot(data.frame(x = sample_data()), aes(x)) +
          geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
          stat_function(fun = dcauchy, args = list(location = median, scale = gamma), color = "red") +
          xlim(quantile(sample_data(), c(0.05, 0.95))) +
          theme_minimal()
      })
      
      output$empirical_cdf <- renderPlot({
        median <- if(input$input_type) input$median else input$median_num
        gamma <- if(input$input_type) input$gamma else input$gamma_num
        
        empirical_cdf <- ecdf(sample_data())
        
        ggplot(data.frame(x = sample_data()), aes(x)) +
          stat_ecdf(geom = "step") +
          labs(title = "Courbe de répartition cumulative empirique", x = "x", y = "Probabilité cumulative") +
          xlim(quantile(sample_data(), c(0.05, 0.95))) +
          theme_minimal()
      })
})

    observeEvent(input$file1, {
      req(input$file1)
      data <- read.csv(input$file1$datapath)
      sample_data(data[, 1])
      median_est <- median(sample_data())
      iqr_est <- IQR(sample_data())
      gamma_est <- iqr_est / 2
      updateNumericInput(session, "median", value = median_est)
      updateNumericInput(session, "gamma", value = gamma_est)
      
      output$fittedParams <- renderDT({
        datatable(data.frame(Paramètre = c("Médiane", "gamma"), Estimation = round(c(median_est, gamma_est), 4)), options = list(pageLength = 5))
      })
      
      output$empirical_hist_sample <- renderPlot({
        ggplot(data.frame(x = sample_data()), aes(x)) +
          geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
          stat_function(fun = dcauchy, args = list(location = median_est, scale = gamma_est), color = "red") +
          labs(title = "Histogramme empirique avec densité ajustée", x = "x", y = "Densité") +
          theme_minimal()
      })
      
      output$empirical_cdf_sample <- renderPlot({
        empirical_cdf <- ecdf(sample_data())
        
        ggplot(data.frame(x = sample_data()), aes(x)) +
          stat_ecdf(geom = "step") +
          labs(title = "Courbe de répartition cumulative empirique", x = "x", y = "Probabilité cumulative") +
          xlim(quantile(sample_data(), c(0.05, 0.95))) +
          theme_minimal()
      })
    })
  
  output$scalePlot <- renderPlot({
    x <- seq(-10, 10, length.out = 1000)
    data <- data.frame(
      x = rep(x, 4),
      y = c(dcauchy(x, location = 0, scale = 0.5), 
            dcauchy(x, location = 0, scale = 1), 
            dcauchy(x, location = 0, scale = 2),
            dcauchy(x, location = 0, scale = 4)),
      scale = factor(rep(c(0.5, 1, 2, 4), each = length(x)))
    )
    
    p <- ggplot(data, aes(x = x, y = y, color = scale)) +
      geom_line() +
      labs(title = "Effet du paramètre d'échelle (\u03B3)",
           x = "x", y = "Densité") +
      scale_color_manual(values = c("red", "blue", "green", "orange"),
                         labels = c("\u03B3 = 0.5", "\u03B3 = 1", "\u03B3 = 2", "\u03B3 = 4")) +
      labs(color = "Échelle") + 
      theme_minimal() +
      theme(legend.position = c(0.25, 0.7),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 14),
            plot.title = element_text(size = 18))
    print(p)
  })
  
  output$locationPlot <- renderPlot({
    x <- seq(-10, 10, length.out = 1000)
    data <- data.frame(
      x = rep(x, 4),
      y = c(dcauchy(x, location = -2, scale = 1), 
            dcauchy(x, location = 0, scale = 1), 
            dcauchy(x, location = 2, scale = 1),
            dcauchy(x, location = 4, scale = 1)),
      location = factor(rep(c(-2, 0, 2, 4), each = length(x)))
    )
    
    p <- ggplot(data, aes(x = x, y = y, color = location)) +
      geom_line() +
      labs(title = "Effet du paramètre de localisation (\u03BC)",
           x = "x", y = "Densité") +
      scale_color_manual(values = c("red", "blue", "green", "orange"),
                         labels = c("\u03BC = -2", "\u03BC = 0", "\u03BC = 2", "\u03BC = 4")) +
      theme_minimal()  +
      theme(legend.position = c(0.20, 0.7),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 14),
            plot.title = element_text(size = 18))
    print(p)
  })
  
  output$combinedPlot <- renderPlot({
    x <- seq(-10, 10, length.out = 1000)
    data <- data.frame(
      x = rep(x, 3),
      y = c(dcauchy(x, location = 0, scale = 1), 
            dcauchy(x, location = 2, scale = 2),
            dcauchy(x, location = -2, scale = 4)),
      combined = factor(rep(c("\u03BC = 0, \u03B3 = 1", "\u03BC = 2, \u03B3 = 2", "\u03BC = -2, \u03B3 = 4"), each = length(x)))
    )
    
    p <- ggplot(data, aes(x = x, y = y, color = combined)) +
      geom_line() +
      labs(title = "Combinaison des paramètres",
           x = "x", y = "Densité") +
      scale_color_manual(values = c("green", "blue", "red"),
                         labels = c("\u03BC = -2, \u03B3 = 4", "\u03BC = 0, \u03B3 = 1", "\u03BC = 2, \u03B3 = 2")) +
      theme_minimal()  +
      theme(legend.position = c(0.75, 0.8),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 14),
            plot.title = element_text(size = 18))
    print(p)
  })
  
  output$pdfEquation <- renderUI({
    params <- parms()
    withMathJax(
      helpText(
        "PDF Equation: $$f(x) = \\frac{1}{\\pi \\gamma \\left[1 + \\left( \\frac{x - x_0}{\\gamma} \\right)^2 \\right]}$$",
        "Where: $x_0$ =", round(params$shape, 6), ", $\\gamma$ =", round(params$scale, 6)
      )  
    )
  })
  
  output$cdfEquation <- renderUI({
    params <- parms()
    helpText(
      "CDF Equation: $$F(x) = \\frac{1}{\\pi} \\arctan\\left( \\frac{x - x_0}{\\gamma} \\right) + \\frac{1}{2}$$",
      "Where: $x_0$ =", round(params$shape, 6), ", $\\gamma$ =", round(params$scale, 6)
    )
  })
  
  output$probabilityPlot <- renderPlot({
    params <- parms()
    limdat <- qcauchy(c(0.025, 0.975), location = params$shape, scale = params$scale)
    x_values <- seq(limdat[1], limdat[2], by = 0.1)
    y_values <- dcauchy(x_values, location = params$shape, scale = params$scale)
    x_value <- input$x_value
    prob <- pcauchy(x_value, location = params$shape, scale = params$scale)
    
    df <- data.frame(x = x_values, y = y_values)
    shading_df <- data.frame(
      x = c(limdat[1], subset(df, x <= x_value)$x, x_value),
      y = c(0, subset(df, x <= x_value)$y, 0)
    )
    
    ggplot(df, aes(x = x, y = y)) +
      geom_line() +
      geom_polygon(data = shading_df, aes(x, y), fill = "lightblue", alpha = 0.5) +
      labs(x = "x", y = "Density", title = "Probability Density Function") +
      xlim(limdat[1], limdat[2]) +
      geom_vline(xintercept = x_value, linetype = "dashed") +
      annotate("text", x = x_value, y = 0, vjust = 1, label = sprintf("x = %.2f", x_value), color = "red")
  })
  
  output$quantilePlot <- renderPlot({
    params <- parms()
    prob_value <- input$prob_value
    
    if (is.na(prob_value) || prob_value < 0 || prob_value > 1) {
      return()
    }
    
    quantile_value <- qcauchy(prob_value, location = params$shape, scale = params$scale)
    
    prob_values <- seq(0, 1, by = 0.01)
    x_values <- qcauchy(prob_values, location = params$shape, scale = params$scale)
    
    valid_indices <- is.finite(x_values) & !is.na(x_values)
    x_values <- x_values[valid_indices]
    prob_values <- prob_values[valid_indices]
    
    if (length(x_values) == 0 || length(prob_values) == 0) {
      return()
    }
    
    df <- data.frame(x = x_values, prob = prob_values)
    
    ggplot(df, aes(x = x, y = prob)) +
      geom_line() +
      geom_hline(yintercept = prob_value, linetype = "dashed", color = "blue") +
      geom_vline(xintercept = quantile_value, linetype = "dashed", color = "red") +
      geom_point(x = quantile_value, y = prob_value, color = "black", shape = 16) +
      labs(x = "Quantile", y = "Cumulative Probability", title = "Quantile Function") +
      ylim(0, 1) +
      xlim(range(x_values))
  })
  
  computed_pr <- reactive({
    params <- parms()
    x_value <- input$x_value
    prob <- pcauchy(x_value, location = params$shape, scale = params$scale)
    paste("P[X <", x_value, "] = ", round(prob, 4))
  })
  
  computed_quantile <- reactive({
    params <- parms()
    prob_value <- input$prob_value
    quantile <- qcauchy(prob_value, location = params$shape, scale = params$scale)
    paste("Q[", prob_value, "] = ", round(quantile, 4))
  })
  
  output$computed_prob <- renderText({
    computed_pr()
  })
  
  output$computed_quantile <- renderText({
    computed_quantile()
  })
  output$download_data <- downloadHandler(
    filename = function() {
      paste("Cauchy_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data.frame(x = sample_data()), file, row.names = FALSE)
    })
  
}

shinyApp(ui, server)

      