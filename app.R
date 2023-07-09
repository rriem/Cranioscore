

library(shiny)


ui <- fluidPage(title = "Cranioscore  V 0.2.2",
                
                fluidRow(column(4, offset = 4,
                                h3(
                                  "Cranioscore v0.2.2"
                                ))),
                
                fluidRow(
                  column(
                    4,
                    br(),
                    radioButtons(
                      inputId = "gcs",
                      label = "Glasgow coma scale préopératoire égal à 15 ",
                      selected = 1 ,
                      inline = TRUE,
                      choiceNames = c("Oui", "Non"),
                      choiceValues = c(0, 1)
                    ),
                    radioButtons(
                      inputId = "atcd",
                      label = "Antécédent de neurochirurgie ",
                      selected = 0,
                      inline = TRUE,
                      choiceNames = c("Oui", "Non"),
                      choiceValues = c(1, 0)
                    ),
                    numericInput(
                      inputId = "taille",
                      label = "Plus grande taille de la tumeur (mm) ",
                      10,
                      min = 2,
                      max = 100
                    ),
                    radioButtons(
                      inputId = "deviation",
                      label = "Déviation de la ligne médiane de 3 mm ou plus",
                      selected = 0,
                      inline = TRUE,
                      choiceNames = c("Oui", "Non"),
                      choiceValues = c(1, 0)
                    )
                  ),
                  column(
                    4,
                    offset = 2,
                    br(),
                    radioButtons(
                      inputId = "transfu",
                      label = "Transfusion peropératoire ",
                      selected = 0,
                      inline = TRUE,
                      choiceNames = c("Oui", "Non"),
                      choiceValues = c(1, 0)
                    ),
                    numericInput(
                      inputId = "pasmax",
                      label = "Pression artérielle systolique peropératoire maximale (mmHg) ",
                      150,
                      min = 60,
                      max = 300
                    ),
                    numericInput(
                      inputId = "pasmin",
                      label = "Pression artérielle systolique peropératoire minimale (mmHg) ",
                      80,
                      min = 20,
                      max = 150
                    ),
                    numericInput(inputId = "duree", label = "Durée de l'intervention (min) ", 60)
                  ),
                  
                  column(
                    12,
                    hr(),
                    verbatimTextOutput("resultat", placeholder = TRUE),
                    br(),
                    hr(),
                    tags$p(
                      HTML(
                        "D'après <strong>Cinotti R <em>et al.</em></strong> Prediction Score for Postoperative Neurologic Complications after Brain Tumor Craniotomy: A Multicenter Observational Study. <em>Anesthesiology 2018;129:1111-1120</em>. <a href=http://doi.org/10.1097/ALN.0000000000002426 , target=_BLANK> Disponible ici </a>"
                      )
                    ),
                    tags$p(
                      HTML(
                        "Créé par <em>Romuald Riem MD MSc</em> <br /> <a rel=license href=http://creativecommons.org/licenses/by/4.0/><img alt=Creative Commons License style=border-width:0 src=https://i.creativecommons.org/l/by/4.0/88x31.png /></a> This work is licensed under a <a rel=license href=http://creativecommons.org/licenses/by/4.0/>Creative Commons Attribution 4.0 International License</a>."
                      )
                    ),
                    tags$p(
                      HTML(
                        "Les sources sont disponibles <a href=http://vps.r-riem.fr/shiny/rriem/cranioscore/app.zip target=_BLANK> ici</a>."
                      )
                    )
                  )
                )
)




server <- function(input, output) {
   
   output$resultat <- renderPrint({
      logit <- -4.80943 + (1.514867*as.numeric(input$gcs)) + (1.05338*as.numeric(input$atcd)) + (0.0087759*input$taille) + (0.511444*as.numeric(input$deviation)) + (0.5164327*as.numeric(input$transfu)) + (0.0117567*as.numeric(input$pasmax)) - (0.0130451*as.numeric(input$pasmin)) + (0.2980989*(input$duree*0.016667))
      odd <- exp(logit)
      p <- odd / (1 + odd)
      pour <- round(p*100,1)
      cat(paste("Ce patient a une probabilité de", round(p,3), "(soit", pour, "%) de présenter une complication postopératoire grave nécessitant une prise en charge en réanimation.", " "))
   })
}


# Run the application 
shinyApp(ui = ui, server = server)


