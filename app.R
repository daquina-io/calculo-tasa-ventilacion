library(shiny)
library(readr)
library(ggplot2)
library(dplyr)

set_color_co2 <- function(x) {
  ifelse( x < 600 , "green",
  ifelse( x >= 600 & x < 800 , "yellow",
  ifelse( x >= 800 & x < 1000, "orange",
  ifelse( x >= 1000 & x < 1500, "red",
  ifelse( x >= 1500 & x < 2000, "purple",
         "maroon")))))
}

set_color <- function(pollulant) {
  if(pollulant == "PM25") return( set_color_p25 )
  return( set_color_co2 )
}

# Define UI for data upload app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Cálculo de la tasa de ventilación en un espacio - ACH"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Select a file ----
      fileInput("file1", "Elija su archivo txt",
                multiple = FALSE,
                accept = c("application/text",
                         ".txt")),

      # Horizontal line ----
      tags$hr(),
      numericInput('co2_exterior',"Valor CO2 de referencia en exteriores", value= 400),
      sliderInput('sample', 'Muestras en datos experimentales para seleccinar ¿en dònde empieza a decaer el aire en el espacio? y ¿en dónde termina de decaer el aire en el espacio?', min=1, max=100, value=c(1,100)),

      a("visor desarrollado por un/loquer", href="https://github.com/daquina-io/calculo-tasa-ventilacion")

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Data file ----
      verbatimTextOutput("textCO2"),
      plotOutput("plotCO2")
    )

  )
)

## Define server logic to read selected file ----
server <- function(input, output, session) {

  df <- reactive({

        ## input$file1 will be NULL initially. After the user selects
        ## and uploads a file, head of that data file by default,
        ## or all rows if selected, will be shown.

        req(input$file1)

        ## when reading semicolon separated files,
        ## having a comma separator causes `read.csv` to error
        tryCatch(
        {
          df <- read.delim(input$file1$datapath, header = F)
          df <- df[1:dim(df)[1],] %>% strsplit(" -> ") %>% unlist %>% matrix(nrow = dim(df)[1], byrow = TRUE)

          idxStartMeasurements <- which(df[,2] == "Start measurements")
          df <- df[-c(1:idxStartMeasurements), ]
          df <- df[-which(df[,2] == "Read SenseAir S8: OK DATA"),]
          df[,2] <- df[,2] %>% sub("CO2\\(ppm\\)\\: ","",.)

          df <- df %>% as.data.frame
          colnames(df) <- c("tiempo","CO2 [ppm]")
          df[,"tiempo"] <- parse_time(df[,"tiempo"])
          df[,"CO2 [ppm]"] <- as.numeric(df[,"CO2 [ppm]"])
          df
        },
        error = function(e) {
            ## return a safeError if a parsing error occurs
            stop(safeError(e))
        }
        )


        samplesLength <- dim(df)[1]
        ## Control the value, min, max, and step.
        updateSliderInput(session, "sample", value = c(1,samplesLength),
                          min = 1, max = samplesLength, step = 1)

        colnames(df) <- c("tiempo","CO2 [ppm]")

        df
  })

  output$textCO2 <-  renderPrint({
    co2_decaystart <- df()[input$sample[1],2]
    t_decaystart <- df()[input$sample[1],1]
    co2_tdecayend <- input$co2_exterior + (co2_decaystart-input$co2_exterior)*0.37
    ## Opción para calcularlo automáticamente
    ## co2_tdecayend <- which.min(abs(array - value))

    ach <- -1*log(
    (df()[input$sample[2],2] - input$co2_exterior)/
    (df()[input$sample[1],2] - input$co2_exterior))/(as.numeric(df()[input$sample[2],1]-df()[input$sample[1],1])/3600)

    list(
      "Muestras seleccionadas para comienza_decaimiento[1] & finaliza_decaimiento[2] del aire en el espacio" = df()[input$sample,],
      "Valor teórico finaliza_decaimiento[2] a buscar en los datos experimentales"=co2_tdecayend,
      "Tiempo transcurrido en el decaimiento [seg]"=as.numeric(df()[input$sample[2],1]-df()[input$sample[1],1]),
      "ACH"=ach[[1]]
      )
  })

  output$plotCO2 <-  renderPlot({
    g <- ggplot(NULL, aes(x=tiempo,y=`CO2 [ppm]`)) + geom_point(data = df()[input$sample,], size = 5, col = "red") + geom_line(data= df(), col = "blue")
    g
  })

}

## Create Shiny app ----
shinyApp(ui, server)
