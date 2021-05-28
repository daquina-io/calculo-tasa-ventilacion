library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(stringr)

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
      sliderInput('sample', label = 'Muestras en datos que se deben seleccionar, para el comienzo del decaimiento de CO2 en el espacio y donde termina de decaer la concentración', min=1, max=100, value=c(1,100)),
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
          ## df <- read.delim("./misc/serial_20210526_184537.txt", header = F)
          df <- read.delim(input$file1$datapath, header = F)

          ## Extrae los tiempos registrados para cada línea
          timecol <- df[1:dim(df)[1],] %>% str_extract("(^[0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{3})")

          ## Donde no hay un tiempo de la forma hh:mm:ss.sss, descarta estas líneas
          if(length(which(is.na(timecol))) > 0) {
            df <- data.frame("V1"=df[-which(is.na(timecol)),])
            timecol <- timecol[-which(is.na(timecol))]
          } else df

          ## Separa el tiempo del demás texto de cada línea para ser parseado a continuación, obteniendo los datos de concentración de CO2
          textcol <- df[1:dim(df)[1],] %>% str_replace("(^[0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{3}) ","")
          ## Detecta las líneas con valores de CO2
          idxCO2 <- textcol %>% str_detect("CO2|OK")
          df <- data.frame(timecol[idxCO2], textcol[idxCO2] %>% str_replace("\\A.*\\: |OK ",""))

          colnames(df) <- c("tiempo","CO2 [ppm]")
          df[,"tiempo"] <- parse_time(df[,"tiempo"])
          df[,"CO2 [ppm]"] <- as.numeric(df[,"CO2 [ppm]"])
          df <- df %>% na.omit
        },
        error = function(e) {
            ## return a safeError if a parsing error occurs
            stop(safeError(e))
        }
        )


        samples_length <- dim(df)[1]
        ## Calcula la ubicación en el arreglo del valor máximo
        max_value_idx <- which(df[,2] == max(df[,2]))[1]
        ## Calcula el valor teoríco donde termina el decaimiento de concentración de CO2
        co2_tdecayend <- input$co2_exterior + (max(df[,2])-input$co2_exterior)*0.37
        ## Calcula ubicación del valor más próximo en la muestra de datos
        ## donde termina el decaimiento de la concentración de CO2
        co2_tdecayend_idx <- which.min(abs(df[,2] - co2_tdecayend))
        ## Si el valor donde termina el decaimiento está antes que el valor máximo
        ## lo recalcula para las muestras a partir del valor máximo
        if(co2_tdecayend_idx <= max_value_idx) co2_tdecayend_idx <- which.min(abs(df[max_value_idx:samples_length,2] - co2_tdecayend)) + max_value_idx
        ## Establece en el slider el valor máximo y el final del decaimiento de CO2
        ## actualiza estos valores en la UI
        updateSliderInput(session, "sample", value = c(max_value_idx,co2_tdecayend_idx),
                          min = 1, max = samples_length, step = 1)

        colnames(df) <- c("tiempo","CO2 [ppm]")

        df
  })

  output$textCO2 <-  renderPrint({
    co2_decaystart <- df()[input$sample[1],2]
    t_decaystart <- df()[input$sample[1],1]
    co2_tdecayend <- input$co2_exterior + (co2_decaystart-input$co2_exterior)*0.37

    ## Fórmula para el cálculo de ACH
    ## https://drive.google.com/file/d/1HwLhHk4XWmC1W1h9qFKI68KtveFufPV4/view
    ## https://wiki.unloquer.org/personas/brolin/proyectos/agentes_calidad_aire/tasaventilacion
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
