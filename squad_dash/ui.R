shinyUI(fluidPage(
    theme = "theme.css",
  titlePanel("Valida datos"),
  sidebarLayout(
      sidebarPanel(
          tabsetPanel(
              tabPanel(
                  h4("Análisis"),
                  br(),
                  radioButtons("data_set",h4("Despliegue de datos:"),
                               choices=c("Históricos",
                                         "Recientes"),
                               selected = "Históricos"),
                  br(),
#                  radioButtons("graph", h4("Tipo de gráfica:"),
 #                              choices=c("Histográma",
  #                                       "Serie de tiempo"),
   #                            selected = "Histográma"),
                  hr(),
                  h4("Estadísticas cláve:"),
                  h6("Número de Conjuntos:"),
                  h5(textOutput("tot_conj"), style="color:#2979ff"),
                  h6("Número de Recursos:"),
                  h5(textOutput("tot_rec"), style="color:#2979ff")

              ),
              tabPanel(
                  h4("Descarga"),
                   br(),
           textInput("nombre", h4("Nombre archivo de descarga:"), value = "archivo"),
      radioButtons("filetype", "File type:",
                   choices = c("csv")),
      downloadButton('downloadData', 'Download')
              )
          )       
      ),
      mainPanel(
      tabsetPanel(
          tabPanel(h3("Datos"),
                dataTableOutput('table')      
                ),
          tabPanel(h3("Histograma"),
                   plotOutput("hist",width = "100%")
                   ),
          tabPanel(h3("Serie de tiempo"),
                   plotOutput("t_series",width = "100%")
                   )
          
      ))
  )
))
