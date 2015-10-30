source("functions.R")

data <- read.csv("data/data.csv", stringsAsFactors = FALSE)
#data_recent <- get_recent_data()

shinyServer(function(input, output, session) {
    
  datasetInput <- reactive({
      ## En esta seccin se carga la base de datos desde la liga que se proporciona.
      if(input$data_set == "Históricos"){
          data
      }else{
          get_recent_data()
      }
  })
  
    output$table <- renderDataTable({
        ## Despliegue de resultados.
            datasetInput()
    })

    output$tot_conj <- renderText({
        nrow(data)
    })

    output$tot_rec <- renderText({
        sum(data$Recursos)
    })

    output$hist <- renderPlot({
        data$fecha <- as.Date(data$fecha)
        data       <- filter(data, Inst != "null" )
            inst <- str_sub(data$Inst,1, 20)
            data_plot <- plyr::count(inst)
            plot <- ggplot(data = data_plot,
                   aes(x = x, y = freq)) +
                geom_bar(stat = "identity",fill = "#2962ff") +
                geom_text(aes(label = freq, y = freq + 3), col = "#2962ff", size = 5) +
                theme(panel.background = element_blank(),
                      axis.text.y = element_text(color="#2962ff", size = 15),
                      axis.title = element_blank()) +
                coord_flip()
        plot
    }, height = 2500, width = 1000)


    output$t_series<- renderPlot({
        data$fecha <- as.Date(data$fecha)
        data       <- filter(data, Inst != "null" )
                data_plot <- plyr::count(data[,c(1,5)])
            plot <- ggplot(data_plot, aes(x = fecha, y = freq, col = Inst )) +
                geom_point(alpha = .5) +
            geom_smooth(aes(group = "1"), wide = 2) +
            theme(panel.background = element_blank(),
                  axis.text.y = element_text(color="#2962ff",
                                             size = 15),
                  axis.title  = element_blank(),
                  legend.position = "none"
                  )
        plot
    })


    
    output$downloadData <- downloadHandler(
        ## Nombre del archivo
        filename = function() {
            paste(input$nombre, input$filetype, sep = ".")
        },
        ## Esta fun. escribe el archivo.
        content = function(file) {
            sep <- switch(input$filetype, "csv" = ",", "tsv" = "\t")
            ## Write to a file specified by the 'file' argument
            write.table(datasetInput(), file, sep = sep,
                        row.names = FALSE)
        }
    )
})
