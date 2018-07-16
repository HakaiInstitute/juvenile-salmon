library(shiny)
library(dplyr)
library(ggplot2)

survey_seines <- readRDS("data/survey_seines.RDS")

ui <- navbarPage(
  title = 'Hakai Institute Juvenile Salmon Program',
  tabPanel("Overview",
           sidebarLayout(
             sidebarPanel(
               helpText(),
               radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                            inline = TRUE),
               downloadButton('downloadReport')
             ),
             mainPanel(
               includeMarkdown('overview.md'),
               img(src = "map_2018.jpg", height = 800, width = 960)
             )
           )
  ),
  tabPanel("Migration Timing",
           sidebarLayout(
             sidebarPanel(
               helpText(),
               selectInput("Species", label = h3("Species"),
                           choices = list("Sockeye" = "SO", "Pink" = "PI",
                                          "Chum" = "CU"),
                           selected = "SO")
             ),
             mainPanel(
               plotOutput("migration_timing")
             )
           )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      src <- normalizePath('report.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      
      library(rmarkdown)
      out <- render('report.Rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

