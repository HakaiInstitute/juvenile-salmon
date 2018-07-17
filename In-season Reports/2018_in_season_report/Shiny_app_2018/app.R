library(shiny)
library(dplyr)
library(ggplot2)

survey_seines <- readRDS("data/survey_seines.RDS") %>% 
  rename("Sockeye" = "so_total", "Pink" = "pi_total", "Chum" = "cu_total")

#######

ui <-fluidPage(
  list(tags$head(HTML('<link rel="icon", href="square hakai favicon.png", 
                                   type="image/png" />'))),
  div(style="padding: 1px 0px; width: '100%'",
      titlePanel(
        title="", windowTitle="Hakai Juvenile Salmon Program"
      )
  ), 
  navbarPage(
  title = div(img(src="Hakai_red.png", height = 30, width = 92), 'Juvenile Salmon Program'),
  tabPanel("Overview",
           sidebarLayout(
             sidebarPanel(
               helpText("Adjust figures, return here and save as pdf"),
               downloadButton("report", "Generate PDF report")
             ),
             mainPanel(
               includeMarkdown('overview.md'),
               img(src = "map_2018.jpg", height = 600, width = 720)
             )
           )
  ),
  tabPanel("Migration Timing",
           sidebarLayout(
             sidebarPanel(
               helpText("Select the species you'd like to plot"),
               selectInput("species", label = h3("Species"),
                           choices = list("Sockeye" = "Sockeye", "Pink" = "Pink",
                                          "Chum" = "Chum"),
                           selected = "Sockeye")
             ),
             mainPanel(
               plotOutput("migration_timing")
             )
           )
  )
)
)


################ Server

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(species = input$species)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  output$migration_timing <- renderPlot({
    
    survey_seines %>% 
      select(year, region, sampling_week, input$species) %>%
      group_by(year, region, sampling_week) %>% 
      summarise(mean = mean(get(input$species), na.rm = T), se = sd(get(input$species)) / sqrt(n())) %>% 
      ungroup() %>% 
      ggplot(aes(x = as_factor(sampling_week), y = mean, colour = region, group = region))+
      geom_line(size = 1)+  
      geom_point() + 
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se, width = 0.2)) +
      scale_colour_discrete(name = "",
                            breaks=c("DI","JS"),
                            labels=c("Discovery Islands", "Johnstone Strait"))+
      #theme(legend.justification=c(1,0), legend.position=c(.8,.8),
            #legend.background = element_rect(fill=alpha(0.1))) + 
      theme(legend.text = element_text(colour="black", size = 12)) +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=12,face="bold")) +
      xlab("Date") +
      ylab("Abundance") +
      theme(legend.title = element_blank()) +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
      labs(
        title = input$species,
        caption = "Average number (± 1 SE) caught in each seine in 2018 averaged over one week periods for each region and represented by the middle day of each week"
      )    
  }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

