library(shiny)
library(dplyr)
library(ggplot2)
library(hakaisalmon)

survey_seines <- readRDS("data/survey_seines.RDS") %>% 
  rename("Sockeye" = "so_total", "Pink" = "pi_total", "Chum" = "cu_total")

fish_and_sealice_field_data <- readRDS("data/fish_and_sealice_field_data.RDS")

spp_labels <- c(CU = "Chum", PI = "Pink", SO = "Sockeye", DI = "Discovery Islands", 
                JS = "Johnstone Strait")

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
             sidebarPanel(width = 3,
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
             sidebarPanel(width = 3,
               helpText("Select the species you'd like to plot"),
               selectInput("Species", label = h3("Species"),
                           choices = list("Sockeye" = "Sockeye", "Pink" = "Pink",
                                          "Chum" = "Chum"),
                           selected = "Sockeye")
             ),
             mainPanel(
               plotOutput("migration_timing")
             )
           )
  ),
  tabPanel("Length",
           sidebarLayout(
             sidebarPanel(width = 3,
               helpText(),
               selectInput("Region", label = h3("Region"),
                           choices = list("Discovery Islands" = "DI", "Johnstone Strait" = "JS"),
                           selected = "Discovery Islands"),
               selectInput("Length_Species", label = h3("Species"),
                           choices = list("Sockeye" = "SO", "Pink" = "PI",
                                          "Chum" = "CU"),
                           selected = "Sockeye")
             ),
             mainPanel(
               plotOutput("Length")
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
      src2 <- file.path(tempdir(), "map_2018.jpg")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      file.copy("map_2018.jpg", src2, overwrite = TRUE)
      # Set up parameters to pass to Rmd document
      params <- list(Species = input$Species, Region = input$Region, 
                     Length_Species = input$Length_Species)
      
      
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
      select(year, region, sampling_week, input$Species) %>%
      group_by(year, region, sampling_week) %>% 
      summarise(mean = mean(get(input$Species), na.rm = T), se = sd(get(input$Species)) / sqrt(n())) %>% 
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
        title = input$Species,
        caption = "Average number (± 1 SE) caught in each seine in 2018 averaged over one week periods for each region and represented by the middle day of each week"
      )    
  }
  )
  output$Length <- renderPlot({
    length_species <- input$Length_Species
    length_region <- input$Region
    length_histo_15_17 <- hakaisalmon::survey_seines_fish %>% 
      select(sampling_week, survey_date, region, species, fork_length) %>% 
      drop_na(fork_length) %>% 
      filter(species == length_species, region == length_region) %>% 
      mutate(year = year(survey_date))
    
    length_histo_2018 <- fish_and_sealice_field_data %>% 
      select(sampling_week, survey_date, region, species, fork_length) %>% 
      filter(species == length_species, region == length_region) %>% 
      mutate(year = year(survey_date))
    
    length_histo <- rbind(length_histo_2018, length_histo_15_17) %>% 
      drop_na(fork_length) %>% 
      mutate(year = as.factor(year))
    
    ggplot(length_histo, aes(x = sampling_week, y = fork_length, fill = year)) +
      geom_boxplot()+
      ylab("Fork Length (mm)")+
      xlab("Date")+
      theme(legend.text = element_text(colour="black", size = 12)) + 
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=12,face="bold")) +
      ggtitle(input$Region) +
      theme(legend.position="bottom") +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
      labs(
        caption = "Fork length boxplots of juvenile salmon grouped by week, and represented by the middle day of each week, compared to the average length from 2015, 2016 and 2017."
      )
  }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

