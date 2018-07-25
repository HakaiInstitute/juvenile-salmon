library(shiny)
library(forcats)
library(lubridate)
library(dplyr)
library(ggplot2)
library(hakaisalmon)

survey_seines <- readRDS("data/survey_seines.RDS") %>% 
  rename("Sockeye" = "so_total", "Pink" = "pi_total", "Chum" = "cu_total")

fish_and_sealice_field_data <- readRDS("data/fish_and_sealice_field_data.RDS")

summary_sealice <- readRDS("data/summary_sealice.RDS")

spp_labels <- c(CU = "Chum", PI = "Pink", SO = "Sockeye", DI = "Discovery Islands", 
                JS = "Johnstone Strait")

theme_set(theme_bw(base_size = 17))
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
  ),
  tabPanel("Parasite Loads",
           sidebarLayout(
             sidebarPanel(width = 3,
              helpText(),
              selectInput("Parameter", label = h3("Parameter"),
                          choices = list("Prevalence" = "prevalence",
                                         "Intensity" = "intensity",
                                         "Abundance" = "abundance"),
                          selected = "Prevalence")
             ),
             mainPanel(
               plotOutput("Parasite_Loads")
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
      tex_file <- file.path(tempdir(), "figure_opts.tex")
      hakai_logo <- file.path(tempdir(), "Hakai Institute Logo Vector.png")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      file.copy("map_2018.jpg", src2, overwrite = TRUE)
      file.copy("figure_opts.tex", tex_file, overwrite = TRUE)
      file.copy("Hakai Institute Logo Vector.png", hakai_logo, overwrite = TRUE)
      # Set up parameters to pass to Rmd document
      params <- list(Species = input$Species, Region = input$Region, 
                     Length_Species = input$Length_Species, 
                     Parameter = input$Parameter)
      
      
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
      xlab("Date") +
      ylab("Abundance") +
      theme(legend.title = element_blank()) +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
      labs(
        title = input$Species,
        caption = "Average catch (± 1 SE) in 2018 for one week periods represented by the middle day of each week."
      ) +
      theme(
        plot.caption = element_text(hjust = 0)
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
      ggtitle(ifelse(input$Region == "DI", "Discovery Islands", "Johnstone Strait")) +
      theme(legend.position="bottom") +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
      labs(
        caption = "Fork length boxplots of juvenile salmon grouped by week and represented by the middle day of each week."
      ) +
      theme(
        plot.caption = element_text(hjust = 0)
      )
  }
  )
  output$Parasite_Loads <- renderPlot({
      summary_sealice %>% 
      group_by(year, region, louse_species, species) %>% 
      summarize(mean = mean(get(input$Parameter), na.rm = TRUE),
                sd = sd(get(input$Parameter), na.rm = TRUE),
                se = sd(get(input$Parameter), na.rm = TRUE)/sqrt(n())) %>%
      ungroup() %>% 
      ggplot(aes(x = factor(year), y = mean, fill = factor(louse_species),
                 group = factor(louse_species))) +
      geom_bar(stat = "identity", position = position_dodge()) +
      geom_errorbar(aes(ymin = mean - se, 
                        ymax = mean + se), 
                        width = 0.2,
                        position = position_dodge(0.9)
                        ) +
      facet_grid(region ~ species, labeller = labeller(species = spp_labels, 
                                                       region = spp_labels)) +
      labs(x = "Year", y = input$Parameter) +
      theme(legend.text = element_text(face = "italic")) +
      scale_fill_discrete(name="Louse Species",
                            breaks=c("motile_caligus", "motile_lep"),
                            labels=c("C. clemensi", "L. salmonis")) +
      theme(legend.position="bottom") +
      labs(caption = paste("The", input$Parameter, " (± SE) of sealice per juvenile salmon infected with motile Lepeoptheirus salmonis and Caligus clemensi \n louse in 2018."
      )
      ) +
      theme(
        plot.caption = element_text(hjust = 0)
      ) +
        geom_text(aes(label = ifelse(mean == 0, round(mean, 1), '')), hjust = -2.5)
  }
  )
  output$sst <- renderPlot({
    
  })
}
# Run the application 
shinyApp(ui = ui, server = server)

