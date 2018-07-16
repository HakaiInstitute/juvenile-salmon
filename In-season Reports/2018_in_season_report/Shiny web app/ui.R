navbarPage(
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
               includeMarkdown('overview.md')
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