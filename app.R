install.packages("ggplot")
install.packages("shiny")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("plotly")
install.packages("readxl")
install.packages("shinythemes")
install.packages("ggthemes")

library(shiny)
#library(ggplot)
library(ggplot2)
library(dplyr)
library(plotly)
library(readxl)
library(shinythemes)
library(ggthemes)

#Datasets
adsl_data <- read_excel("adsl.xlsx")
adlb_data <- read_excel("adlb.xlsx") %>%
  filter(!grepl("change from previous visit", PARAM, ignore.case = TRUE))
data <- read_excel("filtered_subset_data_v4.xlsx")

# Descriptive statistics
descriptive_stats <- data.frame(
  Parameters = c("MIN", "VARIANCE", "MEAN", "MEDIAN", "STANDARD DEVIATION", "MAX"),
  AGE = c(51.00, 67.73, 75.09, 77.00, 77.00, 89.00),
  HEIGHTBL = c(135.90, 115.33, 163.93, 162.85, 10.74, 195.60),
  WEIGHTBL = c(34.00, 198.91, 66.65, 66.70, 14.10, 108.00),
  BMIBL = c(13.70, 16.68, 24.67, 24.20, 4.08, 40.10)
)

# UI
ui <- fluidPage(
  theme = shinytheme("journal"),
  titlePanel("Xanomeline Clinical Trial Data Visualisation Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("btn1", "Demographics", class = "btn-block btn-primary"),
      actionButton("btn2", "Vitals Data", class = "btn-block btn-primary"),
      actionButton("btn3", "Box Plots", class = "btn-block btn-primary"),
      width = 2
    ),
    
    mainPanel(
      div(class = "main-content", uiOutput("dashboardUI")),
      width = 10
    )
  )
)


# Server logic
server <- function(input, output, session) {
  
  #Demographics dashboard logic
  demo_dashboard <- reactive({
    fluidPage(
      theme = shinytheme("slate"),
      titlePanel("Demographics Dashboard"),
      
      fluidRow(
        column(3,  
               sidebarPanel(
                 selectInput("mainChoice", "Select Graph Category:",
                             choices = c("Distribution", "Proportions", "Completion  Flag", "Discontinuation Reasons", "Subject Status")),
                 
                 uiOutput("subChoiceUI"),
                 width = 12
               )
        ),
        column(9,  
               fluidRow(
                 column(9,  
                        plotlyOutput("mainPlot")
                 ),
                 column(12,  
                        tableOutput("statsTable")
                 )
               )
        )
      )
    )
  })
  
  #Vitals dashboard logic
  vitals_dashboard <- reactive({
    fluidPage(
      theme = shinytheme("slate"),
      titlePanel("Line Plots of AVAL for Selected PARAM with Mean and SD by Treatment Arm"),
      
      sidebarLayout(
        sidebarPanel(
          selectInput("param", "Select Parameter:", choices = unique(adlb_data$PARAM)),
          selectInput("agegrp", "Select Age Group:", choices = c("None", unique(adlb_data$AGEGR1))),
          selectInput("sex", "Select Sex:", choices = c("None", unique(adlb_data$SEX))),
          checkboxInput("overlap", "Overlap All Treatment Arms", value = FALSE)
        ),
        
        mainPanel(
          plotOutput("vital_plot")
        )
      )
    )
  })
  
  box_dashboard <- reactive({
    fluidPage(
      titlePanel("Boxplots of Baseline and End Treatment Values by PARAM"),
      
      sidebarLayout(
        sidebarPanel(
          selectInput("param", "Select Parameter:", choices = unique(data$PARAM)),
          selectInput("trta", "Select Treatment Group:", choices = unique(data$TRTA))
        ),
        
        mainPanel(
          plotOutput("boxplot")  # Plot output for the boxplot
        )
      )
    )
    
  })
  
  #Switching between dashboards
  observeEvent(input$btn1, {
    output$dashboardUI <- renderUI({ demo_dashboard() })
  })
  
  observeEvent(input$btn2, {
    output$dashboardUI <- renderUI({ vitals_dashboard() })
  })
  
  observeEvent(input$btn3, {
    output$dashboardUI <- renderUI({ box_dashboard() })
  })
  
  
  # Demographics Server Logic
  output$subChoiceUI <- renderUI({
    req(input$mainChoice)
    
    if (input$mainChoice == "Distribution") {
      selectInput("subChoice", "Select Baseline Parameter:",
                  choices = c("AGE", "BMIBL", "HEIGHTBL", "WEIGHTBL"))
    } 
    else if (input$mainChoice == "Proportions") {
      selectInput("subChoice", "Select Category:",
                  choices = c("ETHNIC", "RACE", "SEX", "TRT01P", "AGEGR1"))
    } 
    else if (input$mainChoice == "Completion  Flag") {
      selectInput("subChoice", "Select Completion Flag:",
                  choices = c("COMP8FL", "COMP16FL", "COMP24FL"))
    }
    else if (input$mainChoice == "Subject Status") {
      selectInput("subChoice", "Select Status:",
                  choices = c("DISCONFL", "DSRAEFL", "DTHFL"))
    }
    else if (input$mainChoice == "Discontinuation Reasons") {
      selectInput("subChoice", "View By:",
                  choices = c("Treatment", "Age Group"))
    }
  })
  
  output$mainPlot <- renderPlotly({
    req(input$subChoice)
    
    if (input$mainChoice == "Discontinuation Reasons") {
      plot_data <- adsl_data %>%
        group_by(.data[[ifelse(input$subChoice == "Treatment", "TRT01P", "AGEGR1")]], DCDECOD) %>%
        summarise(Count = n()) %>%
        ungroup()
      
      plot <- ggplot(plot_data, aes(x = DCDECOD, y = Count, fill = .data[[ifelse(input$subChoice == "Treatment", "TRT01P", "AGEGR1")]])) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Discontinuation Reasons", x = "Reason", y = "Count") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_manual(values = c("#984ea3", "#e7298a", "#8da0cb"))
      
      ggplotly(plot)
      
    } else {
      plot_data <- adsl_data
      plot <- switch(input$mainChoice,
                     "Distribution" = {
                       ggplot(plot_data, aes_string(x = input$subChoice)) + 
                         geom_histogram(binwidth = 5, fill = "lightblue", color = "black") + 
                         labs(title = paste("Distribution of", input$subChoice), x = input$subChoice, y = "Count")+
                         theme_minimal()
                     },
                     "Proportions" = {
                       pie_data <- plot_data %>% count(.data[[input$subChoice]]) %>% rename(Count = n)
                       plot_ly(pie_data, labels = ~.data[[input$subChoice]], values = ~Count, type = 'pie',
                               marker = list(colors = c("#ffff00", "#7570b3", "#ff00ff"))) %>%  
                         layout(title = paste(input$subChoice, "Distribution"))
                     },
                     "Subject Status" = {
                       pie_data <- plot_data %>% count(.data[[input$subChoice]]) %>% rename(Count = n)
                       plot_ly(pie_data, labels = ~.data[[input$subChoice]], values = ~Count, type = 'pie',
                               marker = list(colors = c("#7570b3", "#ff00ff"))) %>%  
                         layout(title = paste(input$subChoice, "Subject Status(Death/Discontinuation)"))
                     },
                     "Completion  Flag" = {
                       ggplot(plot_data, aes_string(x = input$subChoice, fill = "TRT01P")) +
                         geom_bar(position = "dodge") + 
                         labs(title = paste("Completion Flag:", input$subChoice)) +
                         scale_fill_manual(values = c("#984ea3", "#08519c", "#e78ac3")) +  
                         theme_minimal()
                     }
      )
      
      ggplotly(plot)
    }
  })
  
  output$statsTable <- renderTable({
    if (input$mainChoice == "Distribution") {
      selected_stats <- descriptive_stats %>%
        select(Parameters, input$subChoice)
      colnames(selected_stats)[2] <- "Value"  
      selected_stats
    }
  })
  
  # Vitals Server Logic
  filtered_data <- reactive({
    filtered_data <- adlb_data %>%
      filter(PARAM == input$param)
    
    if (input$agegrp != "None") {
      filtered_data <- filtered_data %>%
        filter(AGEGR1 == input$agegrp)
    }
    
    if (input$sex != "None") {
      filtered_data <- filtered_data %>%
        filter(SEX == input$sex)
    }
    
    filtered_data %>%
      group_by(TRTA, AVISITN) %>%
      summarise(
        mean_AVAL = mean(AVAL, na.rm = TRUE),
        sd_AVAL = sd(AVAL, na.rm = TRUE)
      ) %>%
      ungroup()
  })
  
  output$vital_plot <- renderPlot({
    plot_data <- filtered_data()
    
    ggplot(plot_data, aes(x = AVISITN, group = TRTA)) +
      scale_x_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22,24,26)) +
      geom_line(aes(y = mean_AVAL, color = TRTA), show.legend = input$overlap) +
      geom_point(aes(y = mean_AVAL, color = TRTA), show.legend = input$overlap) +
      geom_ribbon(aes(ymin = mean_AVAL - sd_AVAL, ymax = mean_AVAL + sd_AVAL, fill = TRTA), alpha = 0.2, show.legend = input$overlap) +
      labs(title = paste("Mean ± SD for", input$param, "by Treatment Arm (Age Group:", input$agegrp, ", Sex:", input$sex, ")"),
           x = "Visit Number (AVISITN)",
           y = "Parameter Value (AVAL)") +
      theme_solarized() +
      if (!input$overlap) facet_wrap(~TRTA)
  })
  
  #Boxplots
  output$boxplot <- renderPlot({
    # Filter data based on selected TRTA and PARAM
    selected_param <- input$param
    selected_trta <- input$trta
    
    # Filter data based on selections
    filtered_data <- data %>%
      filter(TRTA == selected_trta & PARAM == selected_param)
    
    #Creating a long-format data frame
    baseline_data <- data.frame(Timepoint = "AVAL_Baseline", Value = filtered_data$AVAL_Baseline)
    endtreatment_data <- data.frame(Timepoint = "AVAL_EndTreatment", Value = filtered_data$AVAL_EndTreatment)
    plot_data <- rbind(baseline_data, endtreatment_data)
    
    # Generate the boxplot for the selected PARAM
    ggplot(plot_data, aes(x = Timepoint, y = Value, fill = Timepoint)) +
      geom_boxplot(alpha = 0.7, position = position_dodge(0.8)) +
      labs(title = paste("Boxplot for", selected_param),
           x = "Timepoint", y = "Value") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

#Running the App
shinyApp(ui = ui, server = server)
  
