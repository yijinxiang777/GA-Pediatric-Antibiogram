#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# install.packages("shiny")
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
#scales - format axis tick mark labels
library(scales)
#install.packages('readxl')
library("readxl")
# install.packages("DT")
library(DT)
options(shiny.sanitize.errors = TRUE)
######Load the dataset########################
raw_data<-as_tibble(read_xlsx('raw_data.xlsx',na ="N/A"))
raw_data$sensitive_case <- as.numeric(raw_data$sensitive_case)
raw_data$sensitive_prop <-round((raw_data$sensitive_case/raw_data$Isolates_case),2)
# raw_data$Year.f <-factor(raw_data$Year)
raw_data$Antibiotic <- ifelse(raw_data$Antibiotic == "Cefazolin (non- urine or NOS)", "Cefazolin (non-urine or NOS)",
                              ifelse(raw_data$Antibiotic == "Cefazolin (urine )","Cefazolin (urine)",
                                     ifelse(raw_data$Antibiotic =="NItrofurantoin","Nitrofurantoin",
                                            ifelse(raw_data$Antibiotic =="TMP-SMX","Tmp-Smx",
                                                   raw_data$Antibiotic))))
a<-raw_data %>% 
    filter(!is.na(sensitive_case)) %>%
    group_by(Antibiotic,Year,Bacterium) %>% 
    summarise(sensitive_case = sum(sensitive_case,na.rm = TRUE),
              Isolates_case = sum(Isolates_case,na.rm = TRUE),
              sensitive_prop =round(sensitive_case/Isolates_case,2),
              Hospital = "Overall") %>%
    ungroup()
raw_data <- rbind(raw_data,a)
raw_data$Hospital.f <- factor(raw_data$Hospital,levels = c("Augusta", "CHOA", "Columbus", "Navicent", "Savannah","Overall"))
# Define UI for application that draws a histogram
Bug <- levels(factor(raw_data$Bacterium))
    # levels(factor(raw_data$Bacterium))

ui <- fluidPage(
    # Application title
    titlePanel("Susceptibility to Antibiotics"),
   sidebarLayout(
        sidebarPanel(   
    #allow the user to choose from a prespecified set of options
    selectInput("Bug", "Which Bacterium?", Bug),
    uiOutput("Antibiotic")
                     ),
        # Show a plot of trend
        # mainPanel(plotOutput("linePlot"),
        #           verbatimTextOutput("summary1"))
    mainPanel(
        tabsetPanel(
            tabPanel("Plot",plotOutput("linePlot")),
            tabPanel("Summary Table",  dataTableOutput("mytable"))
                   )
             )
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    observeEvent(input$Bug, {
    output$Antibiotic <-renderUI({
        checkboxGroupInput("Antibiotic", "Which Antibiotic?",  
                           as.vector(unique(filter(raw_data,Bacterium == input$Bug)$Antibiotic)),
                           selected = as.vector(unique(filter(raw_data,Bacterium == input$Bug)$Antibiotic))[1])
    })
    })
    
    observe({
        d<- raw_data %>%
                filter(Bacterium == input$Bug,
                       Antibiotic %in% input$Antibiotic) 
        #Generate plot
        output$linePlot <- renderPlot({
            ggplot(d,aes(x=Year, y= sensitive_prop)) +
                geom_line(aes(color =Antibiotic ),size=1)+ 
                # geom_bar(stat="identity", position=position_dodge()) +
                facet_grid( Hospital.f ~.)+theme_bw()  +
                ylab("Proportion of Susceptibility") + scale_y_continuous(labels = percent,limits = c(0, 1))+
                theme(text=element_text(size=20, face="bold"), 
                      legend.text=element_text(size=16))
        }, height = 600)
        output$mytable <-renderDataTable({
            data <- d %>% filter(Hospital.f=="Overall") %>% 
                select(c("Year","Antibiotic","sensitive_prop")) %>%
                mutate(sensitive_prop = paste(round(100*sensitive_prop,2),"%",sep=""))%>%
                spread(Year, sensitive_prop) 
            datatable(data,rownames = FALSE,
                      caption = 'Summary for overall susceptibility')
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

