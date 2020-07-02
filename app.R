library(shiny)
library(shinydashboard)
library(DT)
library(stringr)
library(dplyr)
library(arules)

item <- readRDS("item.RDS")
rules <- read.csv("df_rules.csv")
retail_transaction <- readRDS("retail_transaction.RDS")
ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(),
    dashboardBody(
        
        selectInput(inputId = "lhs",
                    label = "Barang yang dibeli", 
                    choices = item, 
                    selected = "herb marker thyme", 
                    multiple = TRUE),
    DT::dataTableOutput("rules")
        )
        
        
    )


server <- function(input, output, session) {
  
  output$rules <- DT::renderDataTable({
    
    

    retail_rules <- apriori(data = retail_transaction,
                            parameter = list(supp = 0.01, conf = 0.7),
                            appearance = list(lhs = input$lhs),
                            control = list(verbose = F))
    temp <- retail_rules %>%
      DATAFRAME()

    temp %>% 
      mutate_at(vars(LHS, RHS), str_remove_all, "\\}") %>% 
      mutate_at(vars(LHS, RHS), str_remove_all, "\\{") %>% 
      mutate_at(vars(support, confidence, lift), round, 3) %>% 
      select(-coverage)

    
  })
  
  
  
  
}

shinyApp(ui, server)