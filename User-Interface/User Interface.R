library(shinydashboard)
library(shinydashboardPlus)
library(shiny)
library(readxl)
library(dplyr)
library(lattice)
library(DT)
library(ggplot2)
library(caret)
library(xlsx)
library(rje)
library(openxlsx)
library(shinyjs)
library(tidyr)
library(openxlsx)
library(gmodels)
library(tidyr)
library(factoextra)
library(caret)
library(e1071)
library(SciViews)
library(ROCR)



#HEADER
header <- dashboardHeader(
    title = "Credit Scoring Model",
    # tags$li(class = "dropdown",
    #         tags$a(target="_blank", 
    #                tags$img(height = "20px", width = "20px", src=""))),
            
            
    dropdownMenu(messageItem(
        from = "Support",
        message = "Hit the button to get help",
        icon = icon("life-ring"),
        time = Sys.Date(),
        href = "")
    )
)



#SIDEBAR
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Loading the Dataset",
                 tabName = "loading_data",
                 icon = icon("table"),
                 badgeLabel = "begin",
                 badgeColor = "green"),
        menuItem("Predicted scores",
                 tabName = "scores",
                 icon = icon("list-alt"))
    )
)

#BODY
body <- dashboardBody(
    tabItems(
        # Loading data
        tabItem(tabName = "loading_data",
                fluidPage(
                    h1("Data Visualization"),
                    br(),
                    fileInput(inputId = "file1",label = NULL,
                              buttonLabel = "Browse...",
                              placeholder = "No file selected" 
                    ),
                    helpText(""),
                    p(""),
                    accept = c(".xlsx"),
                    dataTableOutput("value")
                ) 
        ),
        
        # Output
        tabItem(tabName = "scores",
                h1("Customers scoring"),
                br(),
                fluidRow(
                    useShinyjs(), #For onclick()
                    box(title = "Cut-off of the Probability of Default",
                        status = "primary",
                        background = "navy",
                        solidHeader = T,
                        sliderInput(inputId = "cutoff_proba",
                                    label = "",
                                    min = 0,
                                    max = 1, 
                                    value = 0.5)),
                    
                    box(title = "Cut-off of scores", 
                        status = "primary",
                        background = "navy",
                        solidHeader = T,
                        sliderInput(inputId = "cutoff_scores", 
                                    label = "",
                                    min = 0,
                                    max = 140, 
                                    value = 80)),
                    align = "center",
                    
                    column(width = 12,
                           splitLayout(cellWidths = c("30%", "30%"),
                                       actionButton(inputId = "scores_btn", 
                                                    "Get scores", 
                                                    class = "btn-primary btn-lg", 
                                                    icon = icon("fas fa-rocket")),
                                       
                                       actionButton(inputId = "highest_btn", 
                                                    "Get highest scores", 
                                                    class = "btn-primary btn-lg", 
                                                    icon = icon("fas fa-star")),
                                       
                                       actionButton(inputId = "lowest_btn",
                                                    "Get lowest scores",
                                                    class = "btn-primary btn-lg",
                                                    icon = icon("fas fa-trash-restore"))
                           ),
                           
                           #Vertical space
                           br()
                    ),
                    
                    box(title = "",
                        width = 12,
                        status = "primary",
                        background = NULL,
                        collapsible = TRUE,
                        solidHeader = F,
                        dataTableOutput("val"),
                        dataTableOutput("val_high"),
                        dataTableOutput("val_low")),
                       
                    
                    #Save button
                    actionButton(inputId = "save_btn",
                                 "Save results",
                                 class = "btn-primary btn-lg",
                                 icon = icon("fas fa-download"))
                    
                    
                )
        )
    )
) 

# Define UI for application that draws a histogram
ui <- dashboardPage(header = header, sidebar = sidebar, body = body
                    
                    
)


# Define server logic required to draw a histogram
server <- function(input, output) 
{
    output$value <- renderDataTable({
        if (is.null(input$file1)) 
        {
            return (NULL)
        }
        else
        {
            read_excel(input$file1$datapath)
        }
    })
    
    output$btnVal <- renderText(input$myAppButton)
    
    ntext <- eventReactive(input$scores_btn,{
        if (is.null(input$file1)|| is.null(input$cutoff_proba))
        {
            return (NULL)
        }
        else
        {
            #Loading the training set : 
            training_set_imported = import_training_set("C:\\Users\\benja\\OneDrive\\Documents\\PI2A5_Credit_scoring\\Credit_risk_analysis\\User-Interface\\Training_set.xlsx")
            logistic_reg = fit_model(training_set_imported)
            
            #Processing of the test set imported
            test_model = test_set.process(input$file1$datapath, 4)
            y_predict_logit_model = Model.y_predict_logit(test_model, logistic_reg)
            Score = Model.score_prime(y_predict_logit_model)
            
            #Classification of customers according to cutoff_scores
            classication.scores = Model.score_classified(Score, input$cutoff_scores)
            Customer = file_to_tab(input$file1$datapath)
            
            #Conversion from 0 & 1 to G & B
            Prediction = binary_to_letter(classication.scores)
            res = as.data.frame(cbind(Customer, Score, Prediction))
        }
    })
    
    output$val <- renderDataTable({
        ntext()
    })
    
    output$val_high <- renderDataTable({
        nhigh()
    })
    
    output$val_low <- renderDataTable({
        nlow()
    })
    
    onclick("save_btn",
            if (!is.null(input$file1))
            {
                filename = "C:\\Users\\benja\\OneDrive\\Documents\\PI2A5_Credit_scoring\\Credit_risk_analysis\\User-Interface\\Customers-Scoring.xlsx"
                
                list_dataframe = list("Scores" = ntext(), "Scorecard" = score_card())
            
                write.xlsx(list_dataframe, filename)
            }
            else
            {
                return (NULL)
            }
    )

    nhigh <- eventReactive(input$highest_btn,{
        if (!is.null(input$file1) && !is.null(ntext()))
        {
            #Dataframe
            txt = ntext()
            Customer = txt[, 1]
            
            txt2 = sapply(txt[, 2], as.numeric)
            Prediction = txt[, 3]
            
            #Sorting of scores
            Highest = sort(sapply(txt[, 2], as.numeric), decreasing = TRUE)
            
            #Frame with score sorted
            score_sorted_frame = cbind(Customer, Highest, Prediction)
            
            txt2 = cbind(txt[, 1], txt2, txt[, 3])
            
            #Frame with scores sorted & corresponding customers & prediction
            frame_res = order_other_column(score_sorted_frame, txt)
            
            return(frame_res)
        }
        else
        {
            return (NULL)
        }
    })
    
    nlow <- eventReactive(input$lowest_btn,{
        if (!is.null(input$file1) && !is.null(ntext()))
        {
            #Dataframe
            txt = ntext()
            Customer = txt[, 1]
            
            txt2 = sapply(txt[, 2], as.numeric)
            Prediction = txt[, 3]
            
            #Sorting of scores
            Lowest = sort(sapply(txt[, 2], as.numeric), decreasing = FALSE)
            
            #Frame with score sorted
            score_sorted_frame = cbind(Customer, Lowest, Prediction)
            
            txt2 = cbind(txt[, 1], txt2, txt[, 3])
            
            #Frame with scores sorted & corresponding customers & prediction
            frame_res = order_other_column(score_sorted_frame, txt)
            
            return(frame_res)
        }
        else
        {
            return (NULL)
        }
    })
    
}


# Run the application 
shinyApp(ui = ui, server = server)
