library(shiny)
library(shinydashboard)
library(ggplot2)
library(shinyr)
library(dplyr)
library(plotly)
library(htmlwidgets)
library(shinyWidgets)
library(corrplot)
library(DT)
library(GGally)
library(e1071)
library(factoextra)
library(FactoMineR)
library(wordcloud)
library(data.table)
library(nnet)
library(randomForest)
library(caret)
library(DMwR)
library(RColorBrewer)

shinyUI(
    fluidPage(
        dashboardPage(
            dashboardHeader(
                title = 'Predictor Dashboard'
            ),
            dashboardSidebar(
                sidebarMenu(
                    menuItem("Overview", tabName = "ov", icon = icon("dashboard")),
                    menuItem("Raw Data", tabName = "rd", icon = icon("database")),
                    menuItem("Exploratory Analysis", tabName = "ea", icon = icon("line-chart")),
                    menuItem("Predictive Analysis", tabName = "pa", icon = icon("th"))
                )
            ),
            dashboardBody(
              tags$head(tags$style(HTML(".small-box {height: 150px}"))),
                tabItems(
                    tabItem(tabName = "ov",
                         #   h2("Overview"),
                            br(),
                         #   box(width=NULL, title="Statistics", solidHeader = TRUE, status = "primary", 
                                infoBoxOutput("Box1"),
                                infoBoxOutput("Box2"),
                                infoBoxOutput("Box3"),
                                infoBoxOutput("Box4"),
                                infoBoxOutput("Box5"),
                                infoBoxOutput("Box6"),
                        #    ),
                            br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                            box(width=NULL, title="Rentention/Churn Rate, 2020", solidHeader = TRUE, collapsible = TRUE, status = "primary",
                                plotOutput("plot1", height = 250)
                            ),
                            # box(title="Statistics", collapsible = TRUE, status = "primary",
                            #     plotOutput("plot2")
                            # ),
                            # box(title="Statistics", collapsible = TRUE, status = "primary",
                            #     plotOutput("plot3")
                            # ),
                            h2("stuff", style = "visibility: hidden"),
                    ),
                    tabItem(tabName = "rd",
                            tabBox(
                              title = "", width = 12,
                              tabPanel("Data", 
                                       fluidPage(DT::dataTableOutput("filtered_data"),
                                                 downloadBttn("downloadFilteredData",
                                                              "Download Filtered Data",
                                                              size = "sm",
                                                              color = "royal",
                                                              style = "stretch",
                                                              block = T))),
                              tabPanel("Dimensions", 
                                       uiOutput("about_data"),
                                       downloadBttn("downloadAboutDataTable",
                                                    "Download",
                                                    size = "sm",
                                                    color = "royal",
                                                    style = "stretch",
                                                    block = T))
                            ),
                    ),
                    
                    tabItem(tabName = "ea",
                            tabBox(
                                title = "", width = 12,
                                tabPanel("1 Dimensional",
                                         box(uiOutput("width_of_tables"), width = 6),
                                         box(width = 6, uiOutput("xaxis_col_1d")),
                                         uiOutput("single_dimention_plot_ui")),
                                tabPanel("2 Dimensional",
                                         box(uiOutput("xaxis_col"), width = 4),
                                         box(uiOutput("yaxis_col"), width =4),
                                         box(uiOutput("color_plot_type"), width =4),
                                         uiOutput("plot_type"),
                                         plotlyOutput("plot_me")),
                                tabPanel("3 Dimensional",
                                         box(uiOutput("xaxis_col_3d"), width = 3),
                                         box(uiOutput("yaxis_col_3d"), width =3),
                                         box(uiOutput("zaxis_col_3d"), width =3),
                                         box(uiOutput("color_plot_type_3d"), width =3),
                                         uiOutput("plot_type_3d"), plotlyOutput("plot_me_3d"))
                            ),
                    ),
                    
                    tabItem(tabName = "pa",
                            h2("Under maintenance... coming soon...")
                            # tabBox(title = "Predictive Analysis", width = 12,
                            #        tabPanel("Supervised Learning",
                            #                 box(
                            #                   uiOutput("set_seed_regression"),
                            #                   uiOutput("train_data_set_size"),
                            #                   uiOutput("regression_predictor_cols"),
                            #                   uiOutput("regression_dependent_variable_col"),
                            #                   uiOutput("possible_models_to_train"),
                            #                   uiOutput("regression_build_action"),
                            #                   textOutput("regression_model_build_status_text"),
                            #                   uiOutput("regression_validate_method"),
                            #                   uiOutput("file_to_predict_regression"),
                            #                   width = 6),
                            #                 box(title = "Model Details" , 
                            #                     footer = "click on build to update this section",
                            #                     verbatimTextOutput("regression_model_summary"), 
                            #                     width = 6),
                            #                 fluidPage(
                            #                   tabBox(
                            #                     tabPanel("Training Data", 
                            #                              DT::dataTableOutput("regression_train_data"),
                            #                              downloadBttn("downloadTrainData", 
                            #                                           "Download Train Data", 
                            #                                           size = "sm", 
                            #                                           color = "royal", 
                            #                                           style = "stretch", 
                            #                                           block = T)),
                            #                     tabPanel("Test Data", 
                            #                              DT::dataTableOutput("regression_test_data"),
                            #                              downloadBttn("downloadTestData", 
                            #                                           "Download Test Data", 
                            #                                           size = "sm", 
                            #                                           color = "royal",
                            #                                           style = "stretch",
                            #                                           block = T)),
                            #                     tabPanel("The Model", uiOutput("model_details")),
                            #                     tabPanel("Model Validation", 
                            #                              DT::dataTableOutput("regression_validation_table"),
                            #                              downloadBttn("downloadPredictions", 
                            #                                           "Download Predictions", 
                            #                                           size = "sm", 
                            #                                           color = "royal",
                            #                                           style = "stretch", 
                            #                                           block = T)),
                            #                     tabPanel("Confusion Matrix", 
                            #                              uiOutput("conf")),
                            #                     tabPanel("Predictions v/s Actuals", 
                            #                              plotlyOutput("regression_validation_plot")),
                            #                     width = 12))
                            #        ),
                            #        tabPanel("Unsupervised Learning", 
                            #                 fluidPage(
                            #                   uiOutput("Cluster_analysis_inputs_ui"), 
                            #                   uiOutput('cluster_analysis_ui'),
                            #                   uiOutput("clustering_status")))
                            # ),
                    )
                )
            ), skin = "blue"
        )
    )
)
